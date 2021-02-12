# packages =====================================================================
library(knitr)
library(lubridate)
library(tidyverse)
library(xml2)

# functions ====================================================================
source("get_xml.r")
source("extract_overview.r")
source("extract_historic.r")
source("extract_dividends.r")
source("download_ishares.r")

# parameters ===================================================================
dir_ishares <- "/path/to/folder"
dir_overview <- file.path(dir_ishares, "ishares_overview")
dir_price <- file.path(dir_ishares, "ishares_price")
dir_dividends <- file.path(dir_ishares, "ishares_dividends")

data_etf <- read_csv(file.path(dir_ishares, "data_ishares_url.csv"))

# get xml data =================================================================
data_xml <- get_xml(data_etf$url[1])

# extract and clean data =======================================================
map2(data_etf$name, data_etf$url, download_ishares)

# get exchange rate data =======================================================
file_zip <- tempfile()
download.file("https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip", file_zip)
data_fx <- read_csv(unz(file_zip, "eurofxref-hist.csv")) %>%
  select(date = Date, usd_rate = USD, gbp_rate = GBP)

# conversion function ==========================================================
convert_ishares <- function(etf_name) {
  # load files -----------------------------------------------------------------
  file_price <- file.path(dir_price, str_c(etf_name, "_price.csv"))
  file_dividends <- file.path(dir_dividends, str_c(etf_name, "_dividends.csv"))
  price <- read_csv(file_price, col_types = "cDcd")
  dividends <- read_csv(file_dividends, col_types = "cDd")
  
  # set dividends currency -----------------------------------------------------
  dividends$currency <- unique(price$currency)
  
  # convert price --------------------------------------------------------------
  price %>%
    left_join(data_fx, by = "date") %>%
    mutate(
      price = case_when(
        currency == "USD" ~ price / usd_rate,
        currency == "GBP" ~ price / gbp_rate,
        TRUE ~ price
      ),
      currency = "EUR"
    ) %>%
    select(
      -usd_rate, 
      -gbp_rate
    ) %>%
    write_csv(file_price)
  
  # convert dividends ----------------------------------------------------------
  dividends %>%
    left_join(data_fx, by = "date") %>%
    mutate(
      dividend = case_when(
        currency == "USD" ~ dividend / usd_rate,
        currency == "GBP" ~ dividend / gbp_rate,
        TRUE ~ dividend
      )
    ) %>%
    select(
      -currency,
      -usd_rate, 
      -gbp_rate
    ) %>%
    write_csv(file_dividends)
} 

# load etfs ====================================================================
ishares_url <- read_csv("ishares_url.csv")

# run_downloads ================================================================
walk(ishares_url$name, convert_ishares)

# aggregate prices and dividends ===============================================
ishares_data <- map_dfr(data_etf$name, ~{
  
  # load files -----------------------------------------------------------------
  overview <- read_csv(file.path(dir_overview, str_c(.x, "_overview.csv")))
  price <- read_csv(file.path(dir_price, str_c(.x, "_price.csv")))
  dividends <- read_csv(file.path(dir_dividends, str_c(.x, "_dividends.csv")))
  
  # add isin -------------------------------------------------------------------
  out <- price %>%
    mutate(isin = overview$value[overview$parameter == "ISIN"])
  
  # add dividends --------------------------------------------------------------
  out <- out %>%
    left_join(dividends, by = c("name", "date")) %>%
    filter(!is.na(price)) %>%
    arrange(desc(date)) %>%
    mutate(dividend = coalesce(dividend * 0.725, 0)) %>%
    mutate(dividend = cumsum(dividend)) %>%
    mutate(price = price + dividend) %>%
    select(-dividend) %>%
    filter(!is.na(date))
  
  return(out)
})

# trailing monthly returns ----
dates <- tibble(date = seq(from = min(ishares_data$date), to = max(ishares_data$date), by = 1))
dates$i <- rep(1:28, ceiling(nrow(dates) / 28))[seq(nrow(dates))]

data_returns <- map(unique(ishares_data$name), ~{
  xprices <- ishares_data %>%
    filter(name == .x) %>%
    right_join(dates, by = "date") %>%
    fill(name) %>%
    filter(!is.na(name))
  
  xreturns <- map(1:28, ~{
    out <- xprices %>%
      filter(i == .x) %>%
      mutate(start_px = lag(price)) %>%
      mutate(diff_px = price - start_px) %>%
      mutate(return = diff_px / start_px) %>%
      select(isin, name, date, return)
    return(out)
  }) %>%
    bind_rows() %>%
    arrange(date) %>%
    filter(!is.na(return))
  return(xreturns)
}) %>%   bind_rows()

# average returns, variance, & Sharpe ratio ----

kpi <- data_returns %>%
  group_by(isin, name) %>%
  summarise(risk = var(return),
            returns = mean(return)) %>%
  ungroup() %>%
  mutate(sharpe = (returns / risk) * sqrt(365 / 28)) %>%
  mutate(risk = risk * sqrt(365 / 28) * 100,
        returns = returns * (365 / 28) * 100)

kpi %>%
  arrange(desc(sharpe)) %>%
  select(ISIN = isin, Name = name, Return = returns, Risk = risk, Sharpe = sharpe) %>%
  head(10) %>%
  kable(digits = 2)

kpi %>%
  select(Return = returns, Risk = risk, Sharpe = sharpe) %>%
  ggplot() +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_abline(aes(intercept = 0, slope = max(kpi$sharpe)), colour = "darkgreen") +
  geom_point(aes(x = Risk, y = Return, colour = Sharpe))

# share of months with positive returns ----

data_returns %>%
  mutate(return = return > 0) %>%
  group_by(isin, name) %>%
  summarise(week_pos = sum(return),
            week_tot = n()) %>%
  mutate(share_pos = week_pos / week_tot * 100) %>%
  arrange(desc(share_pos)) %>%
  select(ISIN = isin, Name = name, Share_Positives = share_pos, Weeks_Positive = week_pos, Weeks_Total = week_tot) %>%
  head(10) %>%
  kable(digits = 2)
