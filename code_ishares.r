# packages ----
library(knitr)
library(lubridate)
library(progress)
library(tidyverse)
library(xml2)

# functions ----
source("get_xml.r")
source("extract_overview.r")
source("extract_historic.r")
source("extract_dividends.r")
source("clean_overview.r")
source("clean_price.r")
source("clean_dividends.r")
source("download_ishares.r")

# parameters ----
dir_ishares <- "/path/to/folder"
dir_overview <- file.path(dir_ishares, "ishares_overview")
dir_price <- file.path(dir_ishares, "ishares_price")
dir_dividends <- file.path(dir_ishares, "ishares_dividends")

data_etf <- read_tsv(file.path(dir_ishares, "data_ishares_url.tsv"))

# get xml data ----
data_xml <- get_xml(data_etf$url[1])

# extract and clean data ----
map2(data_etf$name, data_etf$url, download_ishares)

# get exchange rate data ----
file_zip <- tempfile()
download.file("https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip", file_zip)
data_fx <- read_csv(unz(file_zip, "eurofxref-hist.csv")) %>%
  select(date = Date, usd_rate = USD, gbp_rate = GBP)

# aggregate data ----
ishares_data <- map(data_etf$name, ~{

  # load files
  overview <- read_tsv(file.path(dir_overview, str_c(.x, "_overview.tsv")))
  price <- read_tsv(file.path(dir_price, str_c(.x, "_price.tsv")))
  dividends <- read_tsv(file.path(dir_dividends, str_c(.x, "_dividends.tsv")))
  
  # get metadata
  name <- .x
  isin <- overview$value[overview$parameter == "ISIN"]
  
  # combine data
  out <- tibble(name, isin) %>%
    mutate(id = TRUE) %>%
    left_join(mutate(price, id = TRUE), by = "id") %>%
    select(-id)
  
  # dividends
  if(dim(dividends)[1] > 0) {
    out <- out %>%
      left_join(dividends, by = "date") %>%
      mutate(dividend = coalesce(dividend * 0.725, 0)) %>%
      mutate(dividend = cumsum(dividend))
  } else {
    out$dividend <- 0
  }
  
  out <- out %>%
    mutate(price = price + dividend) %>%
    select(-dividend)
  
  return(out)
}) %>% bind_rows() %>%
  filter(!is.na(date))
  
# convert to Euro returns ----
ishares_data <- ishares_data %>%
  left_join(data_fx, by = "date") %>%
  mutate(price = case_when(currency == "USD" ~ price / usd_rate,
                           currency == "GBP" ~ price / gbp_rate,
                           TRUE ~ price)) %>%
  select(-currency, -usd_rate, -gbp_rate)

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
