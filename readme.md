iShares
================

# Table of contents

  - [1\. Basic idea](#1-basic-idea)
  - [2\. Download and extract data](#2-download-and-extract-data)
    - [2.a Get XML data](#2a-get-xml-data)
    - [2.b Extract sheet “Overview”](#2b-extract-sheet-overview)
    - [2.c Extract sheet “Historic”](#2c-extract-sheet-historic)
    - [2.d Extract sheet “Dividends”](#2d-extract-sheet-dividends)
    - [2.e Complete function for ishares download](#2f-complete-function-for-ishares-download)
  - [3\. Aggregate data and convert to Euro returns](#3-aggregate-data-and-convert-to-euro-returns)
    - [3.a Get exchange rates](#3a-get-exchange-rates)
    - [3.b Convert data to Euro](#3b-convert-data-to-euro-returns)
	- [3.c Aggregate data](#3c-aggregate-data)
  - [4\. Analyze historic ETF performance](#4-analyze-historic-etf-performance)
    - [4.a Compute trailing monthly returns](#4a-compute-trailing-monthly-returns)
    - [4.b Compute key metrics](#4b-compute-key-metrics)

# 1\. Basic idea

[Get to Top](#table-of-contents)

The basic idea of this script is download information on ETFs that are
part of the iShares family, published by Blackrock. Blackrock provides
basic information on for each ETF on the respective fund’s website. The
aim of this code is to:

  - Download and extract data for a number of ETFs
  - Aggregate data and convert to Euro returns
  - Analyze historic ETF performance

The code uses the following `R` packages:

``` r
library(knitr)
library(lubridate)
library(tidyverse)
library(xml2)
```

# 2\. Download and extract data

[Get to Top](#table-of-contents)

Blackrock shows detailed information on its various iShares ETFs on the
ETF’s website (e.g. [iShares Core € Corp Bond UCITS
ETF](https://www.ishares.com/de/privatanleger/de/produkte/251726/ishares-euro-corporate-bond-ucits-etf/)).
For each ETF, Blackrock provides an Excel file for download covering
fundamental information, historic prices, positions, and dividends. I
limit the analysis to [iShare’s most popular standard and ESG
ETFs](https://www.ishares.com/de/privatanleger/de/anlegen/bestseller). A
list of ETF names and URLs to the respective Excel file is the basis for
the analysis:

	#> # A tibble: 49 x 2
	#>    name                     url                                                 
	#>    <chr>                    <chr>                                               
	#>  1 iShares-Asia-Pacific     https://www.ishares.com/de/privatanleger/de/produkt~
	#>  2 iShares-Core-Corp-Bond   https://www.ishares.com/de/privatanleger/de/produkt~
	#>  3 iShares-Core-DAX         https://www.ishares.com/de/privatanleger/de/produkt~
	#>  4 iShares-Core-EURO-STOXX~ https://www.ishares.com/de/privatanleger/de/produkt~
	#>  5 iShares-Core-FTSE-100    https://www.ishares.com/de/privatanleger/de/produkt~

## 2.a Get XML data

[Get to Top](#table-of-contents)

The Excel files provided by iShares are not “real” Excel files but are
instead XML files created in Excel. The first step is therefore to
download the XML file from iShares, fix some encoding issues in the
first line for for the character *&*.

``` r
# get xml function =============================================================
get_xml <- function(etf_url) {
  file_raw <- tempfile()
  file_xml <- tempfile()
  download.file(etf_url, file_raw)
  txt <- readLines(file_raw, encoding = "UTF-8-BOM")
  txt[1] <- "<?xml version=\"1.0\"?>"
  txt <- str_replace_all(txt, "&", "")
  write_lines(txt, file_xml)
  out <- read_xml(file_xml)
  return(out)
}
```

The output from the `get_xml` function is an XML file with six nodes:

	#> {xml_document}
	#> <Workbook xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet">
	#> [1] <ss:Styles>\n  <ss:Style ss:ID="Default">\n    <ss:Alignment ss:Horizonta ...
	#> [2] <ss:Worksheet ss:Name="&#xC3;&#x153;berblick">\n  <ss:Table>\n    <ss:Row ...
	#> [3] <ss:Worksheet ss:Name="Positionen">\n  <ss:Table>\n    <ss:Row>\n      <s ...
	#> [4] <ss:Worksheet ss:Name="Historisch">\n  <ss:Table>\n    <ss:Row>\n      <s ...
	#> [5] <ss:Worksheet ss:Name="Wertentwicklung">\n  <ss:Table>\n    <ss:Row>\n    ...
	#> [6] <ss:Worksheet ss:Name="Aussch&#xC3;&#xBC;ttungen">\n  <ss:Table>\n    <ss ...

Depending on the file structure, the number of nodes can vary between
four and six. The functions below catch these differences.

## 2.b Extract sheet “Overview”

[Get to Top](#table-of-contents)

To extract data from the XML, I use the following function to find rows
and cell values in the structure:

``` r
# extract values function ======================================================
get_values <- function(data_xml, child) {
  ns <- xml_ns(data_xml)
  rows <- xml_find_all(xml_child(data_xml, child), ".//ss:Table/ss:Row", ns = ns)
  values <- map(rows, ~{
    .x %>%
      xml_find_all(".//ss:Cell/ss:Data", ns = ns) %>%
      xml_text() %>%
      unlist()
  })
  return(values)
}
```

The XML node \#2 contains the sheet “Overview”. The following function
loops through all rows and cells of the Excel sheet and extracts the
overview information.

``` r
# extract overview function ====================================================
extract_overview <- function(data_xml) {
  data_overview <- get_values(data_xml, 2)
  data_overview <- data_overview[-c(1:4)] %>% # rows 1:4 do not include important information
  map_dfr(~ { 
    tibble(
      name = etf_name,
      parameter = .x[[1]], 
      value = .x[[2]]
      )
  }) %>%
    mutate(parameter = str_replace_all(parameter, "\u00C3\u00a4", "\u00e4"))%>%
    mutate(parameter = str_replace_all(parameter, "\u00C3\u00b6", "\u00f6"))%>%
    mutate(parameter = str_replace_all(parameter, "\u00C3\u00bc", "\u00fc")) %>%
    mutate(parameter = str_remove_all(parameter, "\u00C3")) %>%
    mutate(value = str_replace_all(value, "\u00C3\u00a4", "\u00e4"))%>%
    mutate(value = str_replace_all(value, "\u00C3\u00b6", "\u00f6"))%>%
    mutate(value = str_replace_all(value, "\u00C3\u00bc", "\u00fc")) %>%
    mutate(value = str_remove_all(value, "\u00C3"))
  
  write_csv(data_overview, file_overview)
}
```

The output from the `extract_overview` function is a tibble with two
columns containing the basic ETF information:

	#> # A tibble: 36 x 3
	#>    name              parameter            value                                 
	#>    <chr>             <chr>                <chr>                                 
	#>  1 iShares-Asia-Pac~ Basiswährung         USD                                   
	#> ...
	#> 10 iShares-Asia-Pac~ Produktstruktur      Physisch                              
	#> # ... with 26 more rows

## 2.c Extract sheet “Historic”

[Get to Top](#table-of-contents)

The XML node \#4 contains the sheet “Historic”. The following function
loops through all rows cells of the Excel sheet and extracts information
on historic prices.

``` r
# extract historic function ====================================================
extract_historic <- function(data_xml, file_price){
  if (xml_length(data_xml) > 4) { # some files use a different data structure
    data_prices <- get_values(data_xml, 4)
  } else if (xml_length(data_xml) == 4) {
    data_prices <- get_values(data_xml, 3)
  }
  data_prices <- map_dfr(data_prices[-1], ~ { # row 1 contains the rownames
    tibble(
      name = etf_name,
      date = .x[[1]], 
      currency = .x[[2]], 
      price = .x[[3]]
      )
  }) %>%
    mutate(date = str_replace_all(date, "\u00C3\u00a4", "\u00e4")) %>%
    mutate(date = str_replace(date, "Jan", "J\u00e4n")) %>%
    mutate(date = as.Date(date, format = "%d.%b.%Y")) %>%
    mutate(price = as.numeric(price))
  
  write_csv(data_prices, file_prices)
}
```

The output from the `extract_historic` function is a tibble with three
columns containing historic ETF information (date, currency, price \~
NAV):

	#> # A tibble: 3,773 x 4
	#>    name                 date       currency price
	#>    <chr>                <date>     <chr>    <dbl>
	#>  1 iShares-Asia-Pacific 2021-02-10 USD       24.7
	#> ...
	#> 10 iShares-Asia-Pacific 2021-01-28 USD       23.9
	#> # ... with 3,763 more rows

## 2.d Extract sheet “Dividends”

[Get to Top](#table-of-contents)

The XML node \#6 (if included in the XML file) contains the sheet
“Dividends”. The following function loops through all rows and cells
of the Excel sheet and extracts information on dividends.

``` r
# extract dividends function ===================================================
extract_dividends <- function(data_xml, file_dividends){
  if (xml_length(data_xml) == 6) {
    data_dividends <- get_values(data_xml, 6)
    data_dividends <- map_dfr(data_dividends[-1], ~ { # row 1 contains the rownames
      tibble(
        name = etf_name,
        date = .x[[3]], 
        dividend = .x[[4]]
        )
    }) %>%
      mutate(date = str_replace_all(date, "\u00C3\u00a4", "\u00e4")) %>%
      mutate(date = str_replace(date, "Jan", "J\u00e4n")) %>%
      mutate(date = as.Date(date, format = "%d.%b.%Y")) %>%
      mutate(dividend = as.numeric(dividend)) %>%
      filter(!is.na(dividend) & dividend != 0)
  } else {
    data_dividends <- tibble(
      name = etf_name,
      date = Sys.Date(), 
      dividend = 0
      )
  }
  
  write_csv(data_dividend, file_dividend)
}
```

The output from the `extract_dividends` function is a tibble with two
columns containing ETF dividends (date, dividend):

	#> # A tibble: 58 x 3
	#>    name                 date       dividend
	#>    <chr>                <date>        <dbl>
	#>  1 iShares-Asia-Pacific 2020-12-23    0.258
	#> ...
	#> 10 iShares-Asia-Pacific 2018-09-26    0.388
	#> # ... with 48 more rows

## 2.e Complete function for ishares download

[Get to Top](#table-of-contents)

The complete function to download the ETF data from iShares:

``` r
# combined download function ===================================================
download_ishares <- function(etf_name, etf_url) {
  file_overview <- file.path(dir_price, str_c(etf_name, "_overview.csv"))
  file_price <- file.path(dir_price, str_c(etf_name, "_price.csv"))
  file_dividends <- file.path(dir_price, str_c(etf_name, "_dividends.csv"))
  
  data_xml <- get_xml(etf_url)
  data_overview <- extract_overview(data_xml)
  data_price <- extract_historic(data_xml, file_price)
  data_dividends <- extract_dividends(data_xml, file_dividends)
}
```

Map the `download_ishares` function to the list of ETF names and URLs
`[data_etf]`:

``` r
map2(data_etf$name, data_etf$url, download_ishares)
```

The output are three files for each ETF that contain cleaned overview
data, historic prices, and dividends:

  - `[etf_name]_overview.csv`
  - `[etf_name]_prices.csv`
  - `[etf_name]_dividends.csv`

<!-- end list -->

# 3\. Aggregate data and convert to Euro returns

[Get to Top](#table-of-contents)

The iShares ETFs use three different currencies: US Dollar, British
Pound, and Euro. Therefore, I convert prices and dividends to Euro in
order to compare the ETFs.

## 3.a Get exchange rates

[Get to Top](#table-of-contents)

For the comparison, I download exchange rates provided by the
[ECB](https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html).
The ECB offers a ZIP file containing various monthly Euro exchange rates
for download.

``` r
# downlad ECB data =============================================================
file_zip <- tempfile()
download.file("https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip", file_zip)
data_fx <- read_csv(unz(file_zip, "eurofxref-hist.csv")) %>%
  select(date = Date, usd_rate = USD, gbp_rate = GBP)
```

## 3.b Convert data to Euro

[Get to Top](#table-of-contents)

First, I convert all ETF prices and dividends denomminated in USD or
GDP  to Euro using the exchange rates downloaded from the ECB. I loop
through the list of ETF names and URLs `[data_etf]` and load the
individual ETF files extracted and saved in section II. The function
below loads the files, converts them, and overwrites non-Euro files. 

``` r
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
```

The result is Euro-denomminated prices and dividends:

	#> # A tibble: 3,773 x 4
	#>    name                 date       currency price
	#>    <chr>                <date>     <chr>    <dbl>
	#>  1 iShares-Asia-Pacific 2021-02-10 EUR       20.3
	#> ...
	#> 10 iShares-Asia-Pacific 2021-01-28 EUR       19.8
	#> # ... with 3,763 more rows

	#> # A tibble: 58 x 3
	#>    name                 date       dividend
	#>    <chr>                <date>        <dbl>
	#>  1 iShares-Asia-Pacific 2020-12-23    0.212
	#> ...
	#> 10 iShares-Asia-Pacific 2018-09-26    0.330
	#> # ... with 48 more rows

## 3.c Aggregate data

[Get to Top](#table-of-contents)

To account for dividends, I add the dividend payout to the ETF
price \~ net asset value, assuming that all dividend payout was
reinvested. Dividend payouts are reduced by a capital gains tax
rate of 27.5%.

``` r
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
```

The code above results in a table with five columns (name, isin, date,
currency, price) containing the aggregated data for all ETFs:

	#> # A tibble: 139,580 x 5
	#>    name                 date       currency price isin        
	#>    <chr>                <date>     <chr>    <dbl> <chr>       
	#>  1 iShares-Asia-Pacific 2021-02-10 EUR       20.3 IE00B14X4T88
	#> ...
	#> 10 iShares-Asia-Pacific 2021-01-28 EUR       19.8 IE00B14X4T88
	#> # ... with 139,570 more rows

# 4\. Analyze historic ETF performance

[Get to Top](#table-of-contents)

For a comparison of the ETFs, I analyze their historic performance,
using trailling monthly returns. I rely on the following key metrics:

  - Average returns
  - Variance of returns
  - Sharpe ratio
  - Share of months with positive returns

## 4.a Compute trailing monthly returns

[Get to Top](#table-of-contents)

The first step is to map the ETF price data to a list of all possible
dates and to categorize these dates into 28 groups. Each group is one of
28 possible monthly return series. Adding all 28 groups, gives a set of
trailing monthly returns for each ETF.

``` r
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
```

    ## # A tibble: 91,505 x 4
    ##    isin         name   date         return
    ##    <chr>        <chr>  <date>        <dbl>
    ##  1 DE0002635273 DivDAX 2005-05-02 -0.0127 
    ##  ... 
    ## 10 DE0002635273 DivDAX 2005-05-13 -0.00950
    ## # ... with 91,495 more rows

## 4.b Compute key metrics

[Get to Top](#table-of-contents)

**Average returns, variance, & Sharpe ratio**

``` r
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
```

| ISIN         | Name                                    | Return | Risk | Sharpe |
| :----------- | :-------------------------------------- | -----: | ---: | -----: |
| IE00B3F81409 | iShares-Core-Global-Aggregate-Bond      |   3.44 | 0.07 |  49.51 |
| IE00B4WXJJ64 | iShares-Core-Govt-Bond                  |   1.20 | 0.05 |  22.58 |
| IE00B5BMR087 | iShares-Core-SP-500                     |  13.85 | 0.73 |  18.87 |
| IE00B4L5Y983 | iShares-Core-MSCI-World                 |  11.19 | 0.66 |  16.98 |
| IE00B6R52259 | iShares-MSCI-ACWI                       |  10.91 | 0.66 |  16.59 |
| DE000A0F5UF5 | NASDAQ                                  |  12.99 | 0.90 |  14.45 |
| IE00B57X3V84 | iShares-Dow-Jones-Global-Sustainability |   8.77 | 0.68 |  12.86 |
| IE00B3F81R35 | iShares-Core-Corp-Bond                  |   0.56 | 0.05 |  10.90 |
| IE00B4L5YX21 | iShares-Core-MSCI-Japan                 |   8.20 | 0.78 |  10.58 |
| IE00B4ND3602 | iShares-Physical-Gold                   |   5.13 | 0.60 |   8.57 |

``` r
kpi %>%
  select(Return = returns, Risk = risk, Sharpe = sharpe) %>%
  ggplot() +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_abline(aes(intercept = 0, slope = max(kpi$sharpe)), colour = "darkgreen") +
  geom_point(aes(x = Risk, y = Return, colour = Sharpe))
```

![](plot_comparison.png)<!-- -->

**Share of months with positive returns**

``` r
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
```

| ISIN         | Name                                    | Share\_Positives | Weeks\_Positive | Weeks\_Total |
| :----------- | :-------------------------------------- | ---------------: | --------------: | -----------: |
| IE00B6R52259 | iShares-MSCI-ACWI                       |            63.38 |            1329 |         2097 |
| IE00B5BMR087 | iShares-Core-SP-500                     |            63.28 |            1551 |         2451 |
| IE00B4L5Y983 | iShares-Core-MSCI-World                 |            62.68 |            1646 |         2626 |
| DE0005933923 | MDAX                                    |            62.49 |            2935 |         4697 |
| DE000A0F5UF5 | NASDAQ                                  |            62.48 |            2155 |         3449 |
| IE00B3F81409 | iShares-Core-Global-Aggregate-Bond      |            62.01 |             364 |          587 |
| IE00B57X3V84 | iShares-Dow-Jones-Global-Sustainability |            61.38 |            1389 |         2263 |
| IE00B52MJY50 | iShares-Core-MSCI-Pacific-ex-Japan      |            60.72 |            1546 |         2546 |
| DE0005933972 | TecDax                                  |            59.70 |            2807 |         4702 |
| IE00BKM4GZ66 | iShares-Core-MSCI-EM                    |            59.60 |             866 |         1453 |

**Plot ETF returns**

``` r
ishares_data %>%
  filter(isin %in% c("IE00B3F81409", "IE00B4L5Y983") & year(date) >= 2018) %>%
  select(Name = name, Date = date, Price = price) %>%
  ggplot() +
  geom_line(aes(x = Date, y = Price, colour = Name)) +
  facet_wrap(~ Name, scales = "free_y") +
  guides(colour = FALSE) +
  labs(title = "Development of ETF prices", x = NULL)
```

![](plot_price.png)<!-- -->

``` r
data_returns %>%
  filter(isin %in% c("IE00B3F81409", "IE00B4L5Y983") & year(date) >= 2018) %>%
  select(Name = name, Date = date, Return = return) %>%
  ggplot() +
  geom_line(aes(x = Date, y = Return, colour = Name)) +
  geom_hline(aes(yintercept = 0), colour = "darkblue") +
  facet_wrap(~ Name) +
  guides(colour = FALSE) +
  labs(title = "Development of weekly returns", x = NULL)
```

![](plot_returns_dev.png)<!-- -->

``` r
data_returns %>%
  filter(isin %in% c("IE00B3F81409", "IE00B4L5Y983") & year(date) >= 2018) %>%
  select(Name = name, Return = return) %>%
  ggplot() +
  geom_boxplot(aes(x = Name, y = Return, fill = Name)) +
  geom_hline(aes(yintercept = 0), colour = "darkblue") +
  guides(fill = FALSE) +
  labs(title = "Distribution of weekly returns", x = NULL)
```

![](plot_returns_dist.png)<!-- -->

These results allow the selection of the best performing iShares ETF for
investment or can be used for further portfolio analysis.

[Get to Top](#table-of-contents)
