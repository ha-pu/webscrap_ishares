download_ishares <- function(etf_name, etf_url) {
  file_overview <- file.path(dir_price, str_c(etf_name, "_overview.tsv"))
  file_price <- file.path(dir_price, str_c(etf_name, "_price.tsv"))
  file_dividends <- file.path(dir_price, str_c(etf_name, "_dividends.tsv"))
  
  data_xml <- get_xml(etf_url)
  data_overview <- extract_overview(data_xml)
  data_price <- extract_historic(data_xml, file_price)
  data_dividends <- extract_dividends(data_xml, file_dividends)
  
  clean_overview(data_overview, file_overview)
  clean_price(data_price, file_price)
  clean_dividends(data_xml, data_dividends, file_dividends)
}
