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
