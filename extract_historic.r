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
