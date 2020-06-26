clean_dividends <- function(data_xml, data_dividends, file_dividends) {
  data_dividends <- data_dividends %>%
    mutate(date = str_replace_all(date, "\u00C3\u00a4", "\u00e4")) %>%
    mutate(date = str_replace(date, "Jan", "J\u00e4n")) %>%
    mutate(date = as.Date(date, format = "%d.%b.%Y")) %>%
    mutate(dividend = as.numeric(dividend)) %>%
    filter(!is.na(dividend) & dividend != 0)
  
  if (xml_length(data_xml) == 6 & file.exists(file_dividends)) {
    data_dividends <- bind_rows(data_dividends, old_dividends) %>%
      unique() %>%
      group_by(date) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      arrange(desc(date))
  }
  
  write_tsv(data_dividends, file_dividends)
}
