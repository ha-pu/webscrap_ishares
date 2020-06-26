clean_price <- function(data_price, file_price) {
  data_price <- data_price %>%
    mutate(date = str_replace_all(date, "\u00C3\u00a4", "\u00e4")) %>%
    mutate(date = str_replace(date, "Jan", "J\u00e4n")) %>%
    mutate(date = as.Date(date, format = "%d.%b.%Y")) %>%
    mutate(price = as.numeric(price))
  
  if (file.exists(file_price)) {
    data_price <- bind_rows(data_price, read_tsv(file_price)) %>%
      unique() %>%
      group_by(date) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      arrange(desc(date))
  }
  
  write_tsv(data_price, file_price)
}
