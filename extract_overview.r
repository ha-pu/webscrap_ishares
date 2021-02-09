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
