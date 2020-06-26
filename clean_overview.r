clean_overview <- function(data_overview, file_overview) {
  data_overview <- data_overview %>%
    mutate(parameter = str_replace_all(parameter, "\u00C3\u00a4", "\u00e4"))%>%
    mutate(parameter = str_replace_all(parameter, "\u00C3\u00b6", "\u00f6"))%>%
    mutate(parameter = str_replace_all(parameter, "\u00C3\u00bc", "\u00fc")) %>%
    mutate(parameter = str_remove_all(parameter, "\u00C3")) %>%
    mutate(value = str_replace_all(value, "\u00C3\u00a4", "\u00e4"))%>%
    mutate(value = str_replace_all(value, "\u00C3\u00b6", "\u00f6"))%>%
    mutate(value = str_replace_all(value, "\u00C3\u00bc", "\u00fc")) %>%
    mutate(value = str_remove_all(value, "\u00C3"))
  
  write_tsv(data_overview, file_overview)
}
