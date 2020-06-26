extract_overview <- function(data_xml) {
  xml_overview <- data_xml %>%
    xml_child(2) %>%
    xml_child(1)
  
  cnt_rows <- xml_length(xml_overview)
  
  out_cols <- vector(mode = "character", length = 2)
  out_rows <- vector(mode = "list", length = cnt_rows - 4)
  
  pb <- progress_bar$new(total = cnt_rows, format = "[:bar] :percent")
  
  for (i in seq(5, cnt_rows)) {
    xml_row <- xml_overview %>%
      xml_child(i)
    
    out_cols[[1]] <- xml_row %>%
      xml_child(1) %>%
      xml_text()
    
    out_cols[[2]] <- xml_row %>%
      xml_child(2) %>%
      xml_text()
    
    out_rows[[i - 4]] <- out_cols
    pb$tick()
  }
  
  out <- map_dfr(out_rows, ~ tibble(parameter = .x[1], value = .x[2]))
  return(out)
}
