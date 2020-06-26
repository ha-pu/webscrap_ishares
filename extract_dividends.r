extract_dividends <- function(data_xml, file_dividends){
  if (xml_length(data_xml) == 6) {
    xml_dividends <- data_xml %>%
      xml_child(6) %>%
      xml_child(1)
    
    if (file.exists(file_dividends)) {
      old_dividends <- read_tsv(file_dividends)
      cnt_rows <- min(xml_length(xml_dividends), xml_length(xml_dividends) - nrow(old_dividends) + 5)
    } else {
      cnt_rows <- xml_length(xml_dividends)
    }
    
    out_cols <- vector(mode = "character", length = 2)
    out_rows <- vector(mode = "list", length = cnt_rows - 1)
    
    pb <- progress_bar$new(total = cnt_rows - 1, format = "[:bar] :percent")
    
    for (i in seq(2, cnt_rows)) {
      xml_row <- xml_dividends %>%
        xml_child(i)
      
      out_cols[[1]] <- xml_row %>%
        xml_child(1) %>%
        xml_text()
      
      out_cols[[2]] <- xml_row %>%
        xml_child(4) %>%
        xml_text()
      
      out_rows[[i - 1]] <- out_cols
      pb$tick()
    }
    
    out <- map_dfr(out_rows, ~ tibble(date = .x[1], dividend = .x[2]))
  } else {
    out <- tibble(date = NA, dividend = NA)
  }
  return(out)
}
