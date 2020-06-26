extract_historic <- function(data_xml, file_price){
  xml_price <- data_xml %>%
    xml_child(4) %>%
    xml_child(1)

  if (file.exists(file_price)) {
    old_price <- read_tsv(file_price)
    cnt_rows <- min(xml_length(xml_price), xml_length(xml_price) - nrow(old_price) + 5)
  } else {
    cnt_rows <- xml_length(xml_price)
  }
  
  out_cols <- vector(mode = "character", length = 3)
  out_rows <- vector(mode = "list", length = cnt_rows - 1)
  
  pb <- progress_bar$new(total = cnt_rows - 1, format = "[:bar] :percent")
  
  for (i in seq(2, cnt_rows)) {
    xml_row <- xml_price %>%
      xml_child(i)
    
    out_cols[[1]] <- xml_row %>%
      xml_child(1) %>%
      xml_text()
    
    out_cols[[2]] <- xml_row %>%
      xml_child(2) %>%
      xml_text()
    
    out_cols[[3]] <- xml_row %>%
      xml_child(3) %>%
      xml_text()
    
    out_rows[[i - 1]] <- out_cols
    pb$tick()
  }
  
  out <- map_dfr(out_rows, ~ tibble(date = .x[1], currency = .x[2], price = .x[3]))
  return(out)
}
