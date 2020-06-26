get_xml <- function(etf_url) {
  file_raw <- tempfile()
  file_xml <- tempfile()
  download.file(etf_url, file_raw)
  txt <- readLines(file_raw, encoding = "UTF-8-BOM")
  txt[1] <- "<?xml version=\"1.0\"?>"
  txt <- str_replace_all(txt, "S&P", "SP")
  write_lines(txt, file_xml)
  out <- read_xml(file_xml)
  return(out)
}
