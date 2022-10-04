library(magrittr)
library(tesseract)
library(magick)
library(stringr)
library(glue)
library(here)
install.packages("tesseract")
install.packages("magick")
input <- image_read(here("pre.png"))
fra <- tesseract("fra")
?tesseract_download
tesseract_download("fra")
# preprocess <- function(file){
#   input <- magick::image_read(file)
  text <- input %>%
    image_resize("500x") %>%
    image_convert(type = 'Grayscale') %>%
    image_trim(fuzz = 40) %>%
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr(engine = fra) 
  text <- str_replace(text, ' / ', '\n')
  text <- unlist(strsplit(text, "\\\n"))
  text <- str_to_lower(text)
}
text
preprocess(here("one.jpeg"))


get.info <- function(raw_text, param){
  
  if (param == "date"){
    res <- raw_text[grep(pattern = "Date", raw_text, ignore.case = T)]
    res <- sub(".*date : ", "", res) 
  }else if(param == "heure"){
    res <- raw_text[grep(pattern = "Heure", raw_text, ignore.case = T)]
    res <- sub(".*heure : ", "", res) 
  }else{
    res <- raw_text[grep(pattern = "Carte", raw_text, ignore.case = T)]
    res <- sub(".*carte bancaire nfc ", "", res) 
  }
  
  return(res)
}