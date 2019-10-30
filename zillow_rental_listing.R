library(data.table)
library(sf)
library(snakecase)
library(tidyverse)
library(lubridate)


http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_2bedroom.csv


one_bed <- fread("http://files.zillowstatic.com/research/public/Zip/Zip_MedianRentalPricePerSqft_5BedroomOrMore.csv",
                 stringsAsFactors = FALSE)


read_func <- function(bed_seq) {
  link <- paste("http://files.zillowstatic.com/research/public/Zip/Zip_MedianRentalPricePerSqft_",
                bed_seq, "Bedroom.csv", sep = "")
  
  rent_dt <- fread(link, stringsAsFactors = FALSE)
  rent_dt[, beds := bed_seq]
  
}

rent_list <- lapply(1:3, read_func)


clean_func <- function(rent_list) {
  fin_dt <- rent_list[State == "CO"][, .SD, .SDcols = c(1, 2, ncol(rent_list) - 1, 
                                                        ncol(rent_list))]
}


lapply(rent_list, clean_func) %>%
  do.call(rbind, .)



one_bed <- one_bed[State == "CO"]


one_bed <- one_bed[State == "CO"][, .SD, .SDcols = c(2, ncol(one_bed))]

          

