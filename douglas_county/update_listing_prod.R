library(data.table)
library(snakecase)
library(lubridate)
library(tidyverse)


# this script is used to update the listings files by removing them whenever
# the specific mls_number goes to under contract, is expired, or withdrawn. It is run every day and is 
# called by source() in the update_under_contract_prop.R file at the very end. 
# It uses the zips object from said script. First, the listing files that need 
# to be updated are read in, then a logical test is done to remove the rows of 
# the list_dt where the mls_number is also in the uc_dt (now that it is under contract).


# create a function to read in the listings file
read_listings <- function(zips) {
  # This function is called in the update_listings function to read in the 
  # necessary listing files
  
  # create the read path
  read_path <- paste("~/MyStuff/DataScience/RealEstate/douglas_county/for_sale/",
                     zips, ".csv", sep = "")
  
  # ensure file exists, then read the file if it does
  if (file.exists(read_path)) {
    fread(read_path, stringsAsFactors = FALSE)
  }
}


# create a  function to update the listing files by removing any properties 
# that are now under contract
update_listings <- function(zips, uc_dt, exp_dt, wd_dt, file_path) {
  
  # read in the listing files that need to be updated
  list_dt <- lapply(zips, read_listings) %>%
    do.call(rbind, .) %>%
    as.data.table()
  
  # get the mls_numbers from the uc_dt 
  uc_mls_nums <- unique(c(uc_dt$mls_number, exp_dt$mls_number, wd_dt$mls_number))
  
  # remove the rows in the list_dt where the mls_number is also in the
  # uc_mls_nums vector - thus meaning it's under contract
  list_dt <- list_dt[!(mls_number %in% uc_mls_nums)]
  
  # save the new listing file 
  save_function(zips, list_dt = list_dt, file_path = file_path)
}


# lapply the update_listings function
lapply(zips, update_listings, uc_dt = uc, 
       file_path = "~/MyStuff/DataScience/RealEstate/douglas_county/for_sale/")


