library(data.table)
library(snakecase)
library(lubridate)
library(tidyverse)


# this script is used to update the listings files by removing them whenever
# the specific mls_number goes to under contract. It is run every day and is 
# called by source() in the update_under_contract_prop.R file at the very end. 
# It uses the zips object from said script. First, the listing files that need 
# to be updated are read in, then a logical test is done to remove the rows of 
# the list_dt where the mls_number is also in the uc_dt (now that it is under contract).


# create a function to read in the uc file
read_uc <- function(zips) {
  # This function is called in the update_uc function to read in the 
  # necessary listing files
  
  # create the read path
  read_path <- paste("~/MyStuff/DataScience/RealEstate/douglas_county/under_contract/",
                     zips, ".csv", sep = "")
  
  # ensure file exists, then read the file if it does
  if (file.exists(read_path)) {
    fread(read_path, stringsAsFactors = FALSE)
  }
}


# create a  function to update the listing files by removing any properties 
# that are now under contract
update_uc <- function(zips, uc_dt, file_path) {
  
  # read in the listing files that need to be updated
  list_dt <- lapply(zips, read_uc) %>%
    do.call(rbind, .) %>%
    as.data.table()
  
  # get the mls_numbers from the uc_dt 
  uc_mls_nums <- unique(uc_dt$mls_number)
  
  # remove the rows in the list_dt where the mls_number is also in the
  # uc_mls_nums vector - thus meaning it's under contract
  list_dt <- list_dt[!(mls_number %in% uc_mls_nums)]
  
  # save the new listing file 
  save_function(zips, list_dt = list_dt, file_path = file_path)
}


# lapply the update_uc function
lapply(zips, update_uc, uc_dt = uc, file_path = "~/MyStuff/DataScience/RealEstate/douglas_county/for_sale/")


