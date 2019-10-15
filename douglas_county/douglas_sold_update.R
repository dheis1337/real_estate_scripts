library(sf)
library(tidyverse)
library(lubridate)
library(data.table)

# this script takes in properties read in as sold on the mls in douglas county
# and then updates the sold data already saved

# read in the sold data exported from the mls
mls_sold <- fread("~/MyStuff/DataScience/RealEstate/mls_exports/mls_exports.csv", stringsAsFactors = FALSE)


mls_sold <- mls_sold[County == "Douglas"]

# chagne the Sold Date column to the correct format
mls_sold[, c("Sold Date") := 
           lapply(.SD, mdy_hms), .SDcols = c("Sold Date")]


# add sold quarter, year, and month columns
mls_sold[, sold_year := lubridate::year(`Sold Date`)]
mls_sold[, sold_month := lubridate::month(`Sold Date`, label = TRUE, abbr = FALSE)]
mls_sold[, sold_month := as.character(sold_month)]
mls_sold[, sold_quarter := quarter(`Sold Date`)]

# gather the necessary columns 
mls_sold <- mls_sold[, .(NA, `Street #`, `Unit #`, `Building #`, `Street Dir`, `Street Name`,
                         `Street Type`, City, `Zip Code`, County, State,
                        NA, NA, NA, NA, Type, NA, `Tax ID`,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  NA,  `Sold Date`, `Sold Price`,
                         NA, NA, NA, Longitude, Latitude, sold_year, sold_month,
                         sold_quarter, `MLS Number`, Style, NA, NA, NA, `Total Garage Spaces` * 350,
                        NA, (`Basement % Finished` / 100) * `SqFt Basement`,  `SqFt Basement` * (1 - (`Basement % Finished`) / 100),
                        `Exterior Materials`, NA, `Roofing Material`, `# of Stories in this Unit`,
                        `Total Bedrooms`, `Total Baths`, `Year Built`, NA,  `Unit Count`, Taxes)]



# add a row_number column which will be used for lapply at the end of the script
mls_sold[, row_number := 1:nrow(mls_sold)]




# change column names
names(mls_sold) <- c("prop_id", "prop_addr_num", "prop_unit_num", "prop_building_num",
                     "prop_addr_pre", "prop_addr_st", "prop_addr_st_type", "prop_addr_city", "prop_addr_zip", 
                      "prop_addr_county", "prop_addr_state",
                     "parcel_spn", "legal_description", "business_name",
                     "land_use", "prop_use", "prop_rezoning", "account_num", 
                     "account_code", "tax_district", "account_type", "owner_name",
                     "owner_mailing_addr", "owner_mailing_addr2", "owner_mailing_city",  "owner_mailing_state", "owner_mailing_zip",
                     "total_actual_value", "total_assessed_value", "neighborhood",
                     "sale_date", "sale_price", "deed", "grantor", "grantee",
                     "longitude", "latitude",  "sold_year", "sold_month", "sold_quarter", 
                     "mls_number", "prop_style", "built_as", "property_type", "unit_type",
                     "garage_sqft", "walkout_basement_flag", "finished_basement_sqft",
                     "unfinished_basement_sqft",  "exterior_material", 
                     "interior_material", "roof_material", "stories", "beds", "baths",
                     "year_built", "year_remodeled", "num_of_units",  "taxes", "row_number") 
                    


# ensure the character columns are clean
mls_sold[, c("prop_addr_st", "prop_addr_st_type", "prop_addr_city", "prop_addr_county", "sold_month") :=
   lapply(.SD, to_any_case, case = "big_camel", sep_out = " "), .SDcols = 
  c("prop_addr_st", "prop_addr_st_type", "prop_addr_city", "prop_addr_county", "sold_month")]


# change the zips so they're all 5 digits long
mls_sold[, prop_addr_zip := substr(prop_addr_zip, start = 1, stop = 5)]

# find the unique zip codes
zips <- mls_sold %>% select(prop_addr_zip) %>%
  pull() %>%
  unique()


# clean the account_num column to remove "-" from any entries
mls_sold[, account_num := gsub("-", "", account_num)]

# add an R to the 
mls_sold[nchar(account_num) == 7, account_num := paste("R", account_num, sep = "")]


# create function to read in files
read_function <- function(zips) {
  # create the file used for loading. paste the base of the file path to the 
  # correct zip code
  read_file <- paste("~/MyStuff/DataScience/RealEstate/douglas_county/sold/",
                     zips, ".csv", sep = "")
  
  # ensure the file exists, and then read it in
  if (file.exists(read_file)) {
  dt <- fread(read_file, stringsAsFactors = FALSE)  
   
  }
}

# lapply the read_function and than bind the elements together in one data.table
saved_sales <- lapply(zips, read_function) %>%
  do.call(rbind, .)

# change sale_date to posicxt
saved_sales[, sale_date := ymd_hms(sale_date)]

# remove the saved_sales data where the sale_date is NA, as this will prevent 
# the logic in the update_function from working correctly
saved_sales <- saved_sales[!is.na(sale_date)]

# 
# update_function <- function(mls_account, mls_dt, saved_dt) {
# 
#   
#   if (mls_account %in% saved_dt$account_num) {
#     if (mls_dt[account_num == mls_account][order(-sale_date)][1, sale_date] >
#         saved_dt[account_num == mls_account][order(-sale_date)][1, sale_date]) {
#       
#       # get the row of the mls dt that will be used to create update_dt
#       mls_load <- mls_dt[account_num == mls_account]
#       
#       # get the most recent info for the saved property
#       saved_prop <- saved_dt[account_num == mls_account][order(-sale_date)][1]
#       
#       # create the data.table that will be used to update
#       update_dt <- data.table(prop_id = saved_prop$prop_id, 
#                               prop_addr_num = saved_prop$prop_addr_num,
#                               prop_unit_num = saved_prop$prop_unit_num,
#                               prop_building_num = saved_prop$prop_building_num,
#                               prop_addr_pre = saved_prop$prop_addr_pre, 
#                               prop_addr_st = saved_prop$prop_addr_st, 
#                               prop_addr_st_type = saved_prop$prop_addr_st_type,
#                               prop_addr_city = saved_prop$prop_addr_city, 
#                               prop_addr_zip = saved_prop$prop_addr_zip,
#                               prop_addr_county = saved_prop$prop_addr_county,
#                               prop_addr_state = saved_prop$prop_addr_state,
#                               parcel_spn = saved_prop$parcel_spn, 
#                               legal_description = saved_prop$legal_description, 
#                               business_name = saved_prop$business_name, 
#                               land_use = saved_prop$land_use, 
#                               prop_use = saved_prop$prop_use,
#                               prop_rezoning = saved_prop$prop_rezoning, 
#                               account_num = saved_prop$account_num,
#                               account_code = saved_prop$account_code, 
#                               tax_district = saved_prop$tax_district,
#                               account_type = saved_prop$account_type, 
#                               owner_name = mls_sold$grantor,
#                               owner_mailing_addr = NA,
#                               owner_mailing_addr2 = NA,
#                               owner_mailing_city = NA,
#                               owner_mailing_state = NA, 
#                               owner_mailing_zip = NA,
#                               total_actual_value = NA,
#                               total_assessed_value = NA,
#                               neighborhood = saved_prop$neighborhood,
#                               sale_date = mls_load$sale_date,
#                               sale_price = mls_load$sale_price,
#                               deed = NA, 
#                               grantor = NA,
#                               grantee = NA, 
#                               longitude = saved_prop$longitude, 
#                               latitude = saved_prop$latitude,
#                               sold_year = mls_load$sold_year,
#                               sold_month = mls_load$sold_month, 
#                               sold_quarter = mls_load$sold_quarter) %>%
#         distinct(.keep_all = TRUE)
#       
#       
#     }
#   
# }  else if (mls_account %in% saved_dt$parcel_spn) {
#       if (mls_dt[account_num == mls_account][order(-sale_date)][1, sale_date] >
#           saved_dt[parcel_spn == mls_account][order(-sale_date)][1, sale_date]) {
#         
#         # get the row of the mls dt that will be used to create update_dt
#         mls_load <- mls_dt[account_num == mls_account]
#         
#         # get the most recent info for the saved property
#         saved_prop <- saved_dt[parcel_spn == mls_account][order(-sale_date)][1]
#         
#         # create the data.table that will be used to update
#         update_dt <- data.table(prop_id = saved_prop$prop_id, 
#                                 prop_addr_num = saved_prop$prop_addr_num,
#                                 prop_unit_num = saved_prop$prop_unit_num,
#                                 prop_building_num = saved_prop$prop_building_num,
#                                 prop_addr_pre = saved_prop$prop_addr_pre, 
#                                 prop_addr_st = saved_prop$prop_addr_st, 
#                                 prop_addr_st_type = saved_prop$prop_addr_st_type,
#                                 prop_addr_city = saved_prop$prop_addr_city, 
#                                 prop_addr_zip = saved_prop$prop_addr_zip,
#                                 prop_addr_county = saved_prop$prop_addr_county,
#                                 prop_addr_state = saved_prop$prop_addr_state,
#                                 parcel_spn = saved_prop$parcel_spn, 
#                                 legal_description = saved_prop$legal_description, 
#                                 business_name = saved_prop$business_name, 
#                                 land_use = saved_prop$land_use, 
#                                 prop_use = saved_prop$prop_use,
#                                 prop_rezoning = saved_prop$prop_rezoning, 
#                                 account_num = saved_prop$account_num,
#                                 account_code = saved_prop$account_code, 
#                                 tax_district = saved_prop$tax_district,
#                                 account_type = saved_prop$account_type, 
#                                 owner_name = mls_load$grantor,
#                                 owner_mailing_addr = NA,
#                                 owner_mailing_addr2 = NA,
#                                 owner_mailing_city = NA,
#                                 owner_mailing_state = NA, 
#                                 owner_mailing_zip = NA,
#                                 total_actual_value = NA,
#                                 total_assessed_value = NA,
#                                 neighborhood = saved_prop$neighborhood,
#                                 sale_date = mls_load$sale_date,
#                                 sale_price = mls_load$sale_price,
#                                 deed = NA, 
#                                 grantor = NA,
#                                 grantee = NA, 
#                                 longitude = saved_prop$longitude, 
#                                 latitude = saved_prop$latitude,
#                                 sold_year = mls_load$sold_year,
#                                 sold_month = mls_load$sold_month, 
#                                 sold_quarter = mls_load$sold_quarter)
#         
#         
#       } 
# } else {
#     
#     # subset to get the correct mls sale 
#     mls_load <<- mls_dt[account_num == mls_account][order(-sale_date)][1]
#     
#     # subset to get the correct saved data
#     saved_prop <<- saved_dt[prop_addr_num == mls_load$prop_addr_num &
#                              prop_addr_pre == mls_load$prop_addr_pre &
#                              prop_addr_st == mls_load$prop_addr_st &
#                              prop_addr_st_type == mls_load$prop_addr_st_type &
#                              prop_addr_city == mls_load$prop_addr_city &
#                              prop_addr_zip == mls_load$prop_addr_zip &
#                              prop_unit_num == mls_load$prop_unit_num][order(-sale_date)][1]
#     
#     if (is.na(saved_prop$sale_date)) {
#       
#       update_dt <- mls_load
#       
#     } else {
#     
#       if (mls_load$sale_date > saved_prop$sale_date) {
#       update_dt <- data.table(prop_id = saved_prop$prop_id, 
#                               prop_addr_num = saved_prop$prop_addr_num,
#                               prop_unit_num = saved_prop$prop_unit_num,
#                               prop_building_num = saved_prop$prop_building_num,
#                               prop_addr_pre = saved_prop$prop_addr_pre, 
#                               prop_addr_st = saved_prop$prop_addr_st, 
#                               prop_addr_st_type = saved_prop$prop_addr_st_type,
#                               prop_addr_city = saved_prop$prop_addr_city, 
#                               prop_addr_zip = saved_prop$prop_addr_zip,
#                               prop_addr_county = saved_prop$prop_addr_county,
#                               prop_addr_state = saved_prop$prop_addr_state,
#                               parcel_spn = saved_prop$parcel_spn, 
#                               legal_description = saved_prop$legal_description, 
#                               business_name = saved_prop$business_name, 
#                               land_use = saved_prop$land_use, 
#                               prop_use = saved_prop$prop_use,
#                               prop_rezoning = saved_prop$prop_rezoning, 
#                               account_num = saved_prop$account_num,
#                               account_code = saved_prop$account_code, 
#                               tax_district = saved_prop$tax_district,
#                               account_type = saved_prop$account_type, 
#                               owner_name = mls_load$grantor,
#                               owner_mailing_addr = NA,
#                               owner_mailing_addr2 = NA,
#                               owner_mailing_city = NA,
#                               owner_mailing_state = NA, 
#                               owner_mailing_zip = NA,
#                               total_actual_value = NA,
#                               total_assessed_value = NA,
#                               neighborhood = saved_prop$neighborhood,
#                               sale_date = mls_load$sale_date,
#                               sale_price = mls_load$sale_price,
#                               deed = NA, 
#                               grantor = NA,
#                               grantee = NA, 
#                               longitude = saved_prop$longitude, 
#                               latitude = saved_prop$latitude,
#                               sold_year = mls_load$sold_year,
#                               sold_month = mls_load$sold_month, 
#                               sold_quarter = mls_load$sold_quarter)
#    
#       } else {
#         update_dt <- saved_prop
#       }
#     }
#   } 
# } 
# 


# the update function goes through the mls_sold data.table row-by-row, 
# finds the property each row in the saved_sales data.table, then conducts 
# a logical test to ensure that any new information is saved in the update_dt
update_function <- function(row_nums, mls_dt, saved_dt) {
  # subset to get the correct mls sale 
  mls_load <- mls_dt[row_number == row_nums][order(-sale_date)][1]
  
  # subset to get the correct saved data
  saved_prop <<- saved_dt[prop_addr_num == mls_load$prop_addr_num &
                            prop_addr_pre == mls_load$prop_addr_pre &
                            prop_addr_st == mls_load$prop_addr_st &
                            prop_addr_st_type == mls_load$prop_addr_st_type &
                            prop_addr_city == mls_load$prop_addr_city &
                            prop_unit_num == mls_load$prop_unit_num][order(-sale_date)][1]
  if (is.na(saved_prop$sale_date)) {
    
    update_dt <- mls_load[, row_number := NULL]
    
  } else {
    
  
    if (mls_load$sale_date > saved_prop$sale_date) {
      update_dt <- data.table(prop_id = saved_prop$prop_id, 
                              prop_addr_num = saved_prop$prop_addr_num,
                              prop_unit_num = saved_prop$prop_unit_num,
                              prop_building_num = saved_prop$prop_building_num,
                              prop_addr_pre = saved_prop$prop_addr_pre, 
                              prop_addr_st = saved_prop$prop_addr_st, 
                              prop_addr_st_type = saved_prop$prop_addr_st_type,
                              prop_addr_city = saved_prop$prop_addr_city, 
                              prop_addr_zip = saved_prop$prop_addr_zip,
                              prop_addr_county = saved_prop$prop_addr_county,
                              prop_addr_state = saved_prop$prop_addr_state,
                              parcel_spn = saved_prop$parcel_spn, 
                              legal_description = saved_prop$legal_description, 
                              business_name = saved_prop$business_name, 
                              land_use = saved_prop$land_use, 
                              prop_use = saved_prop$prop_use,
                              prop_rezoning = saved_prop$prop_rezoning, 
                              account_num = saved_prop$account_num,
                              account_code = saved_prop$account_code, 
                              tax_district = saved_prop$tax_district,
                              account_type = saved_prop$account_type, 
                              owner_name = mls_load$grantor,
                              owner_mailing_addr = NA,
                              owner_mailing_addr2 = NA,
                              owner_mailing_city = NA,
                              owner_mailing_state = NA, 
                              owner_mailing_zip = NA,
                              total_actual_value = NA,
                              total_assessed_value = NA,
                              neighborhood = saved_prop$neighborhood,
                              sale_date = mls_load$sale_date,
                              sale_price = mls_load$sale_price,
                              deed = NA, 
                              grantor = NA,
                              grantee = NA, 
                              longitude = saved_prop$longitude, 
                              latitude = saved_prop$latitude,
                              sold_year = mls_load$sold_year,
                              sold_month = mls_load$sold_month, 
                              sold_quarter = mls_load$sold_quarter,
                              mls_number = mls_load$mls_number)
    }   
  }
}



# lapply the update function of the same numebrs that are in the row_number column
# in mls_sold column
new_dt <- lapply(1:nrow(mls_sold), update_function, mls_dt = mls_sold, saved_dt = saved_sales) %>%
  do.call(rbind, .)



# create a function to save the data
# create a function to save the data based on zip code
save_function <- function(zips, dt) {
  save_dt <- dt %>% filter(prop_addr_zip == zips)
  
  save_file <- paste("~/MyStuff/DataScience/RealEstate/douglas_county/sold/", zips, 
                     ".csv", sep = "")
  
  fwrite(save_dt, file = save_file, row.names = FALSE, append = TRUE, na = NA)
  
}


# pull the zips from the new_dt object
zips <- unique(new_dt[, prop_addr_zip])

# lapply the save_function over the zips vector to save the updated information 
# for each zip that had information to update
lapply(zips, save_function, dt = new_dt)


rm(saved_sales, mls_sold)



