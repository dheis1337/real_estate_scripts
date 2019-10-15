library(data.table)
library(sf)
library(snakecase)
library(tidyverse)
library(lubridate)

# this script is the script run to update the sales and parcels data for douglas county

# load in doug parcels
doug_addr <- st_read("~/MyStuff/DataScience/RealEstate/douglas_county/shapefiles/Addresses_w_Accounts.shp", 
                     stringsAsFactors = FALSE)


# read in subdivision file
doug_sub <- st_read("~/MyStuff/DataScience/RealEstate/douglas_county/shapefiles/Subdivisions_General.shp",
                    stringsAsFactors = FALSE)


# transform the coord system for joining
doug_sub <- doug_sub %>% st_transform(2232)

# join the subdivisions into the parcels 
doug_addr <- doug_addr %>% st_join(doug_sub, left = FALSE)


# transform to WSG84
doug_addr <- doug_addr %>% st_transform(4326)

# change to data.table for cleaning
doug_addr <- as.data.table(doug_addr, na.strings = c("", NA))

# find all rows that have residential addressess
res <- grep("RESIDENTIAL", unique(doug_addr[ACCOUNT_SU == "Real"][, ADDRESS_US]), value = TRUE)

# select the properties you want
doug_addr <- doug_addr[ADDRESS_US %in% res]

# select desired columns
doug_addr <- doug_addr[, .(OBJECTID.x, ADDRESS_NU, UNIT_NO, BUILDING_N, 
                           STREET_PRE, STREET_NAM, STREET_TYP,  POSTAL_NAM, 
                           ZIP_CODE, "Douglas",  LOCATION_S,  PARCEL_SPN, LEGAL_DESC, BUSINESS_N, LAND_USE_Z, ADDRESS_US, 
                           ADDRESS_RE, ACCOUNT_NO, ACCOUNT_SU, TAX_DISTRI, ACCOUNT_TY, 
                           OWNER_NAME, MAILING_AD, MAILING__1, MAILING_CI,
                           MAILING_ST, MAILING_ZI, TOTAL_ACTU, TOTAL_ASSE, 
                           SUBDIVISIO.y, geometry)]


# change names
names(doug_addr) <- c("prop_id", "prop_addr_num",  "prop_unit_num", 
                      "prop_building_num", "prop_addr_pre", "prop_addr_st", 
                      "prop_addr_st_type", "prop_addr_city", "prop_addr_zip", "prop_addr_county",
                      "prop_addr_state", "parcel_spn", "legal_description", "business_name", "land_use",
                      "prop_use", "prop_rezoning", "account_num", "account_code", 
                      "tax_district", "account_type", "owner_name", 
                      "owner_mailing_addr", "owner_mailing_addr2", "owner_mailing_city", 
                      "owner_mailing_state", "owner_mailing_zip", "total_actual_value", 
                      "total_assessed_value", "neighborhood",
                      "geometry")





# Some columns  big camel case
doug_addr[, c("legal_description", "owner_name", "business_name", "land_use",
              "prop_use", "owner_mailing_addr", "owner_mailing_addr2", 
              "owner_mailing_city", "prop_addr_st", "prop_addr_st_type", "prop_addr_city",
              "neighborhood")  := lapply(.SD, to_any_case, case = "big_camel", sep_out = " "),
          .SDcols = c("legal_description", "owner_name", "business_name", "land_use",
                      "prop_use", "owner_mailing_addr", "owner_mailing_addr2", 
                      "owner_mailing_city", "prop_addr_st", "prop_addr_st_type", "prop_addr_city", 
                      "neighborhood")]




# chnage street types to map to how they're set up in the mls data
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Dr"), j = "prop_addr_st_type", value = "Drive")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Trl"), j = "prop_addr_st_type", value = "Trail")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Rd"), j = "prop_addr_st_type", value = "Road")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "St"), j = "prop_addr_st_type", value = "Street")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Cir"), j = "prop_addr_st_type", value = "Circle")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Ct"), j = "prop_addr_st_type", value = "Court")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Ln"), j = "prop_addr_st_type", value = "Lane")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Pl"), j = "prop_addr_st_type", value = "Place")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Pkwy"), j = "prop_addr_st_type", value = "Parkway")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Blvd"), j = "prop_addr_st_type", value = "Boulevard")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Ter"), j = "prop_addr_st_type", value = "Terrace")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Pt"), j = "prop_addr_st_type", value = "Point")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Hwy"), j = "prop_addr_st_type", value = "Highway")
set(doug_addr, i = which(doug_addr$prop_addr_st_type == "Ave"), j = "prop_addr_st_type", value = "Avenue")



# load in sales table
doug_sales1 <- fread("~/MyStuff/DataScience/RealEstate/douglas_county/Property_Sales_Data.csv",
                    stringsAsFactors = FALSE, na.strings = c("", NA))


# select desired columns 
doug_sales_final <- doug_sales[, .(ACCOUNT_NO, SALE_DATE, SALE_PRICE, DEED_TYPE, 
                                   GRANTOR, GRANTEE)]

# new names
names(doug_sales_final) <- c("account_num", "sale_date", "sale_price", "deed", 
                             "grantor", "grantee")


# make the grantee and grantor column big camel case
doug_sales_final[, c("grantee", "grantor")  := lapply(.SD, to_any_case, case = "big_camel", sep_out = " "),
                 .SDcols = c("grantee", "grantor")]


# set the keys for the data.table join
setkey(doug_addr, "account_num")
setkey(doug_sales_final, "account_num")

# inner join the two tables
# sales_join <- doug_addr[doug_sales_final, on = "account_num", allow.cartesian = TRUE]

sales_join <- doug_addr %>% left_join(doug_sales_final, by = c("account_num" = "account_num")) %>%
  as.data.table()

# change the geometry object to a character object
seal_coords <- as.character(sales_join$geometry)

# clean the character vectors so they're pairs without c() wrapping
seal_coords <- gsub("c\\(", "", seal_coords)
seal_coords <- gsub("\\)", "", seal_coords)

# split the pairs on ", " and return a matrix
seal_coords <- str_split(seal_coords, pattern = ", ", simplify = TRUE) %>%
  as.data.table() %>%
  setNames(c("longitude", "latitude"))

# bind the matrix and the sales_join data.table
sales_join <- cbind(sales_join, seal_coords)

# remove the geometry column
sales_join[, geometry := NULL]


# change sale_date column to date format
sales_join[, sale_date :=  mdy_hms(`sale_date`)]

# add a sale_year
sales_join[, sold_year := lubridate::year(`sale_date`)]

# add a month sold column
sales_join[, sold_month := lubridate::month(`sale_date`, label = TRUE, abbr = FALSE)]

# add a quarter sold column
sales_join[, sold_quarter := lubridate::quarter(sale_date)]

# remove entries where zip is na
sales_join <- sales_join[!is.na(prop_addr_zip) | !is.na(prop_addr_num)]

# make any entries that are blanks ("") NA
set(sales_join, i = which(sales_join$legal_description == "Null"), "legal_description", value = NA)

# clean the account_num column to remove "-" from any entry
sales_join[, account_num := gsub("-", "", account_num)] 

# add an "R" to any account_num that is 7 characters long
sales_join[nchar(account_num) == 7, account_num := paste("R", account_num, sep = "")]

# add an mls_number column
sales_join[, mls_number := NA]


# read in property improvements
doug_imp <- fread("~/MyStuff/DataScience/RealEstate/douglas_county/Property_Improvements_Data.csv",
                  stringsAsFactors = FALSE)

# subset the improvements dataset 
doug_imp <- doug_imp[PROPERTY_TYPE_CODE %in% c("Residential", "Townhouse", "Multiple Unit", "Condo",
                                   "Triplex", "Duplex")]


# subset the dataset to what is needed
doug_imp <- doug_imp[, .(ACCOUNT_NO, STYLE, BUILT_AS, PROPERTY_TYPE_CODE, UNIT_TYPE, TOTAL_GARAGE_SF, WALKOUT_BASEMENT_FLAG, 
             TOTAL_FINISHED_BASEMENT_SF, TOTAL_UNFINISHED_BASEMENT_SF,  EXTERIOR_CONSTRUCTION_TYPE,
             INTERIOR_FINISH_TYPE, ROOF_MATERIAL_CODE, NO_OF_STORY, NO_OF_BEDROOM, NO_OF_BATHROOM, 
             BUILT_YEAR, REMODELED_YEAR, NO_OF_UNIT, NA)]



# chagne the names of the columns
names(doug_imp) <- c("account_num", "prop_style", "built_as", "property_type", "unit_type",
                     "garage_sqft", "walkout_basement_flag", "finished_basement_sqft", 
                     "unfinished_basement_sqft", "exterior_material",
                     "interior_material", "roof_material", "stories", "total_beds", 
                     "total_baths", "year_built", "year_remodeled", "num_of_units", "taxes")



# set keys for joining
setkey(sales_join, "account_num")
setkey(doug_imp, "account_num")


sales_join <- sales_join[doug_imp]

sales_join <- sales_join[!is.na(prop_addr_zip)]


# find out unique zips
zip <- c(80104, 80108, 80109, 80116, 80118, 80124, 80125, 80126, 80129, 80130,
         80131, 80134, 80135, 80163)


# create a function to save the data based on zip code
save_function <- function(zips, dt) {
  save_dt <- dt %>% filter(prop_addr_zip == zips)
  
  save_file <- paste("~/MyStuff/DataScience/RealEstate/douglas_county/sold/", zips, 
                     ".csv", sep = "")
  
  fwrite(save_dt, file = save_file, row.names = FALSE)
  
}
 
# sapply over the zip vector 
sapply(zip, save_function, dt = sales_join)


# remove the unnecessary objects
rm(doug_addr, doug_sales, doug_sales_final, sales_join, seal_coords, doug_sub, res,
   zip, doug_imp)


