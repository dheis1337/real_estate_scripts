library(data.table)
library(snakecase)
library(lubridate)
library(tidyverse)


# this script is a cleaning script for all the active listings on the MLS 
listings <- fread("~/Downloads/Full Export (8).csv", stringsAsFactors = FALSE)


# subset the data.table 
listings <- listings[, .(`MLS Number`, `Last Change Timestamp (System)`, Type, `List Price`,
                          `Street #`,                       
                         `Street Dir`, `Street Name`, `Street Type`,`Unit #`, `Building #`,  City, State, 
                         `Zip Code`, County, `Tax ID`, `Total Baths`, `Total Bedrooms`, `SqFt Above`, 
                         `SqFt Finished`, `SqFt Total`, `SqFt Basement`, `SqFt Main`, 
                         `SqFt Upper`, `Total Garage Spaces`, `Total Carport Spaces`, 
                         `Interior Features`, Flooring, `Heating Type`, `Fuel Type`, Cooling,
                         `Exterior Materials`, `Roofing Material`, `Public Remarks`,`Broker Remarks`, `Unit Count`,
                         `Basement Size`, `Basement % Finished`, `Basement Finished`,
                         Style, `Style Characteristics`, `Subfloor / Foundation Type`, Architecture,
                         Construction, `Year Built`, `School District`, `High School`,
                         `Middle School`, `Total Annual HOA Fees`, Zoning, `Lot Size SqFt`, 
                         `Financial Terms`, Taxes, DOM, CDOM, `List Date`, `Original List Price`, 
                         `Listing Agent Name`, `Listing Agent Direct Work Phone`, `Listing Agent Email`)]

# change the column names
names(listings) <- c("mls_number", "last_change_timestamp", "mls_type", "list_price",
                     "prop_addr_num", "prop_addr_pre", "prop_addr_st", "prop_addr_st_type",
                     "prop_unit_num", "prop_building_num", "prop_addr_city", "prop_addr_state",
                     "prop_addr_zip", "prop_addr_county", "account_num", "total_baths",
                     "total_beds", "sqft_above", "sqft_finished", "sfqt_total", "sqft_basement",
                     "sqft_main", "sqft_upper", "garage_spaces", "carport_spaces",
                     "interior_features", "flooring", "heating_type", "fuel_type", 
                     "cooling", "exterior_materials", "roofing_material", "public_remarks",
                     "broker_remarks", "unit_count",  "basement_size", "basement_percent_finished",
                     "basement_finished_flag",  "style", "style_characteristics", 
                     "foundation", "architecture", "construction", "year_built", 
                     "school_district", "high_school", "middle_school", 
                     "total_annual_hoa", "zoning", "lot_size_sqft", "financial_terms",
                     "taxes", "dom", "cdom", "list_date", "original_list_price", 
                     "listing_agent_name", "listing_agent_phone", "listing_agent_email")



# ensure the character columns are clean
listings[, c("prop_addr_st", "prop_addr_st_type", "prop_addr_city", "prop_addr_county") :=
           lapply(.SD, to_any_case, case = "big_camel", sep_out = " "), .SDcols = 
           c("prop_addr_st", "prop_addr_st_type", "prop_addr_city", "prop_addr_county")]


# change date format for listing and then add a few columns
listings[, c("list_date", "last_change_timestamp") := 
           lapply(.SD, mdy_hms), .SDcols = c("list_date", "last_change_timestamp")]

# add sold quarter, year, and month columns
listings[, list_year := year(list_date)]
listings[, list_month := month(list_date, label = TRUE, abbr = FALSE)]
listings[, list_month := as.character(list_month)]
listings[, list_quarter := quarter(list_date)]


# create a function to save the data to file
save_function <- function(zips, list_dt) {
  save_file <- paste("~/MyStuff/DataScience/RealEstate/douglas_county/for_sale/", 
  zips, ".csv", sep = "")
  
  save_dt <- list_dt[prop_addr_zip == zips]
    
  fwrite(save_dt, save_file, row.names = FALSE)
}


# get the unique zips for the listing table
zips <- unique(listings[, prop_addr_zip])


# lapply to save the listings
lapply(zips, save_function, list_dt = listings)





