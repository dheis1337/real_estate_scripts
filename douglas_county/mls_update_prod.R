library(data.table)
library(snakecase)
library(lubridate)
library(tidyverse)


# this script is a cleaning script for all the under contract listings on the MLS 
mls <- fread("~/Downloads/Full Export (14).csv", stringsAsFactors = FALSE)


# subset the data.table 
mls <- mls[, .(`MLS Number`, `Last Change Timestamp (System)`, Type, Status, `List Price`,
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
               `Listing Agent Name`, `Listing Agent Direct Work Phone`, `Listing Agent Email`,
               `Under Contract Date`, `Sold Date`, `Sold Price`, `Sold Term`, Latitude, Longitude)]

# change the column names
names(mls) <- c("mls_number", "last_change_timestamp", "mls_type", "mls_status", "list_price",
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
                "listing_agent_name", "listing_agent_phone", "listing_agent_email", 
                "under_contract_date", "sold_date", "sold_price", "sold_terms",
                "latitude", "longitude")



# ensure the character columns are clean
mls[, c("prop_addr_st", "prop_addr_st_type", "prop_addr_city", "prop_addr_county") :=
      lapply(.SD, to_any_case, case = "big_camel", sep_out = " "), .SDcols = 
      c("prop_addr_st", "prop_addr_st_type", "prop_addr_city", "prop_addr_county")]


# change date format for listing and then add a few columns
mls[, c("list_date", "last_change_timestamp", "under_contract_date", "sold_date") := 
      lapply(.SD, mdy_hms), .SDcols = c("list_date", "last_change_timestamp", 
                                        "under_contract_date", "sold_date")]


# add under contract quarter, year, and month columns
mls[, uc_year := year(under_contract_date)]
mls[, uc_month := month(under_contract_date, label = TRUE, abbr = FALSE)]
mls[, uc_month := as.character(uc_month)]
mls[, uc_quarter := quarter(under_contract_date)]

# add sold quarter, year, and month columns
mls[, sold_year := year(sold_date)]
mls[, sold_month := month(sold_date, label = TRUE, abbr = FALSE)]
mls[, sold_month := as.character(sold_month)]
mls[, sold_quarter := quarter(sold_date)]


# add list quarter, year, and month columns
mls[, list_year := year(list_date)]
mls[, list_month := month(list_date, label = TRUE, abbr = FALSE)]
mls[, list_month := as.character(list_month)]
mls[, list_quarter := quarter(list_date)]



# create a function to save the data to file
save_function <- function(zips, mls_dt, append_option = FALSE, 
                          file_path) {
  save_file <- paste(file_path, 
                     zips, ".csv", sep = "")
  
  save_dt <- mls_dt[prop_addr_zip == zips]
  
  fwrite(save_dt, save_file, row.names = FALSE, append = append_option)
}


# get the unique zips for the listing table
zips <- unique(mls[, prop_addr_zip])


# lapply to save the mls
lapply(zips, save_function, mls_dt = mls, append_option = TRUE,
       file_path = "~/MyStuff/DataScience/RealEstate/douglas_county/mls/")


