library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

# Quickly read in WTI crude oil from csv
wti_crude_spot <- read_csv("spot_crude.csv")
wti_crude_spot <- wti_crude_spot %>% 
        select(1:3) 

# Create a clean df with MoM and YoY for just Crude Oil
wti_crude_spot <- wti_crude_spot %>% 
        mutate("Date2" = as.Date(as.yearmon(Date, "%b-%Y"), frac = 1),
               "Month" = month(Date2),
               "Year" = year(Date2),
               "MoM" = (`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)` - lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`))/ lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`),
               "YoY" = (`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)` - lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, 12))/ lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, 12))

# Calculate yearly stats
year_stats <- wti_crude_spot %>% 
        group_by(Year) %>% 
        summarize( "yr_mean" = mean(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`),
                   "yr_median" = median(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`))
# Join to larger dataframe
wti_crude_spot <- left_join(wti_crude_spot, year_stats, on = c("Year" = "Year"))

# Read rest of data directly from xlsx file into tables
raw_data_path <- "raw_data_sheet.xlsx"
sheets <- raw_data_path %>%
        excel_sheets() %>% 
        set_names()

conv_gasoline <- read_excel(raw_data_path, sheet = sheets[3], skip = 2)
RBOB_gasoline <- read_excel(raw_data_path, sheet = sheets[4], skip = 2)
heating_oil <- read_excel(raw_data_path, sheet = sheets[5], skip = 2)
uls_diesel <- read_excel(raw_data_path, sheet = sheets[6], skip = 2)
jet <- read_excel(raw_data_path, sheet = sheets[7], skip = 2)
propane <- read_excel(raw_data_path, sheet = sheets[8], skip = 2)

# wti_crude_spot$Date <- as_date(wti_crude_spot$Date, format = "%b-%Y", tz = "UTC")
