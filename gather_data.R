library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

# Quickly read in WTI crude oil from csv
wti_crude_spot <- read_csv("DATA/spot_crude.csv")
wti_crude_spot <- wti_crude_spot %>% 
        select(1:3) 

# Create a clean df with MoM and YoY for just Crude Oil
wti_crude_spot <- wti_crude_spot %>% 
        mutate("Date2" = as.Date(as.yearmon(Date, "%b-%Y"), frac = 1),
               "Month" = month(Date2),
               "Year" = year(Date2),
               "MoM_crude_oil" = (`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)` - lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`))/ lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`),
               "YoY_crude_oil" = (`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)` - lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, 12))/ lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, 12))

# Calculate yearly stats
year_stats <- wti_crude_spot %>% 
        group_by(Year) %>% 
        summarize( "yr_mean_crude" = mean(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`),
                   "yr_median_crude" = median(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`))
# Join to larger dataframe
wti_crude_spot <- left_join(wti_crude_spot, year_stats, on = c("Year" = "Year"))

# Read rest of data directly from xlsx file into tables
raw_data_path <- "DATA/raw_data_sheet.xlsx"
sheets <- raw_data_path %>%
        excel_sheets() %>% 
        set_names()

conv_gasoline <- read_excel(raw_data_path, sheet = sheets[3], skip = 2, col_types = c("date", "numeric", "numeric")) %>% 
        mutate("Month" = month(Date), "Year" = year(Date))

RBOB_gasoline <- read_excel(raw_data_path, sheet = sheets[4], skip = 2, col_types = c("date", "numeric")) %>% 
        mutate("Month" = month(Date), "Year" = year(Date))

heating_oil <- read_excel(raw_data_path, sheet = sheets[5], skip = 2, col_types = c("date", "numeric")) %>% 
        mutate("Month" = month(Date), "Year" = year(Date))

uls_diesel <- read_excel(raw_data_path, sheet = sheets[6], skip = 2, col_types = c("date", "numeric", "numeric", "numeric")) %>% 
        mutate("Month" = month(Date), "Year" = year(Date))

jet <- read_excel(raw_data_path, sheet = sheets[7], skip = 2, col_types = c("date", "numeric")) %>% 
        mutate("Month" = month(Date), "Year" = year(Date))
propane <- read_excel(raw_data_path, sheet = sheets[8], skip = 2, col_types = c("date", "numeric")) %>% 
        mutate("Month" = month(Date), "Year" = year(Date))

# Join conv_gasoline and heating_oil
energy_df <- left_join(wti_crude_spot, conv_gasoline[,2:5], on = c("Year" = "Year", "Month" = "Month")) %>% 
        left_join(heating_oil[,2:4], on = c("Year" = "Year", "Month" = "Month")) %>%
        left_join(uls_diesel[-1], on = c("Year" = "Year", "Month" = "Month")) %>% 
        left_join(RBOB_gasoline[-1], on = c("Year" = "Year", "Month" = "Month")) %>% 
        left_join(jet[-1], on = c("Year" = "Year", "Month" = "Month")) %>% 
        left_join(propane[-1], on = c("Year" = "Year", "Month" = "Month"))

energy_df <- energy_df %>% select("Date"= `Date2`, c(5:6, 2:3, 7:length(energy_df)))

# Add US crude oil production
US_crude_prod <- read_excel("DATA/US_crude_prod.xls", sheet = sheets[2], skip = 2, col_types = c("date", "numeric")) %>% 
        mutate("Month" = month(Date), "Year" = year(Date))

energy_df <- left_join(energy_df, US_crude_prod[-1], by = c("Year" = "Year", "Month" = "Month"))


monthly_cpi <- read_csv("DATA/monthly_cpi.csv")
monthly_cpi$DATE <- as.Date(as.yearmon(monthly_cpi$DATE, "%b-%Y"), frac = 1)
monthly_cpi$CPI_nonadjusted <- monthly_cpi$CWUR0000SA0
monthly_cpi <- monthly_cpi[,c(1,3)]

energy_df <- left_join(energy_df, monthly_cpi, by = c("Date" = "DATE"))

Dow <- read_csv("DATA/DJI_monthly.csv")
Dow$Date <- as.Date(as.Date(as.yearmon(Dow$Date, "%b-%Y"), frac = 1))
names(Dow) <- c("Date", "Dow_Open", "Dow_High", "Dow_low", "Dow_Close", 
                "Dow_Adjusted_Close", "Dow_Volume")

energy_df <-  left_join(energy_df, Dow, by = c("Date" = "Date"))

