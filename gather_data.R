library(tidyverse)
library(lubridate)
library(zoo)

wti_crude_spot <- read_csv("spot_crude.csv")
wti_crude_spot <- wti_crude_spot %>% 
        select(1:3) 

wti_crude_spot <- wti_crude_spot %>% 
        mutate("Date2" = as.Date(as.yearmon(Date, "%b-%Y"), frac = 1),
               "Month" = month(Date2),
               "Year" = year(Date2),
               "MoM" = (`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)` - lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`))/ lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`),
               "YoY" = (`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)` - lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, 12))/ lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, 12))

wti_crude_spot %>% 
        group_by(Year) %>% 
        summarize( "yr_mean" = mean(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`),
                   "yr_mode" = median(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`))
# wti_crude_spot$Date <- as_date(wti_crude_spot$Date, format = "%b-%Y", tz = "UTC")
