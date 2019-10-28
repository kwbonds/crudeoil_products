library(tidyverse)
library(lubridate)
library(zoo)

wti_crude_spot <- read_csv("spot_crude.csv")
wti_crude_spot <- wti_crude_spot %>% 
        select(1:3) 

wti_crude_spot <- wti_crude_spot %>% 
        mutate("Date2" = as.yearmon(Date, "%b-%Y"))

# wti_crude_spot$Date <- as_date(wti_crude_spot$Date, format = "%b-%Y", tz = "UTC")
