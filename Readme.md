Crude Oil Modeling
================
Kevin Bonds
26 November, 2019

In an attempt to showcase my current understanding of various modeling techniques, the following analysis will be carried out and explained. This is a learning process for me and also will be an interative--meaning it may be incomplete to the viewer at any given moment. Nonetheless, it will be public in it's unfinished state in the hopes that others can instruct and/or benefit.

I'll attempt to show some basic data ingestion, data preperation, visualization, and predictive modeling techniques in the process. I will use the *R* programming language with RMarkdown for this document.

All code for this analysis can be found at: <https://github.com/kwbonds/crudeoil_products>. Feel free to clone/fork and comment to me at <kevin.w.bonds@gmail.com>.

The first thing to do is to load the libraries needed. I like to keep these collected at the top of any analysis rather that scattered throughout for future reference.

``` r
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(knitr)
library(ggplot2)
library(yardstick)
library(Metrics)
library(astsa)
```

Collecting data
===============

I'm going to start with some time series analysis using crude oil products. This data can be found as an xls file that can be downloaded from: <https://www.eia.gov/dnav/pet/PET_PRI_SPT_S1_M.htm>.

I'll load the data and do some quick formatting. Then I'll take a quick look and begin modeling the data and make predictions. Loading the individual Excel tabs in to tables and joining them into one big table.

``` r
# Read rest of data directly from xlsx file into tables
raw_data_path <- "DATA/raw_data_sheet.xlsx"
sheets <- raw_data_path %>%
        excel_sheets() %>% 
        set_names()

crude_oil <- read_excel(raw_data_path, sheet = sheets[2], skip = 2, col_types = c("date", "numeric", "numeric")) %>% 
        mutate("Date2" = as.Date(as.yearmon(Date, "%b-%Y"), frac = 1),
               "Month" = month(Date2),
               "Year" = year(Date2))
```

``` r
crude_oil <- crude_oil %>% 
        mutate("Date2" = as.Date(as.yearmon(Date, "%b-%Y"), frac = 1),
               "Month" = month(Date2),
               "Year" = year(Date2),
               "MoM_crude_oil" = 
                       (`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)` - 
                                lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`))/ 
                       lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`),
               "YoY_crude_oil" = 
                       (`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)` - 
                                lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, 12))/ 
                       lag(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, 12))
```

``` r
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

kable(crude_oil[12:17,], caption= "Table with MoM and YoY")
```

| Date       |  Cushing, OK WTI Spot Price FOB (Dollars per Barrel)|  Europe Brent Spot Price FOB (Dollars per Barrel)| Date2      |  Month|  Year|  MoM\_crude\_oil|  YoY\_crude\_oil|
|:-----------|----------------------------------------------------:|-------------------------------------------------:|:-----------|------:|-----:|----------------:|----------------:|
| 1986-12-15 |                                                16.11|                                                NA| 1986-12-31 |     12|  1986|        0.0584757|               NA|
| 1987-01-15 |                                                18.65|                                                NA| 1987-01-31 |      1|  1987|        0.1576660|       -0.1866550|
| 1987-02-15 |                                                17.75|                                                NA| 1987-02-28 |      2|  1987|       -0.0482574|        0.1481242|
| 1987-03-15 |                                                18.30|                                                NA| 1987-03-31 |      3|  1987|        0.0309859|        0.4512292|
| 1987-04-15 |                                                18.68|                                                NA| 1987-04-30 |      4|  1987|        0.0207650|        0.4548287|
| 1987-05-15 |                                                19.44|                                             18.58| 1987-05-31 |      5|  1987|        0.0406852|        0.2639792|

Since prices are taken at the end of the month, dates are converted to month end.

``` r
# Join conv_gasoline and heating_oil
energy_df <- 
        left_join(crude_oil, conv_gasoline[,2:5], on = c("Year" = "Year", "Month" = "Month")) %>% 
        left_join(heating_oil[,2:4], on = c("Year" = "Year", "Month" = "Month")) %>%
        left_join(uls_diesel[-1], on = c("Year" = "Year", "Month" = "Month")) %>% 
        left_join(RBOB_gasoline[-1], on = c("Year" = "Year", "Month" = "Month")) %>% 
        left_join(jet[-1], on = c("Year" = "Year", "Month" = "Month")) %>% 
        left_join(propane[-1], on = c("Year" = "Year", "Month" = "Month"))

energy_df <- energy_df %>% select("Date"= `Date2`, c(5:6, 2:3, 7:length(energy_df)))
kable(head(energy_df))
```

| Date       |  Month|  Year|  Cushing, OK WTI Spot Price FOB (Dollars per Barrel)|  Europe Brent Spot Price FOB (Dollars per Barrel)|  MoM\_crude\_oil|  YoY\_crude\_oil|  New York Harbor Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon)|  U.S. Gulf Coast Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon)|  New York Harbor No. 2 Heating Oil Spot Price FOB (Dollars per Gallon)|  New York Harbor Ultra-Low Sulfur No 2 Diesel Spot Price (Dollars per Gallon)|  U.S. Gulf Coast Ultra-Low Sulfur No 2 Diesel Spot Price (Dollars per Gallon)|  Los Angeles, CA Ultra-Low Sulfur CARB Diesel Spot Price (Dollars per Gallon)|  Los Angeles Reformulated RBOB Regular Gasoline Spot Price (Dollars per Gallon)|  U.S. Gulf Coast Kerosene-Type Jet Fuel Spot Price FOB (Dollars per Gallon)|  Mont Belvieu, TX Propane Spot Price FOB (Dollars per Gallon)|
|:-----------|------:|-----:|----------------------------------------------------:|-------------------------------------------------:|----------------:|----------------:|----------------------------------------------------------------------------------:|----------------------------------------------------------------------------------:|----------------------------------------------------------------------:|-----------------------------------------------------------------------------:|-----------------------------------------------------------------------------:|-----------------------------------------------------------------------------:|-------------------------------------------------------------------------------:|---------------------------------------------------------------------------:|-------------------------------------------------------------:|
| 1986-01-31 |      1|  1986|                                                22.93|                                                NA|               NA|               NA|                                                                                 NA|                                                                                 NA|                                                                     NA|                                                                            NA|                                                                            NA|                                                                            NA|                                                                              NA|                                                                          NA|                                                            NA|
| 1986-02-28 |      2|  1986|                                                15.46|                                                NA|       -0.3257741|               NA|                                                                                 NA|                                                                                 NA|                                                                     NA|                                                                            NA|                                                                            NA|                                                                            NA|                                                                              NA|                                                                          NA|                                                            NA|
| 1986-03-31 |      3|  1986|                                                12.61|                                                NA|       -0.1843467|               NA|                                                                                 NA|                                                                                 NA|                                                                     NA|                                                                            NA|                                                                            NA|                                                                            NA|                                                                              NA|                                                                          NA|                                                            NA|
| 1986-04-30 |      4|  1986|                                                12.84|                                                NA|        0.0182395|               NA|                                                                                 NA|                                                                                 NA|                                                                     NA|                                                                            NA|                                                                            NA|                                                                            NA|                                                                              NA|                                                                          NA|                                                            NA|
| 1986-05-31 |      5|  1986|                                                15.38|                                                NA|        0.1978193|               NA|                                                                                 NA|                                                                                 NA|                                                                     NA|                                                                            NA|                                                                            NA|                                                                            NA|                                                                              NA|                                                                          NA|                                                            NA|
| 1986-06-30 |      6|  1986|                                                13.43|                                                NA|       -0.1267880|               NA|                                                                               0.42|                                                                              0.409|                                                                   0.38|                                                                            NA|                                                                            NA|                                                                            NA|                                                                              NA|                                                                          NA|                                                            NA|

Modeling crude oil
==================

To create a time series model for crude oil price we should determine what sort of model may best fit. Looking at the plot of the data:

``` r
ggplot(energy_df, aes(x = energy_df$Date, y = energy_df$`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`)) + geom_line() + ylab("WTI Spot Price (Dollars per Barrel)") + xlab("Date") + ggtitle("Monthly average for West Texas Crude Oil")
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

It appears the data is not stabalized. There is a general trend and maybe some exponential growth. Let's try standardizing the data by log-diffenecing to remove trend and growth.

``` r
cop <-  ts(energy_df$`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, start= c(1986,1), end = c(2019,8), frequency = 12)
```

``` r
crude_oil_returns <- log(cop)
plot(crude_oil_returns, type = "l")
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
plot(diff(crude_oil_returns), type = "l")
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

This is looking pretty stabilized. So this suggests that an integrated model is appropriate (d = 1). So let's check the ACF and PACF of the logged data to see if we can determine if an Auto-regressive model, Moving Average model or a combined model is best.

``` r
acf2(crude_oil_returns)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

The above suggests a ARIMA(1,1,0) model because the acf is tailing off and the PACF cuts at lag 1 (suggesting ar = 1). I'll use the sarima package to create the model and to forcast it. sarima has some nice tools for this.

``` r
ar_sim_x <- sarima(crude_oil_returns, p = 1, d = 1, q = 0)
```

![](README_files/figure-markdown_github/model_1-1.png)

``` r
ar_sim_x
```

Let's try adding a parameter and see if that improves things? We are looking for the Akaike Information Criteron (AIC) and the Bayesian Information Criterion (BIC) to judge the strength of the model.

``` r
ar_sim_x_2 <- sarima(crude_oil_returns, p = 2, d = 1, q = 0)
```

![](README_files/figure-markdown_github/model_2-1.png)

``` r
ar_sim_x_2
```

That does not. We can see that the added parameter is not statistically significant and the BIC and AIC both go down.

Now let's see if adding seasonality to the model will improve it. Looking at the ACF/PCF for the differenced data.

``` r
acf2(diff(diff(crude_oil_returns), 48))
```

![](README_files/figure-markdown_github/seasonal_acf-1.png)

From the ACF/PACF it seems that the ACF may trail off at each log (12 months) and the PCF cuts off. But the signal is small if any. We can try to add a seasonal AR and see what happens.

``` r
ar_sim_x_3 <- sarima(crude_oil_returns, p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12)
```

![](README_files/figure-markdown_github/model_3-1.png)

``` r
ar_sim_x_3
```

After trying a few it doesn't see that the model improves much

Now that we are satisfied with the non-sesonal ARIMA(1,1,0), let's forecast 6 months ahead. We'll use the sarima package

``` r
oil_for <- sarima.for(cop, n.ahead = 6, 1,1,0)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
oil_for$pred
```

    ##           Jan      Feb Mar Apr May Jun Jul Aug      Sep      Oct      Nov
    ## 2019                                           53.90442 53.60729 53.53667
    ## 2020 53.59547 53.65225                                                   
    ##           Dec
    ## 2019 53.55037
    ## 2020

Gas Prices
==========

``` r
gas_price <- ts(energy_df$`New York Harbor Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon)`, start= c(1986,1), end = c(2019,8), frequency = 12)
```

``` r
plot(diff(gas_price), type = "l")
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
gas_returns <- log(gas_price)
plot(gas_returns, type = "l")
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
plot(diff(gas_returns), type = "l")
```

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
acf2(gas_returns)
```

![](README_files/figure-markdown_github/acf2_gas_prices-1.png)

``` r
gas_mdl <- sarima(gas_returns, p = 2, d = 1, q = 0)
```

![](README_files/figure-markdown_github/gas_mdl_1-1.png)

``` r
gas_mdl
```

``` r
gas_mdl <- sarima(gas_returns, p = 1, d = 1, q = 2)
```

![](README_files/figure-markdown_github/gas_mdl_2-1.png)

``` r
gas_mdl
```

``` r
sarima.for(gas_price, 1,1,2, n.ahead = 6)
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

    ## $pred
    ##           Jan      Feb Mar Apr May Jun Jul Aug      Sep      Oct      Nov
    ## 2019                                           1.614915 1.637612 1.656848
    ## 2020 1.687288 1.699412                                                   
    ##           Dec
    ## 2019 1.673238
    ## 2020         
    ## 
    ## $se
    ##            Jan       Feb Mar Apr May Jun Jul Aug       Sep       Oct
    ## 2019                                             0.1279105 0.2074903
    ## 2020 0.3180102 0.3403182                                            
    ##            Nov       Dec
    ## 2019 0.2557773 0.2907294
    ## 2020
