intra_month <- function(markets = c("at","de","dk","fi","nl","no","se","uk","us"),
                        min_date = '2017-01-01',
                        max_date = '2017-12-31') {

  #' Intra month distribution main function
  #'
  #' DESCRIPTION HERE
  #' Process using daily_purchases data for historical distribution estimation.
  #' Update this table from time to time. No need to do it for each iteration.
  #'
  #'
  #' @param markets vector containing markets for which distribution is to be estimated.
  #' @param min_date Start date for month distribution estimate.
  #' @param max_date End date for month distribution estimate.
  #' @keywords intramonth distribution
  #' @export
  #' @examples
  #' EXAMPLE HERE


#Loading Libraries and sourcing R files-----------------------------------------
  require(tidyverse)
  require(Rglpk)
  require(sqldf)
  require(lubridate)
  require(dplyr)
  require(purrr)
  require(forecast)
  #source("normalise_by_month.R")
  #source("remove_rd_2yrsback.r")
  #source("holiday_scraper.r")
#Load Daily Volume--------------------------------------------------------------
  daily_purchases_act <- daily_purchases %>%
    mutate( Activation_date = as.Date(Activation_date)) %>%
    mutate( year = year(Activation_date)) %>%
    mutate( month = month(Activation_date)) %>%
    mutate( week = format(Activation_date, format="%U")) %>%
    mutate( day = wday(Activation_date)) %>%
    filter( Country_Code %in% markets) %>%
    arrange(Country_Code, Activation_date)
#Load forecasting period--------------------------------------------------------
  period_length <- as.Date(max_date) - as.Date(min_date) + 1
  period <- data.frame(forecast_period = as.Date(seq(as.Date(min_date),
                                                     by = "day", length.out = period_length)))

  market_list <- data.frame(country_code = markets)
  forecasting_period <- merge(market_list, period, all = TRUE) %>%
    mutate(forecast_period = as.Date(forecast_period)) %>%
    mutate(year = year(forecast_period)) %>%
    mutate(month = month(forecast_period)) %>%
    mutate(week = format(forecast_period, format = "%U")) %>%
    mutate(day = weekdays(forecast_period)) %>%
    mutate(volume = 0) %>%
    arrange(country_code, forecast_period)

#Remove and Quantify Red Day effects--------------------------------------------
  rd_effect <- data.frame(country = factor(),
                          year = integer(),
                          date = as.Date(character()),
                          holiday_name = character(),
                          effect=double(),
                          next_wd_effect = double(),
                          month = integer())
  l <- remove_rd_2yrsback(daily_purchases_act,
                          rd_effect,
                          markets,
                          period)
  daily_purchases <- l[[1]]
  rd_effect <- l[[2]]
  bf_effect <- l[[3]]
  xmas_effect <- l[[4]]
#Create Monthly Index for the data w/o Red days---------------------------------
  total_monthly <- daily_purchases %>%
    group_by(Country_Code, year, month) %>%
    summarise(monthly_vol = sum(Daily_Volume))

  daily_percentage <- merge(daily_purchases, total_monthly,
                            by=c("Country_Code", "month", "year"))

  monthly_percentage <- daily_percentage %>%
    mutate(normalised_daily = Daily_Volume/monthly_vol) %>%
    arrange(Country_Code, Activation_date)
#Loop through markets and match weeks_last_year with forecast_period------------
  for (i in markets){
    b <- monthly_percentage %>% #define b as same period for forecast period but last year
      filter(Country_Code == i ,
            Activation_date >= (min(period$forecast_period)-years(1) ),
            Activation_date <= (max(period$forecast_period)-years(1) ) )

    if ( (forecasting_period$year[1] - 1) %% 4 == 0 ){#Checking if last year was a leap year
      b <- b[-(1:2), ]
    }else{
      b <- b[-(1:1), ]
    }
    j <- min(which(forecasting_period$country_code == i ))#Find position of where country starts
    nye <- monthly_percentage[ monthly_percentage$Country_Code == i &
                               monthly_percentage$month == 12 &
                               day(monthly_percentage$Activation_date) == 31 &
                               monthly_percentage$year == (forecasting_period$year[1]-1) , ]
    b <- normalise_by_month(b, nye, period$forecast_period)
    forecasting_period$volume[seq(j, j - 1 + length(b[, 1]))] <- b$updated_normalised_daily
  } #Map 1st Monday last year to 1st Monday this year etc...
  forecasting_period[is.na(forecasting_period)] <- 0
  f <- forecasting_period
# forecasting_period<-f

#Add BF_effect -----------------------------------------------------------------
for(i in markets){
  j <- max(which(forecasting_period$day == 6 & #FRIDAY
                   forecasting_period$month == 11 &
                   forecasting_period$country_code == i ))#Finds row for BF
  if(is.finite(j)){
  forecasting_period$volume[(j - 4):(j + 7)] <- forecasting_period$volume[(j - 4):(j + 7)]*bf_effect[bf_effect$country == i, 2:13]
  }else{
    print("Black Friday not found")
    print(i)
  }
}
forecasting_period$volume <- unlist(forecasting_period$volume)

#Add Xmas_effect----------------------------------------------------------------
for(i in markets){
  j <- which(day(forecasting_period$forecast_period) == 24 &
                   forecasting_period$month == 12 &
                   forecasting_period$country_code == i)#Finds row for BF
  if(is.finite(j)){
  forecasting_period$volume[(j - 2):(j + 7)] <- forecasting_period$volume[(j - 2):(j + 7)]*xmas_effect[xmas_effect$country == i, 2:11]
  } else{
    print("Christmas not found")
    print(i)
  }
}
forecasting_period$volume <- unlist(forecasting_period$volume)

#Add Red Days-------------------------------------------------------------------
rd_effect[is.na(rd_effect)] <- 0
for(i in seq(1, length(rd_effect$Date))){
  if((rd_effect$effect[i] == 0) | (rd_effect$holiday_name[i] %in% c("New Year's Eve")) )
    {
    #do nothing
  }#If holidays have no recorded effect of NYE
  else {
  c <- forecasting_period$country_code == rd_effect$country[i] &
       forecasting_period$forecast_period == rd_effect$Date[i]

  forecasting_period$volume[c] <- forecasting_period$volume[c]*rd_effect$effect[i]

  next_wd <-min( which(forecasting_period$forecast_period > rd_effect$Date[i]
                       & forecasting_period$country_code == rd_effect$country[i]
                       & !(forecasting_period$forecast_period %in% rd_effect$Date)
                       & (forecasting_period$day %in% c(2, 3, 4, 5, 6) ) ) ) #Monday to Friday

  forecasting_period$volume[next_wd] <- forecasting_period$volume[next_wd]*rd_effect$next_wd_effect[i]
  } #Add the effect of Red Day
} #Looping through all holidays

#Normalise After adding the Red Days--------------------------------------------
m_total <- forecasting_period %>%
  group_by(country_code, year, month) %>%
  summarise(t_monthly = sum(volume))

forecasting_period <- left_join(forecasting_period, m_total,by = c("country_code" = "country_code", "month" = "month"))

forecasting_period <- forecasting_period %>%
  mutate(volume = volume/t_monthly) %>%
  select(country_code,
         forecast_period,
         year.x,
         month,
         week,
         day,
         volume)
#write_excel_csv(f, "X:/public/R folder/Intra Month Distribution/intramonth_distribution/indexes.csv")
#write_excel_csv(forecasting_period, "X:/public/R folder/Intra Month Distribution/intramonth_distribution/Monthly_indexes.csv")
#Return and Write---------------------------------------------------------------
return(forecasting_period)


}



