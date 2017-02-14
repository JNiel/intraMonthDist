remove_rd <- function(d_p, rd_effect){
  #'Remove red days; 2 years back in time.
  #'
  #'DESCRIPTION HERE
  #'
  #'@param d_p data frame with daily actuals data. Defaults to daily_purchases_act.
  #'@param rd_effect Matrix with historical red day effect.
  #'@keywords red_day holiday
  #'@export
  #'@examples
  #'EXAMPLE HERE

  last_year <- 2016
  holidays <- holiday_scraper()
  holidays$Date = as.Date(holidays$Date,"%b %d")
  year(holidays$Date) <- holidays$year
  levels(holidays$country) <-c("se","at","no","dk","fi","de","nl","uk","us")
  holidays <- holidays %>%
   mutate_if(is.factor,as.character)%>%
    arrange(country,Date)
     for (i in c("at","de","dk","fi","nl","no","se")){
#Defining and Choosing Red Days from Holiday Scraper----------------------------
       # i="se"
       if(i == "at"){
         red_days <-c("National holiday")
       }else if(i == "de"){
         red_days <-c("Silent Day","National holiday","Common Local holidays")
       }else if(i == "dk"){
         red_days <-c("National holiday")
       }else if(i == "fi"){
         red_days <-c("De facto holiday","National holiday")
       }else if(i == "nl"){
         red_days <-c("National holiday")
       }else if(i == "no"){
         red_days <-c("National holiday","National holiday, Flag day")
       }else if(i == "se"){
         red_days <-c("De facto holiday","Public holiday")
       }
      c_holidays <- holidays %>%
        filter(country == i,
               year == last_year,
               holiday_type %in% red_days)

      #RD_TEMPS NEEDS TO BE CREATED FROM HOLIDAYS
      rd_temp<-holidays %>%
        filter(country == i,
               year == last_year + 1,
               holiday_type %in% red_days)%>%
        select(country,year,Date,holiday_name)%>%
        mutate(effect = 0)%>%
        mutate(next_wd_effect=0)%>%
        mutate(month=month(Date))

      working_days <- d_p %>%
        filter(Country_Code == i,
               year == last_year,
               !(Activation_date %in% c_holidays$Date) )###MAKE SURE ONLY RED DAYS ARE REMOVED
      b <- d_p %>%
        filter(d_p$Country_Code == i,
               d_p$year == last_year)
#Loop through Holiday-----------------------------------------------------------
      for (k in seq(1,length(c_holidays$Date) ) ){
        # k<-1
        #Change volume to next same working day
        if(
          ( (weekdays(c_holidays$Date[k]) %in% c("Saturday","Sunday") ) |
          (c_holidays$holiday_name[k] %in% c("Christmas Eve","Christmas Day","Boxing Day","New Year's Eve") ) )
          )
        {
          if(weekdays(c_holidays$Date[k]) %in% c("Saturday","Sunday") ){

            #We want to make sure the holiday doesn't always fall on a weekend....checking 2 years is enough

          }
        }
        else{
            next_same_weekday <- min( which(
                             working_days$day == weekdays(c_holidays$Date[k])
                             & working_days$Activation_date > c_holidays$Date[k]
                             & working_days$Country_Code == i ) )
            #perhaps change to average between next_same_weekday and previous_same_weekday

            rd_temp$effect[k]<- b$Daily_Volume[b$Activation_date == c_holidays$Date[k] ] - working_days$Daily_Volume[next_same_weekday]

            b$Daily_Volume[b$Activation_date == c_holidays$Date[k] ] <- working_days$Daily_Volume[next_same_weekday]



            next_work_day <- min( which(working_days$Activation_date > c_holidays$Date[k]
                                        & working_days$Country_Code == i
                                        & (working_days$day %in% c("Monday","Tuesday",
                                          "Wednesday","Thursday","Friday") ) ) )
            next_work_day_sw <- min( which(working_days$Activation_date > working_days$Activation_date[next_work_day]
                                  & working_days$Country_Code == i
                                  & working_days$day == working_days$day[next_work_day] ) )

            rd_temp$next_wd_effect[k]<-b$Daily_Volume[b$Activation_date == working_days$Activation_date[next_work_day]] - working_days$Daily_Volume[next_work_day_sw]

            b$Daily_Volume[b$Activation_date == working_days$Activation_date[next_work_day]] <- working_days$Daily_Volume[next_work_day_sw]


        }

      }


      d_p[d_p$Country_Code == i & d_p$year == last_year, ] <- b


#Quantify Black Friday and Xmas effects
#Removing Black Friday Effect---------------------------------------------------
      b <- d_p[d_p$Country_Code == i & d_p$year == last_year, ]
      j <- max(which(b$day == "Friday" & b$month == 11 ))#Finds row for BF
      print(j)
      b$Daily_Volume[(j-4):(j+7)] = 0
      myts <- ts(b$Daily_Volume[(j+22):(j+8)],end =c(2016), frequency=7)
      fit <- stl(myts,s.window=7,s.degree=1)
      bf_effect <- b$Daily_Volume[(j+7):(j-4)] - forecast(fit,h=12)[[2]]
      b$Daily_Volume[(j+7):(j-4)] <- forecast(fit,h=12)[[2]]
      d_p[d_p$Country_Code == i & d_p$year == last_year, ] <- b

#Removing Christmas Effect------------------------------------------------------

      # remember to add xmas condition which Monday it peaks
      b <- d_p[d_p$Country_Code == i & d_p$year == last_year, ]
      j <- which(b$month == 12 & day(b$Activation_date) == 24 )#Finds row for xmas eve
      b$Daily_Volume[(j-2):(j+7)] = 0
      myts <- ts(b$Daily_Volume[(j-17):(j-3)],end =c(2016), frequency=7)
      fit <- stl(myts,s.window=7,s.degree=1)
      xmas_effect <-0
      b$Daily_Volume[(j-2):(j+7)] <- forecast(fit,h=10)[[2]]
      d_p[d_p$Country_Code == i & d_p$year == last_year, ] <- b

#Creating Indexes for RD effects-----------------------------------------------
      total_monthly <- b %>%
        group_by(month) %>%
        summarise(monthly_vol = sum(Daily_Volume))

      rd_temp <- left_join(rd_temp,total_monthly,by = "month")
      rd_temp <- rd_temp%>%
        mutate(effect = effect/monthly_vol)%>%
        mutate(next_wd_effect = next_wd_effect/monthly_vol)%>%
        select(country,year,Date,holiday_name,effect,next_wd_effect,month)

      bf_effect <- bf_effect/total_monthly$monthly_vol[11]

      rd_effect <- rbind(rd_effect,rd_temp)

     }
  l=list(d_p,rd_effect,bf_effect,xmas_effect)
  return(l)
}
