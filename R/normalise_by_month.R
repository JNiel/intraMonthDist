normalise_by_month <- function(b, nye, f_period){
  #' Normalisation function used in the intraMonthDist package.
  #'
  #' Further information...
  #' @param b Data frame
  #' @param nye Extracted information for new years eve
  #' @param f_period Forecast period
  #' @export
  #' @examples
  #' att <- normalise_by_month(b = .., nye = .., f_period = ..)


  #the logic might need to be updated further to account for indexes that cross from month to month
  yr<-year(f_period[1])
  if(yr %% 4 ==0 ){
    b <- rbind(b, nye)#need to add an extra one because of leap year
    b <- rbind(b, nye)
  }else{
    b <- rbind(b, nye)
  }
  b$Activation_date <- f_period
  b$year <- year(f_period)
  b$month <- month(f_period)
  b$week <- format(b$Activation_date, format="%U")
  b$day <- weekdays(f_period)
  m_total <- b %>%
    group_by(Country_Code, year, month) %>%
    summarise(t_monthly = sum(normalised_daily))
  b <- merge(b, m_total, by = c("Country_Code", "year", "month"))
  b <- b %>%
    mutate(updated_normalised_daily = normalised_daily/t_monthly) %>%
    arrange(Country_Code, Activation_date)
  return(b)
}
