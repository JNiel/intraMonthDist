holiday_scraper <- function(countries = c("sweden", "austria", "norway", "denmark", "finland", "germany", "netherlands", "uk", "us"),
                            start_year = 2010,
                            end_year = 2020){
  #' A Holiday Scraper
  #'
  #' This function allows you to gather holiday dates and their type for specified countries over a specified time period.
  #' Data is gathered from timeanddate.com, so make sure your internet connection is up for it to work properly.
  #' Output data frame "master calendar" with variables:
  #' country - Country for given holiday dates.
  #' year - Year of given holiday dates.
  #' Date - date of the actual holiday listed.
  #' holiday_name - Official name of holiday given in calendar.
  #' holiday_type - Type of holiday. All types are included here, in total 26 different.
  #' Date_fmt - Date output in date format Y-m-d.
  #' @param countries Vector with countries for which holidays are wanted. Full name, lower case, with exception of "uk" and "us". Defaults to c("sweden", "norway", "denmark", "finland", "germany", "netherlands", "uk", "us").
  #' @param start_year Numeric year of which you want the calendar to start from. Defaults to 2010.
  #' @param end_year Numeric year of which you want the calendar to end at. Defaults to 2020.
  #' @keywords calendar
  #' @export
  #' @examples
  #' holiday_scraper(countries = c("se", "no", "fi"), start_year = 2015, end_year = 2018)

  #OBS! SINCE THIS IS A WEB SCRAPER - YOU NEED TO HAVE PROPER INTERNET CONNECTION FOR IT TO WORK.

  #Jon Nielsen - 2017-01-17

  require(rvest)
  require(dplyr)
  #Initial step: set countries you wish to get dates for and year range:
  rm(master_calendar)

  years <- seq(start_year, end_year, 1)
  url_base <- "https://www.timeanddate.com/holidays/"
  master_calendar <- data.frame()

  #Set loale to english so that dates are read correctly
  Sys.setlocale(category = "LC_TIME", locale ="English")

  for(i in  1:length(countries)) {
    for(j in 1:length(years)) {

      #url <- "https://www.timeanddate.com/holidays/sweden/2017"
      url <- paste(url_base, countries[i], "/", years[j], sep="")
      calendar <- url %>%
        read_html() %>%
        html_nodes(xpath = '/html/body/div[1]/div[7]/article/section[1]/div[5]/table') %>%
        html_table() %>%
        .[[1]] %>%
        select(Date, holiday_name = get("Holiday Name"), holiday_type = get("Holiday Type"))
      country <- rep(countries[i], dim(calendar)[1])
      year <- rep(years[j], dim(calendar)[1])
      master_calendar <- rbind(master_calendar, cbind(country, year, calendar)[-1,])
    }
  }
  master_calendar$Date_fmt <- as.Date(paste(master_calendar$year, master_calendar$Date), format="%Y %b %d")

  return(master_calendar)
}
