#setwd("C:/_wolfs25/git/mtwhitney")
#library(tcltk)

#Sys.setenv("JAVA_HOME" = "C:/Program Files/Java/jdk1.8.0_201")

library(RSelenium)

source("RSelenium.R")

sink("my_log.txt")

#init_RSelenium("https://www.recreation.gov/permits/233260/",
#    driver_location = "C:/Programme_2/RSelenium/selenium-server-standalone-3.141.59.jar",
#    ie_driver_folder = "C:/Programme_2/RSelenium",
#    browserName = "chrome"
#    )
print("Open Driver")
remDr <- remoteDriver(browserName = "phantomjs")

print("remDr$open")
remDr$open()

print("navigate")
remDr$navigate("https://www.recreation.gov/permits/233260/")

Sys.sleep(8)

click_element(
    object = remDr,
    type_of_search = "id",
    value = "division-selection-select",
    iframe = FALSE
    )

click_element(object = remDr,
    type_of_search = "css selector",
    value = ".rec-select-option-button",
    iframe = FALSE,
    find = TRUE,
    which = 2
    )
Sys.sleep(2)
enter_text(object = remDr,
    type_of_search = "id",
    value = "number-input-",
    iframe = FALSE,
    text_to_type = "1",
    visibility = FALSE
    )

month <- get_element_text(
    object = remDr,
    type_of_search = "id",
    value = "CalendarMonth__caption",
    iframe = FALSE
    )
while(!grepl("October",month)) {
  click_element(object = remDr,
      type_of_search = "css selector",
      value = ".DayPickerNavigation__next",
      iframe = FALSE,
      find = TRUE,
      which = 2
  )
  month <- get_element_text(
      object = remDr,
      type_of_search = "id",
      value = "CalendarMonth__caption",
      iframe = FALSE
  )
}


earliest_day <- get_element_text(
    object = remDr,
    type_of_search = "css selector",
    value = ".rec-available-day",
    iframe = FALSE,
    which = 1
)[1]

sink()

log <- readLines("my_log.txt")

fileConn<-file("output.txt")
writeLines(paste0("The earliest day for Mnt Whitney in October is: ",
        earliest_day, "th of October 2019.\n\n-------------------\nLOG:\n", paste(log, collapse = "\n")), con = fileConn)
close(fileConn)

