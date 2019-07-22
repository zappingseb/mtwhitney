#library(tcltk)

library(RSelenium)

source("RSelenium.R")

sink("my_log.txt")

print("Open Driver")
remDr <- remoteDriver(remoteServerAddr = "seleniumcontainer", browserName = "phantomjs")

print("remDr$open")
remDr$open()

print("navigate")
remDr$navigate("https://www.recreation.gov/permits/233260/")

Sys.sleep(8)
tryCatch({
  el_1 <- remDr$findElements("id", "division-selection-select")
  el_1[[1]]$clickElement()
  el_2 <- remDr$findElements("css selector", ".rec-select-option-button")
  el_2[[2]]$clickElement()
  Sys.sleep(1)

  message("Enter Text")
  el_3 <- remDr$findElements("id", "number-input-")
  remDr$executeScript(
      paste0("arguments[0].setAttribute('value','",
          "1","');"),
      list(el_3[[1]]))
  el_3[[1]]$clearElement()
  el_3[[1]]$sendKeysToElement(list("1"))

  message("get Month")
  month_elem <- remDr$findElements("id", "CalendarMonth__caption")

  month <- month_elem[[1]]$getElementText()

  message("Month while start")
  while(!grepl("October", month)) {
    el_4 <- remDr$findElements("css selector", ".DayPickerNavigation__next")
    el_4[[2]]$clickElement()
    month_elem <- remDr$findElements("id", "CalendarMonth__caption")
    month <- month_elem[[1]]$getElementText()
  }

  message("Read Early day")
  day_elem <- remDr$findElements("css selector", ".rec-available-day")[[1]]
  earliest_day <- day_elem$getElementText()

  sink()

  log <- readLines("my_log.txt")

  fileConn<-file("output.txt")
  writeLines(paste0("The earliest day for Mnt Whitney in October is: ",
          earliest_day, "th of October 2019.\n\n-------------------\nLOG:\n", paste(log, collapse = "\n")), con = fileConn)
  close(fileConn)


}, error = function(e){
  remDr$close()
})



