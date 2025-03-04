#library(tcltk)

library(RSelenium)

file.remove("/tmp/my_log.txt")
file.remove("/tmp/output.txt")

sink("/tmp/my_log.txt")
print(date())
print("Open Driver")
remDr <- remoteDriver(remoteServerAddr = "seleniumcontainer", browserName = "chrome")

print("remDr$open")
remDr$open()

print("navigate")
remDr$navigate("https://www.recreation.gov/permits/233260/")

Sys.sleep(8)
tryCatch({
      el_1 <- remDr$findElements("id", "division-selection")
      el_1[[1]]$clickElement()
      el_2 <- remDr$findElements("css selector", "option")
      el_2[[3]]$clickElement()
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
      month_elem <- remDr$findElements("css selector", ".CalendarMonth_caption strong")

      month <- month_elem[[1]]$getElementText()

      message("Month while start")
      while(!grepl("October", month)) {
        el_4 <- remDr$findElements("css selector", ".sarsa-day-picker-range-controller-month-navigation-button.right")
        el_4[[1]]$clickElement()

        Sys.sleep(1)
        month_elem <- remDr$findElements("css selector", ".CalendarMonth_caption")
        month <- month_elem[[2]]$getElementText()
        print(month)
      }

      message("Read Early day")
      day_elem <- remDr$findElements("css selector", ".rec-available-day")

      if (length(day_elem) < 1) {
        earliest_day <- "NODAY"
      } else {
        earliest_day <- strsplit(day_elem[[1]]$getElementText()[[1]], split = "\n")[[1]][1]
      }
      sink()

      remDr$close()

      log <- readLines("/tmp/my_log.txt")

      fileConn<-file("/tmp/output.txt")
      writeLines(paste0("The earliest day for Mnt Whitney in ", month[[1]], " is: ",
              earliest_day, "th of October 2019.\n\n-------------------\nLOG:\n", paste(log, collapse = "\n")), con = fileConn)
      close(fileConn)


    }, error = function(e){
      print(e)
      sink()
      fileConn <- file("/tmp/output.txt")
      log <- readLines("/tmp/my_log.txt")
      writeLines(paste0("ERROR while crawling.\n\n-------------------\nLOG:\n", paste(log, collapse = "\n")), con = fileConn)
      close(fileConn)
      remDr$close()
    })

system("python /tmp/sendmail.py")
