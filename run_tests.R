setwd("C:/_wolfs25/git/mtwhitney")
library(tcltk)
library(RTest)
Sys.setenv("JAVA_HOME" = "C:/Program Files/Java/jdk1.8.0_201")
devtools::load_all("C:/_wolfs25/git/RSeleniumTest")


source("overwrite_expect.R")

execute("Sebastian Wolf", project.name = "Crawl Mt Whitney")
