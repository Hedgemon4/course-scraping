# load required packages
setwd("C:/Users/spenc/Documents/R/Projects/course-scraping/src/web-scraping-testing")
library(rvest)
library(dplyr)
library(stringr)
source("stringUtil.R")

# Webpage link to Waterloo Data Science Academic Calander
link = "http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1"

academicCalander = read_html(link)

# Get webpage title
title = academicCalander %>%
  html_node("title") %>%
  html_text()

title <- str_squish(title)

# Get course codes
courseLinks <- academicCalander %>%
  html_nodes("#ctl00_contentMain_lblContent li a")

courseCodes <- html_text(courseLinks)
courseCodes

# Get course names
courseNames <- academicCalander %>%
  html_nodes("li li") %>%
  html_text()

courseNames <- remove_from_string(courseNames, courseCodes)
courseNames
