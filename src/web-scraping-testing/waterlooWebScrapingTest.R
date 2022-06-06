# load required packages

library(rvest)
library(dplyr)
library(stringr)


# Webpage link to Waterloo Data Science Academic Calander
link = "http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1"

academicCalander = read_html(link)

# Get webpage title
title = academicCalander %>%
  html_node("title") %>%
  html_text()

title <- str_squish(title)

title

# Get course codes
courseCodes <- academicCalander %>%
  html_nodes("#ctl00_contentMain_lblContent li a") %>%
  html_text()

courseCodes <- str_squish(courses)
courseCodes

# Get course names
courseNames <- academicCalander %>%
  html_nodes("#ctl00_contentMain_lblContent li") %>%
  html_text()

courseNames
