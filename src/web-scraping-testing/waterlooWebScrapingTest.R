# load required packages

library(rvest)
library(dplyr)


# Webpage link to Waterloo Data Science Academic Calander
link = "http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1"

academicCalander = read_html(link)

title = academicCalander %>%
  html_node("title") %>%
  html_text()

title
