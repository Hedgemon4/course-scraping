# load required packages
setwd("C:/Users/spenc/Documents/R/Projects/course-scraping/src/web-scraping-testing")
library(rvest)
library(dplyr)
library(stringr)
source("stringUtil.R")

# Webpage link to Waterloo Data Science Academic Calendar
academicCalendar = read_html("http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1")

# Get webpage title
title = academicCalendar %>%
  html_node("title") %>%
  html_text()

title <- str_squish(title)

# Get course codes
courseCodes <- academicCalendar %>% html_nodes("#ctl00_contentMain_lblContent li a") %>% html_text()

# Get course names
courseNamesWithCodes <- academicCalendar %>%
  html_nodes("li li") %>%
  html_text()

# Remove codes from course names
courseNames <- remove_from_string(courseNamesWithCodes, courseCodes)

# Create data frame with course codes and names
courses = data.frame(courseCodes, courseNames)
colnames(courses) <- c('Course Number', 'Course Name')

# Display Data Frame
courses['Course Description'] <- NA
View(courses)

# TODO: Get course descriptions from course links

# Get course desrciption
get_courses <- function(course_description_link){
  course_descriptions <- read_html(course_description_link) %>% html_nodes(".colspan-2:nth-child(4)") %>%
    html_text()
  course_codes <- read_html(course_description_link) %>% html_nodes(".divTableCell:nth-child(1) strong") %>%
    html_text()
  course_names <- read_html(course_description_link) %>% html_nodes(".colspan-2 strong") %>%
    html_text()
  return(data.frame(course_codes, course_names, course_descriptions))
}

compsci_courses <- get_courses("http://ugradcalendar.uwaterloo.ca/courses/CS")
View(compsci_courses)
math_courses <- get_courses("http://ugradcalendar.uwaterloo.ca/courses/MATH")
View(math_courses)
stat_courses <- get_courses("http://ugradcalendar.uwaterloo.ca/courses/STAT")
View(stat_courses)
