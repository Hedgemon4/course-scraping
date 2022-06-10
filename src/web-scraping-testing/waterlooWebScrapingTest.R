# load required packages
# setwd("C:/Users/spenc/Documents/R/Projects/course-scraping/src/web-scraping-testing")
library(rvest)
library(dplyr)
library(stringr)

# functions
get_courses <- function(course_description_link){
  course_descriptions <- read_html(course_description_link) %>% html_nodes(".colspan-2:nth-child(4)") %>%
    html_text() %>% str_squish() 
  course_codes <- read_html(course_description_link) %>% html_nodes(".divTableCell:nth-child(1) strong") %>%
    html_text() %>% remove_from_course_code() %>% str_squish()
  course_names <- read_html(course_description_link) %>% html_nodes(".colspan-2 strong") %>%
    html_text() %>% str_squish()
  return(data.frame(course_codes, course_names, course_descriptions))
}

remove_from_course_code <- function(course_code){
  course_code2 <- str_remove_all(course_code, "LEC|LAB|\\,|TST|TUT|0\\.50|PRJ|RDG|STU|0\\.25|0\\.00|2\\.50")
  return(course_code2)
}

remove_from_string <- function(a, b){
  i = 1
  for(val in a){
    remove <- paste(b[i], " ", sep = "")
    a[i] <- str_remove(val, remove)
    i <- i + 1
  }
  return(a)
}

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
courses_waterloo = data.frame(courseCodes, courseNames)
colnames(courses) <- c('Course Code', 'Course Name')

# Display Data Frame
View(courses)

# TODO: Get course descriptions from course links

# Get course desrciptions
compsci_courses_waterloo <- get_courses("http://ugradcalendar.uwaterloo.ca/courses/CS")
colnames(compsci_courses_waterloo) <- c('Course Code', 'Course Name', 'Course Description')
View(compsci_courses_waterloo)
math_courses_waterloo <- get_courses("http://ugradcalendar.uwaterloo.ca/courses/MATH")
colnames(math_courses_waterloo) <- c('Course Code', 'Course Name', 'Course Description')
View(math_courses_waterloo)
stat_courses_waterloo <- get_courses("http://ugradcalendar.uwaterloo.ca/courses/STAT")
colnames(stat_courses_waterloo) <- c('Course Code', 'Course Name', 'Course Description')
View(stat_courses_waterloo)

# TODO: Get "one of", "all of", and other labels from requirements

# Write files to csv
write.csv(compsci_courses_waterloo, "compsci_courses_waterloo.csv")
write.csv(math_courses, "math_courses_waterloo.csv")
write.csv(stat_courses, "stat_courses_waterloo.csv")
write.csv(courses, "courses_waterloo.csv")
