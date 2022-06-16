# load required packages
# setwd("C:/Users/spenc/Documents/R/Projects/course-scraping/src/web-scraping-testing")
library(rvest)
library(dplyr)
library(stringr)

# functions
get_courses <- function(course_description_link, course_description_node, course_code_node, coure_name_node){
  course_description <- read_html(course_description_link) %>% html_nodes(course_description_node) %>%
    html_text() %>% str_squish() 
  course_code <- read_html(course_description_link) %>% html_nodes(course_code_node) %>%
    html_text() %>% remove_from_course_code() %>% str_squish()
  course_name <- read_html(course_description_link) %>% html_nodes(coure_name_node) %>%
    html_text() %>% str_squish()
  return(data.frame(course_code, course_name, course_description))
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

# Fix spelling errors
courseNames[1] <- substr(courseNames[1], 0, nchar(courseNames[1]) - 1)

# Create data frame with course codes and names
courses_waterloo = data.frame(courseCodes, courseNames)
colnames(courses_waterloo) <- c('Course Code', 'Course Name')

# TODO: Get course descriptions from course links

# Get course descriptions
compsci_courses_waterloo <- get_courses("http://ugradcalendar.uwaterloo.ca/courses/CS", 
                                        ".colspan-2:nth-child(4)", ".divTableCell:nth-child(1) strong", ".colspan-2 strong")
colnames(compsci_courses_waterloo) <- c('Course Code', 'Course Name', 'Course Description')
math_courses_waterloo <- get_courses("http://ugradcalendar.uwaterloo.ca/courses/MATH", 
                                     ".colspan-2:nth-child(4)", ".divTableCell:nth-child(1) strong", ".colspan-2 strong")
colnames(math_courses_waterloo) <- c('Course Code', 'Course Name', 'Course Description')
stat_courses_waterloo <- get_courses("http://ugradcalendar.uwaterloo.ca/courses/STAT", 
                                     ".colspan-2:nth-child(4)", ".divTableCell:nth-child(1) strong", ".colspan-2 strong")
colnames(stat_courses_waterloo) <- c('Course Code', 'Course Name', 'Course Description')

# TODO: Use merge to get spreadsheets info into one category
courses_compsci <- merge(courses_waterloo, compsci_courses_waterloo, by=c("Course Code", "Course Name"))
courses_math <- merge(courses_waterloo, math_courses_waterloo, by=c("Course Code", "Course Name"))
courses_stat <- merge(courses_waterloo, stat_courses_waterloo, by=c("Course Code", "Course Name"))

courses <- rbind(courses_compsci, courses_math, courses_stat)

# Display data frames
View(courses_waterloo)
View(compsci_courses_waterloo)
View(math_courses_waterloo)
View(stat_courses_waterloo)
View(courses)

# TODO: Get "one of", "all of", and other labels from requirements
# TODO: Scrape lab, requirements, and other information from Waterloo webpage
# TODO: Generalize functions for course scraping from other universities
# TODO: Try functions on other universities webpages

# Write files to csv
# write.csv(compsci_courses_waterloo, "compsci_courses_waterloo.csv")
# write.csv(math_courses, "math_courses_waterloo.csv")
# write.csv(stat_courses, "stat_courses_waterloo.csv")
# write.csv(courses, "courses_waterloo.csv")
