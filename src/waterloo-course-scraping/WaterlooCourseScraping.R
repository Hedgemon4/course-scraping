# Course Scraping Waterloo Data Science Requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Get Course Requirements
academic_calander_link <-
  "http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1"
course_requirements_waterloo <-
  get_course_dataframe(academic_calander_link,
                       "#ctl00_contentMain_lblContent li a",
                       "li li")
colnames(course_requirements_waterloo) <-
  c('Course Code', 'Course Name')

# Clean Data
course_requirements_waterloo["Course Name"] <-
  gsub("CS.\\d+.|MATH.\\d+.|STAT.\\d+.",
       "",
       course_requirements_waterloo$`Course Name`) %>% str_squish()
course_requirements_waterloo[1, 2] <- "Elementary Algorithm Design and Data Abstraction"

categories <- get_requirement_categories("http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1", "p+ ul li", c("One", "Two", "Three", "All"))
colnames(categories) <- c("Category", "Required from Category")

course_requirements_waterloo <- cbind.data.frame(course_requirements_waterloo, categories)

# Get courses by subject
compsci_courses_waterloo <-
  get_course_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )
colnames(compsci_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')
math_courses_waterloo <-
  get_course_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )
colnames(math_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')
stat_courses_waterloo <-
  get_course_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )
colnames(stat_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

# Clean Data
clean_from_data <-
  "LEC|LAB|\\,|TST|TUT|0\\.50|PRJ|RDG|STU|0\\.25|0\\.00|2\\.50"
compsci_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", compsci_courses_waterloo$`Course Code`) %>% str_squish()
math_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", math_courses_waterloo$`Course Code`) %>% str_squish()
stat_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", stat_courses_waterloo$`Course Code`) %>% str_squish()

# Get other course information

compsci_courses_waterloo <- cbind.data.frame(compsci_courses_waterloo, get_other_course_information(
  "http://ugradcalendar.uwaterloo.ca/courses/CS", ".colspan-2 :nth-child(1)", compsci_courses_waterloo))
math_courses_waterloo <- cbind.data.frame(math_courses_waterloo, get_other_course_information(
  "http://ugradcalendar.uwaterloo.ca/courses/MATH", ".colspan-2 :nth-child(1)", math_courses_waterloo))
stat_courses_waterloo <- cbind.data.frame(stat_courses_waterloo, get_other_course_information(
  "http://ugradcalendar.uwaterloo.ca/courses/STAT", ".colspan-2 :nth-child(1)", stat_courses_waterloo))

# Merge Data
compsci <- merge(course_requirements_waterloo,compsci_courses_waterloo, by = c("Course Code", "Course Name"))
math <- merge(course_requirements_waterloo,math_courses_waterloo,by = c("Course Code", "Course Name"))
stat <-merge(course_requirements_waterloo,stat_courses_waterloo,by = c("Course Code", "Course Name"))

courses <- rbind(compsci, math, stat)
View(courses)
