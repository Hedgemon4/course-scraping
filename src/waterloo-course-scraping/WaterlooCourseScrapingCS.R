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
  get_text_dataframe(academic_calander_link,
                       "#ctl00_contentMain_lblContent li a",
                       "li li")
colnames(course_requirements_waterloo) <-
  c('Course Code', 'Course Name')

# Clean Data
course_requirements_waterloo["Course Name"] <-
  gsub("CS.\\d+.|MATH.\\d+.|STAT.\\d+.",
       "",
       course_requirements_waterloo$`Course Name`) %>% str_squish()
course_requirements_waterloo[1, 2] <-
  "Elementary Algorithm Design and Data Abstraction"

categories <- 
  get_requirement_categories(
    "http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1",
    "p+ ul li",
    c("One", "Two", "Three", "All")
  )
colnames(categories) <- c("Category", "Required from Category")

course_requirements_waterloo <-
  cbind.data.frame(course_requirements_waterloo, categories)

# Get courses by subject
compsci_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(compsci_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

math_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(math_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

stat_courses_waterloo <-
  get_text_dataframe(
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

matches <- c("Prereq:", "Antireq:", "Coreq:", "Note", ".")
columns <-
  c("Prerequisite",
    "Antirequisite",
    "Corequisite",
    "Note",
    "Other Information")

compsci_courses_waterloo <-
  seperate_information_increment(
    matches,
    columns,
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".colspan-2 :nth-child(1)",
    compsci_courses_waterloo,
    "character",
    TRUE,
    2,
    1
  )

math_courses_waterloo <-
  seperate_information_increment(
    matches,
    columns,
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".colspan-2 :nth-child(1)",
    math_courses_waterloo,
    "character",
    TRUE,
    2,
    1
  )

stat_courses_waterloo <-
  seperate_information_increment(
    matches,
    columns,
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".colspan-2 :nth-child(1)",
    stat_courses_waterloo,
    "character",
    TRUE,
    2,
    1
  )

# Scrape Course Component Data
course_component <-
  c("LEC", "LAB", "TST", "TUT", "PRJ", "RDG", "STU")

course_component_name <-
  c("Lecture",
    "Lab",
    "Test Slot",
    "Tutorial",
    "Project",
    "Reading",
    "Studio")

compsci_courses_waterloo <-
  seperate_information(
    course_component,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".divTableCell:nth-child(1) strong",
    compsci_courses_waterloo,
    "logical",
    FALSE
  )

math_courses_waterloo <-
  seperate_information(
    course_component,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".divTableCell:nth-child(1) strong",
    math_courses_waterloo,
    "logical",
    FALSE
  )

stat_courses_waterloo <-
  seperate_information(
    course_component,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".divTableCell:nth-child(1) strong",
    stat_courses_waterloo,
    "logical",
    FALSE
  )

# Scrape Credit Data
credit_value <- c("0.00", "0.25", "0.50", "2.50")
name <- c("Credit Amount")

compsci_courses_waterloo <-
  seperate_information_single(
    credit_value,
    name,
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".divTableCell:nth-child(1) strong",
    compsci_courses_waterloo,
    "character",
    TRUE
  )

math_courses_waterloo <-
  seperate_information_single(
    credit_value,
    name,
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".divTableCell:nth-child(1) strong",
    math_courses_waterloo,
    "character",
    TRUE
  )

stat_courses_waterloo <-
  seperate_information_single(
    credit_value,
    name,
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".divTableCell:nth-child(1) strong",
    stat_courses_waterloo,
    "character",
    TRUE
  )

# Merge Data
compsci <-
  merge(
    course_requirements_waterloo,
    compsci_courses_waterloo,
    by = c("Course Code", "Course Name")
  )

math <-
  merge(
    course_requirements_waterloo,
    math_courses_waterloo,
    by = c("Course Code", "Course Name")
  )

stat <-
  merge(
    course_requirements_waterloo,
    stat_courses_waterloo,
    by = c("Course Code", "Course Name")
  )

courses <- rbind(compsci, math, stat)
View(courses)
