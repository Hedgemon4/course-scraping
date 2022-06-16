# Course Scraping Waterloo Data Science Requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Get Course Requirements
academic_calander_link <- "http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1"
course_requirements_waterloo <- get_course_dataframe(academic_calander_link, "#ctl00_contentMain_lblContent li a", "li li")
colnames(course_requirements_waterloo) <- c('Course Code', 'Course Name')

# Clean Data
course_requirements_waterloo["Course Name"] <- gsub("CS.\\d+.|MATH.\\d+.|STAT.\\d+.", "", course_requirements_waterloo$`Course Name`) %>% str_squish()

# Get courses by subject
compsci_courses_waterloo <- get_course_dataframe("http://ugradcalendar.uwaterloo.ca/courses/CS", 
                                        ".divTableCell:nth-child(1) strong", ".colspan-2 strong", ".colspan-2:nth-child(4)")
colnames(compsci_courses_waterloo) <- c('Course Code', 'Course Name', 'Course Description')
math_courses_waterloo <- get_course_dataframe("http://ugradcalendar.uwaterloo.ca/courses/MATH", 
                                              ".divTableCell:nth-child(1) strong", ".colspan-2 strong", ".colspan-2:nth-child(4)")
colnames(math_courses_waterloo) <- c('Course Code', 'Course Name', 'Course Description')
stat_courses_waterloo <- get_course_dataframe("http://ugradcalendar.uwaterloo.ca/courses/STAT", 
                                              ".divTableCell:nth-child(1) strong", ".colspan-2 strong", ".colspan-2:nth-child(4)")
colnames(stat_courses_waterloo) <- c('Course Code', 'Course Name', 'Course Description')

# Other information
other_information <- read_html("http://ugradcalendar.uwaterloo.ca/courses/CS") %>% html_nodes("em") %>%
  html_text() %>% str_squish()

prereq <- array()
antireq <- array()
other <- array()
i = 0

for(item in other_information){
  if(item == ""){
    i = i + 1
    next
  }else if (grepl("Prereq", item)){
    prereq[i][1] <- item
  }else if (grepl("Antireq", item))
    antireq[i][1] <- item
  else if (grepl("Note:", item)){
    other[i + 1][1] <- item
  }else
    other[i][1] <- item
}    

prereq <- data.frame(prereq)
antireq <- data.frame(antireq)
other <- data.frame(other)

View(prereq)
View(antireq)
View(other)

# Clean Data
clean_from_data <- "LEC|LAB|\\,|TST|TUT|0\\.50|PRJ|RDG|STU|0\\.25|0\\.00|2\\.50"
compsci_courses_waterloo["Course Code"] <- gsub(clean_from_data, "", compsci_courses_waterloo$`Course Code`) %>% str_squish()
math_courses_waterloo["Course Code"] <- gsub(clean_from_data, "", math_courses_waterloo$`Course Code`) %>% str_squish()
stat_courses_waterloo["Course Code"] <- gsub(clean_from_data, "", stat_courses_waterloo$`Course Code`) %>% str_squish()

# Merge Data
compsci <- merge(course_requirements_waterloo, compsci_courses_waterloo, by=c("Course Code", "Course Name"))
math <- merge(course_requirements_waterloo, math_courses_waterloo, by=c("Course Code", "Course Name"))
stat <- merge(course_requirements_waterloo, stat_courses_waterloo, by=c("Course Code", "Course Name"))

courses <- rbind(compsci, math, stat)
courses <- as.data.frame(lapply(courses, unlist))
course_requirements_waterloo <- as.data.frame(lapply(course_requirements_waterloo, unlist))
# Display Data Frames
View(compsci_courses_waterloo)
View(course_requirements_waterloo)
View(courses)
View(math_courses_waterloo)
View(stat_courses_waterloo)
