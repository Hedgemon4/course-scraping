# Simon Fraser University Data Science Program Requirement Scraping ####
# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Program Requirements

program_link <- "https://www.sfu.ca/students/calendar/programs/data-science/major/bachelor-of-science/"
program_page <- read_html(program_link)

program_information <- html_nodes(program_page, ".course > a, #page-content > section > p, .course+ h3") %>%
  html_text() %>% str_squish() %>% 
  str_extract_all("((BUS|CMPT|MACM|DATA|MATH|STAT|BUS|ECON)\\s([0-9]{3}))|(Upper\\sDivision.*)|(.*Students\\scomplete.*)|((one|both|all)\\sof.*)") %>%
  unlist() %>% str_remove_all("Students\\scomplete.*units.*") %>% stri_remove_empty()

# Courses ####

# Business Courses
business_course_link <- "https://www.sfu.ca/students/calendar/courses/bus"
business_course_page <- read_html(business_course_link)

business_courses <- html_nodes(business_course_page, ".main > h3") %>% 
  html_text() %>% str_squish()
business_course_code <- gsub("(BUS\\s[0-9]{3}[A-Z]?)\\s\\-\\s(.*)(\\([0-9]*\\))?", "\\1", business_courses)
business_course_name <- gsub("(BUS\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(\\([0-9]\\))?", "\\2", business_courses) %>%
  str_squish()
business_course_credit_amount <- gsub("(BUS\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(?:\\(([0-9]*)\\))?", "\\3", business_courses) %>%
  str_squish() %>% as.numeric() %>% str_replace_na("0") %>% as.numeric()

business_course_information <- html_nodes(business_course_page, ".main > p") %>%
  html_text() %>% str_squish()

business_course_prereq <- gsub("(.*)(Prerequisite.*?(?:\\.|corequisite))(.*)", "\\2", business_course_information)
business_course_prereq <-  grep("Prerequisite",business_course_prereq, invert = TRUE) %>% 
  replace(business_course_prereq, ., "")

business_course_coreq <- gsub("(.*)((?:C|c)orequisite.*?(\\.))(.*)", "\\2", business_course_information)
business_course_coreq <- grep("Corequisite", business_course_coreq, invert = TRUE) %>% 
  replace(business_course_coreq, ., "")

business_course_antireq <- gsub("(.*)(Students\\swith\\scredit\\sfor.*?\\.)(.*)", "\\2", business_course_information)
business_course_antireq <- grep("Students\\swith\\scredit\\sfor", business_course_antireq, invert = TRUE) %>%
  replace(business_course_antireq, ., "")

