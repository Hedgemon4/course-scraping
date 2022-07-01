# Data Science Requirement Scraping from the University of Manitoba ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# TODO List ####
# TODO: Figure out how to seperate information in table on main page
# TODO: Get course information
# TODO: Watch video about dplyr
# TODO: Use course lists to get categories and category requirements

# Program Requirements ####
web_link <- "https://catalog.umanitoba.ca/undergraduate-studies/science/data-science/data-science-bsc-major"

web_page <- read_html(web_link)

course_codes <- html_nodes(web_page, "#degreerequirementstextcontainer .code") %>% html_text()

course_nodes <- html_nodes(web_page, "#degreerequirementstextcontainer .code")

# Course Information ####

courses_main_link <- "https://catalog.umanitoba.ca/"
course_info_links <- html_nodes(web_page, "#degreerequirementstextcontainer .code") %>% html_attr("href")
number_of_courses <- length(course_info_links)

course_name <- vector(mode = "character", length = number_of_courses)
course_description <- vector(mode = "character", length = number_of_courses)
course_credit <- vector(mode = "character", length = number_of_courses)
prereq <- vector(mode = "character", length = number_of_courses)
coreq <- vector(mode = "character", length = number_of_courses)
attributes <- vector(mode = "character", length = number_of_courses)
equiv <- vector(mode = "character", length = number_of_courses)
exclusive <- vector(mode = "character", length = number_of_courses)

i <- 1
for(item in course_info_links){
  course_link <- paste0(courses_main_link, course_info_links[i])
  course_page <- read_html(course_link)
  course_information <- html_nodes(course_page, ".courseblockextra") %>% html_text() %>% str_squish()
  course_name[i] <- html_node(course_page, ".detail-title strong") %>% html_text() %>% str_squish()
  course_description[i] <- course_information[1]
  course_credit[i] <- html_node(course_page, ".detail-hours_html strong") %>% html_text() %>% str_squish()
  equiv[i] <- paste(grep("Equiv To:", course_information, value = TRUE), collapse = "")
  exclusive[i] <- paste(grep("Mutually Exclusive:", course_information, value = TRUE), collapse = "")
  attributes[i] <- paste(grep("Attributes:", course_information, value = TRUE), collapse = "")
  prcr <- grep("PR/CR", course_information, value = TRUE) %>% strsplit(split = "\\.") %>% unlist()
  prereq[i] <- grep("Prerequisite(s|)", prcr, ignore.case = TRUE, value = TRUE) %>% paste(collapse = "")
  coreq[i] <- grep("Co-requisite|corequisite|co requisite", prcr, ignore.case = TRUE, value = TRUE) %>% paste(collapse = "")
  i = i + 1
}

course_info <- data.frame(course_codes, course_name, course_description, course_credit, prereq, coreq, equiv, exclusive, attributes)
colnames(course_info) <- c("Course Code", "Course Name", "Course Description", "Course Credit", "Prerequisite", "Corequisite", "Equivalency", "Mutually Exclusive", "Attributes")
information <- html_nodes(web_page, "#degreerequirementstextcontainer .codecol") %>% html_text()

program_table <- html_nodes(web_page, "#degreerequirementstextcontainer > table.sc_plangrid") %>% html_table() %>% .[[1]]
colnames(program_table) <- c("code", "name", "credit")

core_requirements <- program_table %>% filter(str_detect(code, "COMP|MATH|DATA|STAT"), credit == '3') %>% select(code)
core_requirement_category <- rep("Core", nrow(core_requirements))
core_requirement_category_description <- rep("Core Requirement", nrow(core_requirements))
core_requirements <- cbind(core_requirements, core_requirement_category, core_requirement_category_description)
colnames(core_requirements) <- c("Course Code", "Category", "Category Description")

general_requirements <- program_table %>% filter(str_detect(code, 'credit'), str_detect(code, 'above|from:', negate = TRUE)) %>% select(code, credit)
general_requirement_category <- rep("General", nrow(general_requirements))
general_requirement_code <- rep("General Requirement", nrow(general_requirements))
general_requirement_empty <- rep(NA, nrow(general_requirements))
general_requirements <- cbind(general_requirement_code, general_requirement_category, general_requirements)
add_
colnames(general_requirements) <- cbind("Course Code", "Category", "Category Description", "Credit Amount")
