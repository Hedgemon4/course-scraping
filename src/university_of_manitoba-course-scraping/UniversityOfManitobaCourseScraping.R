# Data Science Requirement Scraping from the University of Manitoba ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# TODO List ####
# TODO: Figure out how to seperate information in table on main page
# TODO: Get course information

# Program Requirements ####
web_link <- "https://catalog.umanitoba.ca/undergraduate-studies/science/data-science/data-science-bsc-major"

web_page <- read_html(web_link)

course_codes <- html_nodes(web_page, "#degreerequirementstextcontainer .code") %>% html_text()

course_nodes <- html_nodes(web_page, "#degreerequirementstextcontainer .code")

# Course Information ####

courses_main_link <- "https://catalog.umanitoba.ca/"
course_info_links <- html_nodes(web_page, "#degreerequirementstextcontainer .code") %>% html_attr("href")

course_info_test <- read_html(paste0(courses_main_link, course_info_links[1])) %>% html_nodes(".courseblockextra") %>% html_text() %>% str_squish()

course_description <- vector(mode = "character", length = length(course_info_links))
prereq <- vector(mode = "character", length = length(course_info_links))
coreq <- vector(mode = "character", length = length(course_info_links))
attributes <- vector(mode = "character", length = length(course_info_links))
eqiv <- vector(mode = "character", length = length(course_info_links))
exclusive <- vector(mode = "character", length = length(course_info_links))


information <- html_nodes(web_page, "#degreerequirementstextcontainer .codecol") %>% html_text()

program_table <- html_nodes(web_page, "#degreerequirementstextcontainer > table.sc_plangrid") %>% html_table() %>% .[[1]]

course_names <- html_elements(web_page, xpath = "//*[@id=\"degreerequirementstextcontainer\"]/table/tr/td/text()") %>% html_text()
