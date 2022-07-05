# University of Toronto Data Science Program Requirements Scraping

# Packages ####
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Functions ####

# Source for util functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Required Courses ####
program_link <- "https://artsci.calendar.utoronto.ca/program/asspe1687"
courses <- get_text_css(program_link, "#block-fas-content a") %>% unique() %>% 
  grep("CSC|JSC|MAT|STA", .  , value = TRUE)

# Course links
program_page <- read_html(program_link)
course_links <- html_nodes(program_page, "#block-fas-content a") %>% html_attr("href") %>%
  unique() %>% grep("CSC|JSC|MAT|STA", .  , value = TRUE)

academic_calendar_link <- "https://artsci.calendar.utoronto.ca"

course_info_test1 <- get_text_css(paste0(academic_calendar_link, course_links[1]), "#block-fas-content p , .field--label-inline") %>% str_trim()
course_info_test2 <- get_text_css(paste0(academic_calendar_link, course_links[1]), "#block-fas-content p") %>% str_trim()
course_info_test3 <-get_text_css(paste0(academic_calendar_link, course_links[1]), "#block-fas-content .clearfix") %>% str_trim()
