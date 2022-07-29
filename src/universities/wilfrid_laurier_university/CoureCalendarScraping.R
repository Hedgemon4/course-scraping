# Course Calendar Scraping for Wilfrid Laurier University

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Business Courses ####

business_link <- "https://academic-calendar.wlu.ca/department.php?cal=1&d=2617&s=1036&y=85#Course_Offerings"
business_page <- read_html(business_link)

business_calendar <- html_nodes(business_page, "#main > div.content > div > table") %>%
  html_table() %>% .[[1]]
colnames(business_calendar) <- c("Course Code", "Course Name", "Credit Amount")

business_course_links <- html_nodes(business_page, "#main > div.content > div > table > tr > td > a") %>%
  html_attr("href")

business_calendar_link <- "https://academic-calendar.wlu.ca/"
num_business_courses <- nrow(business_calendar)
hours <- vector(mode = "character")
description <- vector(mode = "character")

i <- 1
while(i < num_business_courses){
  course_page <- read_html(paste0(business_calendar_link, business_course_links[i]))
  hours[i] <- html_nodes(course_page, "#main > div.content > div.hours") %>% paste0("") %>% str_squish()
  description[i] <- html_nodes(course_page, "#main > div.content > p") %>% paste0(collapse = "") %>% str_squish()
  i <- i + 1
}

closeAllConnections()
