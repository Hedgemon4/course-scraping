# Concordia University Data Science Program Requirement Scraping ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

program_link <- "https://www.concordia.ca/academics/undergraduate/calendar/current/section-31-faculty-of-arts-and-science/section-31-200-department-of-mathematics-and-statistics/ba-bsc-joint-major-in-data-science.html"
program_page <- read_html(program_link)

requirements_table1 <- html_nodes(program_page, "#content-main > div > div > div:nth-child(2) > div > div > div.container > div > div.program-node-children.joint-major-in-data-science > div > div > div:nth-child(1) > table") %>%
  html_table() %>% .[[1]]

requirements_table2 <- html_nodes(program_page, "#content-main > div > div > div:nth-child(2) > div > div > div.container > div > div.program-node-children.joint-major-in-data-science > div > div > div:nth-child(2) > table") %>% 
  html_table() %>% .[[1]]

test <- html_nodes(program_page, "#content-main > div > div > div:nth-child(2) > div > div > div.container > div > div.program-node-children.joint-major-in-data-science > div > div > div > table") %>%
  html_table() %>% unlist()
