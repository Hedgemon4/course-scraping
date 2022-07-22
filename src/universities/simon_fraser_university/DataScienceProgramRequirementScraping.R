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

test <- html_nodes(program_page, ".course > a, #page-content > section > p, .course+ h3") %>%
  html_text() %>% str_squish()
