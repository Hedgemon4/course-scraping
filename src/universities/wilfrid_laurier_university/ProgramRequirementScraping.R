# Program Requirement Scraping for Wilfrid Laurier University ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

program_link <- "https://academic-calendar.wlu.ca/program.php?cal=1&d=2589&p=5763&s=1034&y=85"
program_page <- read_html(program_link)
