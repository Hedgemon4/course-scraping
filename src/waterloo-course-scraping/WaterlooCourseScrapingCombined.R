# Waterloo Data Science Requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Source for util functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Functions for this script

# test1 <- read_html(web_link) %>% html_elements(xpath = "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li/ul/li") %>% html_text() %>% str_squish()
# Filter categories by html layout instead of string matching

get_courses_in_category <- function(categories) {
  category_names <- get_item_vector("Category", length(categories))
  course_category <- vector(mode = "character")
  course_category_description <- vector(mode = "character")
  i <- 1
  j <- 1
  while (i <= length(categories)) {
    courses_in_category <-
      read_html(web_link) %>% html_nodes(paste0(
        "#ctl00_contentMain_lblContent > ul > li:nth-child(",
        i,
        ") > ul > li > a"
      )) %>% html_text() %>% str_squish()
    for (course in courses_in_category) {
      course_category[j] <- category_names[i]
      course_category_description[j] <- categories[i]
      j = j + 1
    }
    i = i + 1
  }
  return(data.frame(course_category, course_category_description))
}

# Get Requirement Course Codes and Categories
data_science_link <- "http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1"
data_science_requirements <- get_text_dataframe(data_science_link, "#ctl00_contentMain_lblContent ul ul a")
stat_link <- "http://ugradcalendar.uwaterloo.ca/page/MATH-Statistics1"
stat_requirements <- get_text_dataframe(stat_link, "#ctl00_contentMain_lblContent ul ul a")

# Get Categories
data_science_categories <- get_text_xpath(data_science_link, "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li/text()")
stat_categories <- get_text_xpath(stat_link, "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li/text()")

# Protects against changing information if website structure changes
if(length(stat_categories) == 9){
  stat_categories[4] <- paste(stat_categories[4], "STAT", stat_categories[5], " ")
  stat_categories <- stat_categories[-5]
} else stop("The website structure has changed. Please look at the statistics degree requirements.")

test1 <- get_courses_in_category(data_science_categories)
stat_requirements <- get_courses_in_category()
