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
program_link <-
  "https://artsci.calendar.utoronto.ca/program/asspe1687"
courses <-
  get_text_css(program_link, "#block-fas-content a") %>% unique() %>%
  grep("CSC|JSC|MAT|STA", .  , value = TRUE)

# Course links
program_page <- read_html(program_link)
course_links <-
  html_nodes(program_page, "#block-fas-content a") %>% html_attr("href") %>%
  unique() %>% grep("CSC|JSC|MAT|STA", .  , value = TRUE)

academic_calendar_link <- "https://artsci.calendar.utoronto.ca"

number_of_courses <- length(courses)

course_codes <- vector(mode = "character", length = number_of_courses)
course_names <- vector(mode = "character", length = number_of_courses)
course_descriptions <- vector(mode = "character", length = number_of_courses)
prereq <- vector(mode = "character", length = number_of_courses)
coreq <- vector(mode = "character", length = number_of_courses)
breadth <- vector(mode = "character", length = number_of_courses)
hours <- vector(mode = "character", length = number_of_courses)
antireq <- vector(mode = "character", length = number_of_courses)
distribution <- vector(mode = "character", length = number_of_courses)
delivery <- vector(mode = "character", length = number_of_courses)
antireq <- vector(mode = "character", length = number_of_courses)
recommended <- vector(mode = "character", length = number_of_courses)
other <- vector(mode = "character", length = number_of_courses)

i <- 1
for(link in course_links) {
  course_page <- read_html(paste0(academic_calendar_link, link))
  course_title <-
    html_nodes(course_page, ".page-title") %>% html_text() %>%
    str_split(":") %>% unlist() %>% str_squish()
  course_codes[i] <- course_title[1]
  course_names[i] <- course_title[2]
  course_descriptions[i] <-
    html_nodes(course_page, "#block-fas-content .field--label-hidden p") %>%
    html_text() %>% str_squish() %>% paste(collapse = " ")
  other_course_information <- html_nodes(course_page, "#block-fas-content > div > article > div > div") %>%
    html_text() %>% str_squish()
  other_course_information <- other_course_information[! other_course_information %in% course_descriptions[i]]
  prereq[i] <- grep("Prerequisite", other_course_information, value = TRUE) %>% paste(collapse = " ")
  coreq[i] <- grep("Corequisite", other_course_information, value = TRUE) %>% paste(collapse = " ")
  breadth[i] <- grep("Breadth Requirements", other_course_information, value = TRUE) %>% paste(collapse = " ")
  hours[i] <- grep("Hours", other_course_information, value = TRUE) %>% paste(collapse = " ")
  distribution[i] <- grep("Distribution Requirements", other_course_information, value = TRUE) %>% paste(collapse = " ")
  delivery[i] <- grep("Mode of Delivery", other_course_information, value = TRUE) %>% paste(collapse = " ")
  antireq[i] <- grep("Exclusion", other_course_information, value = TRUE) %>% paste(collapse = " ")
  recommended[i] <- grep("Recommended Preparation", other_course_information, value = TRUE) %>% paste(collapse = " ")
  other[i] <- grep("Prerequisite|Corequisite|Breadth Requirements|Hours|Distribution Requirements|Hours|Mode of Delivery|Exclusion|Recommended Preparation",
                   other_course_information, value = TRUE, invert = TRUE) %>% paste(collapse = " ")
  i <- i + 1
}

required_courses <- data.frame(course_codes, course_names, course_descriptions, prereq, coreq, antireq, recommended, breadth, delivery, distribution, hours, other) %>%
  filter(!course_codes == "Sorry, this course is not in the current Calendar.")
