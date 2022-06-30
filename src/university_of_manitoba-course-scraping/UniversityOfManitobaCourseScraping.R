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

course_description <- vector(mode = "character", length = length(course_info_links))
prereq_and_coreq <- vector(mode = "character", length = length(course_info_links))
coreq <- vector(mode = "character", length = length(course_info_links))
attributes <- vector(mode = "character", length = length(course_info_links))
equiv <- vector(mode = "character", length = length(course_info_links))
exclusive <- vector(mode = "character", length = length(course_info_links))

i <- 1
for(item in course_info_links){
  course_link <- paste0(courses_main_link, course_info_links[i])
  course_information <- read_html(course_link) %>% html_nodes(".courseblockextra") %>% html_text() %>% str_squish()
  course_description[i] <- course_information[1]
  equiv[i] <- paste(grep("Equiv To:", course_information, value = TRUE), collapse = "")
  exclusive[i] <- paste(grep("Mutually Exclusive:", course_information, value = TRUE), collapse = "")
  attributes[i] <- paste(grep("Attributes:", course_information, value = TRUE), collapse = "")
  prereq[i] <- grep("PR/CR", course_information, value = TRUE) %>% paste(collapse = "") %>%
    sub("(.*)(Prerequisite.*)", "\\2", .)
  coreq[i] <- grep("PR/CR", course_information, value = TRUE) %>% paste(collapse = "") %>%
    sub("(.*)(Co-requisite.*)", "\\2", .)
  i = i + 1
}

course_info_test <- read_html(paste0(courses_main_link, course_info_links[8])) %>% html_nodes(".courseblockextra") %>% html_text() %>% str_squish()

item <- grep("PR/CR", course_info_test, value = TRUE) %>% paste(collapse = "") 

prereq_test <- sub("(.*?)(Prerequisite.*){1}(Co-requisite.*){1}", "\\1", item)

course_info <- data.frame(course_codes, course_description, equiv, exclusive, attributes, prereq, coreq)

information <- html_nodes(web_page, "#degreerequirementstextcontainer .codecol") %>% html_text()

program_table <- html_nodes(web_page, "#degreerequirementstextcontainer > table.sc_plangrid") %>% html_table() %>% .[[1]]

course_names <- html_elements(web_page, xpath = "//*[@id=\"degreerequirementstextcontainer\"]/table/tr/td/text()") %>% html_text()
