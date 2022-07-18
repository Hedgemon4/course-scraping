# Program Requirement Scraping for Wilfrid Laurier University ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Course Information ####
calendar_link <-
  "https://academic-calendar.wlu.ca/program.php?cal=1&d=2589&p=5763&s=1034&y=85"
calendar_page <- read_html(calendar_link)

link_text <- html_nodes(calendar_page, "a") %>% html_text()
links <- html_nodes(calendar_page, "a") %>% html_attr("href")
course_indices <-
  grep("(CP|ST|MA|DATA|EC|BU|ENTR)([0-9]{3})", link_text)
course_codes <- link_text[course_indices] %>% unique()
course_links <- links[course_indices] %>% unique()

closeAllConnections()

calendar_main_link <- "https://academic-calendar.wlu.ca/"

num_courses <- length(course_codes)
course_name <- vector("character", num_courses)
course_description <- vector("character", num_courses)
credit_amount <- vector("numeric", num_courses)
antireq <- vector("character", num_courses)
coreq <- vector("character", num_courses)
prereq <- vector("character", num_courses)
lecture <- vector("logical", num_courses)
lab <- vector("logical", num_courses)
tutorial <- vector("logical", num_courses)
note <- vector("character", num_courses)
hours <- vector("character", num_courses)
test <- vector("character", num_courses)

i <- 1
for (item in course_links) {
  course_page <- read_html(paste0(calendar_main_link, item))
  course_name[i] <-
    html_nodes(course_page, "span:nth-child(2)") %>% html_text()
  credit_amount[i] <-
    html_nodes(course_page, "span~ span") %>% html_text() %>% .[[1]] %>%
    str_extract("(([0-9]{1})(\\.)([0-9]{1}))") %>% as.numeric()
  course_description[i] <-
    html_nodes(course_page, "p") %>% html_text() %>% paste(collapse = "")  %>%
    str_squish()
  if (course_description[i] == "")
    course_description[i] <-
    html_nodes(course_page, ".content div span") %>% html_text() %>% paste(collapse = "")
  hours[i] <- html_nodes(course_page, ".hours") %>% html_text()
  lecture[i] <- grepl("Lecture", hours[i])
  lab[i] <- grepl("Lab", hours[i])
  tutorial[i] <- grepl("Tutorial", hours[i])
  other_info <-
    html_nodes(course_page, "#main > div.content > div.reqs > dl") %>%
    html_text() %>% paste("") %>% str_split("\\.|\\[") %>% unlist()
  prereq[i] <-
    grep("Prerequisites", other_info, value = TRUE) %>% paste0("") %>% str_remove("Prerequisites")
  antireq[i] <-
    grep("Exclusions", other_info, value = TRUE) %>% paste0("") %>% str_remove("Exclusions")
  coreq[i] <-
    grep("Co\\-requisites", other_info, value = TRUE) %>% paste0("") %>% str_remove("Co\\-requisites")
  note[i] <-
    grep("Note(s|:)", other_info, value = TRUE) %>% paste0("") %>% str_remove("Note(s|:)") %>%
    str_squish()
  i <- i + 1
}

credit_amount <- credit_amount * 6

course_information <-
  data.frame(
    course_codes,
    course_name,
    course_description,
    credit_amount,
    antireq,
    coreq,
    prereq,
    hours,
    lecture,
    lab,
    tutorial,
    note
  )
colnames(course_information) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Hours",
    "Lecture",
    "Lab",
    "Tutorial",
    "Note"
  )

# Requirement Information ####
requirement_link <-
  "https://students.wlu.ca/programs/science/data-science/program-requirements.html"
requirement_page <- read_html(requirement_link)

requirements <-
  html_nodes(requirement_page, "ul:nth-child(4) li") %>% html_text()

credits_replaced <- str_replace_all(requirements, "([0-9]{1}\\.[0-9]{1})", )

                                    