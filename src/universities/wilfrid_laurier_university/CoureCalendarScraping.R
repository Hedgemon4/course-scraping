# Course Calendar Scraping for Wilfrid Laurier University

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Business Courses ####

business_link <-
  curl(
    "https://academic-calendar.wlu.ca/department.php?cal=1&d=2617&s=1036&y=85#Course_Offerings",
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  )
business_page <- read_html(business_link)

business_table <-
  html_nodes(business_page, "#main > div.content > div > table") %>%
  html_table() %>% .[[1]]
colnames(business_table) <-
  c("Course Code", "Course Name", "Credit Amount")

business_course_links <-
  html_nodes(business_page, "#main > div.content > div > table > tr > td > a") %>%
  html_attr("href")

num_business_courses <- nrow(business_table)
hours <- vector(mode = "character", num_business_courses)
description <- vector(mode = "character", num_business_courses)
course_information <-
  vector(mode = "character", num_business_courses)
lecture <- vector(mode = "logical", num_business_courses)
lab <- vector(mode = "logical", num_business_courses)
tutorial <- vector(mode = "logical", num_business_courses)
antireq <- vector(mode = "character", num_business_courses)
coreq <- vector(mode = "character", num_business_courses)
prereq <- vector(mode = "character", num_business_courses)
note <- vector("character", num_business_courses)

i <- 1
while (i <= num_business_courses) {
  course_page <-
    read_html(curl(
      paste0(
        "https://academic-calendar.wlu.ca/",
        business_course_links[i]
      ),
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    ))
  hours[i] <-
    html_nodes(course_page, "#main > div.content > div.hours") %>% html_text() %>% paste0("") %>% str_squish()
  lecture[i] <- grepl("Lecture", hours[i])
  lab[i] <- grepl("Lab", hours[i])
  tutorial[i] <- grepl("Tutorial", hours[i])
  description[i] <-
    html_nodes(course_page, "#main > div.content > p") %>% html_text %>% paste0(collapse = "") %>% str_squish()
  other_info <-
    html_nodes(course_page, "#main > div.content > div.reqs > dl") %>%
    html_text() %>% str_squish() %>% str_split("\\.") %>% unlist()
  prereq[i] <-
    grep("Prerequisites", other_info, value = TRUE) %>% paste0(collapse = "") %>% str_remove("Prerequisites")
  antireq[i] <-
    grep("Exclusions", other_info, value = TRUE) %>% paste0(collapse = "") %>% str_remove("Exclusions")
  coreq[i] <-
    grep("Co\\-requisites", other_info, value = TRUE) %>% paste0(collapse = "") %>% str_remove("Co\\-requisites")
  note[i] <-
    grep("Note(s|:)", other_info, value = TRUE) %>% paste0(collapse = "") %>% str_remove("Note(s|:)") %>%
    str_squish()
  i <- i + 1
}

closeAllConnections()
business_credits <-
  select(business_table, `Credit Amount`) %>% unlist() %>% unname() * 6

business_calendar <-
  cbind(
    business_table$`Course Code`,
    business_table$`Course Name`,
    description,
    business_credits,
    antireq,
    coreq,
    prereq,
    hours,
    lecture,
    lab,
    tutorial,
    note
  )

colnames(business_calendar) <-  c(
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
