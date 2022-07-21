# Concordia University Data Science Program Requirement Scraping ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# TODO: Remove False from notes column

# Math and Stat Courses ####
math_stat_course_link <-
  "https://www.concordia.ca/academics/undergraduate/calendar/current/section-31-faculty-of-arts-and-science/section-31-200-department-of-mathematics-and-statistics/mathematics-and-statistics-courses.html#2565"
math_stat_course_page <- read_html(math_stat_course_link)

math_stat_courses <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.group.panel.xlarge > h3"
  ) %>%
  html_text()

math_stat_num_courses <- length(math_stat_courses)
math_stat_course_description <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p.crse-descr"
  ) %>%
  html_text()

math_stat_course_code <-
  str_extract_all(math_stat_courses,
                  "((?:ACTU|MACF|MATH|MAST|STAT)\\s[0-9]{3})") %>%
  unlist()
math_stat_course_name <-
  gsub(
    "((?:ACTU|MACF|MATH|MAST|STAT)\\s[0-9]{3})(.*)(\\([0-9]?\\scredits\\))",
    "\\2",
    math_stat_courses
  ) %>%
  str_squish()
math_stat_course_credit_amount <-
  gsub(
    "((?:ACTU|MACF|MATH|MAST|STAT)\\s[0-9]{3})(.*)(?:\\(([0-9]?)\\scredits\\))",
    "\\3",
    math_stat_courses
  ) %>%
  str_squish() %>% as.numeric()

course_info <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div"
  ) %>%
  html_text()

course_components <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p > span.components"
  ) %>%
  html_text() %>% str_squish()
course_requisites <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p > span.requisites"
  ) %>%
  html_text() %>% str_squish()
course_notes <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > ul"
  ) %>%
  html_text() %>% str_squish()

has_requisites <- grepl("Prerequisite\\/Corequisite:", course_info)
has_components <- grepl("Component\\(s\\):", course_info)
has_notes <- grepl("Notes:", course_info)

antireq <- vector(mode = "character", math_stat_num_courses)
coreq <- vector(mode = "character", math_stat_num_courses)
prereq <- vector(mode = "character", math_stat_num_courses)
hours <- vector(mode = "character", math_stat_num_courses)
lecture <- vector(mode = "logical", math_stat_num_courses)
lab <- vector(mode = "logical", math_stat_num_courses)
tutorial <- vector(mode = "logical", math_stat_num_courses)
research <- vector(mode = "logical", math_stat_num_courses)
reading <- vector(mode = "logical", math_stat_num_courses)
notes <- vector(mode = "logical", math_stat_num_courses)

i <- 1
req_index <- 1
comp_index <- 1
note_index <- 1
while (i <= math_stat_num_courses) {
  if (has_requisites[i]) {
    item <-
      course_requisites[req_index] %>% str_split("\\.") %>% unlist()
    prereq[i] <-
      grep("course(s)?\\smust\\sbe\\scompleted\\spreviously:",
           item,
           value = TRUE) %>%
      paste(collapse = " ")
    coreq[i] <-
      grep("previously\\sor\\sconcurrently:", item, value = TRUE) %>%
      paste(collapse = " ")
    req_index <- req_index + 1
  }
  if (has_components[i]) {
    item <- course_components[comp_index]
    lecture[i] <- grepl("Lecture", item)
    lab[i] <- grepl("Laboratory", item)
    tutorial[i] <- grepl("Tutorial", item)
    research[i] <- grepl("Research", item)
    reading[i] <- grepl("Reading", item)
    comp_index <- comp_index + 1
  }
  if (has_notes[i]) {
    item <- course_notes[note_index] %>% str_split("\\.") %>% unlist()
    antireq[i] <-
      grep("(receive(d)?\\scredit)", item, value = TRUE) %>%
      paste(collapse = " ")
    notes[i] <-
      grep("(receive(d)?\\scredit)",
           item,
           value = TRUE,
           invert = TRUE) %>%
      paste(collapse = " ")
    note_index <- note_index + 1
  }
  i <- i + 1
}

math_stat_course_dataframe <-
  data.frame(
    math_stat_course_code,
    math_stat_course_name,
    math_stat_course_description,
    math_stat_course_credit_amount,
    antireq,
    coreq,
    prereq,
    lecture,
    lab,
    tutorial,
    reading,
    research,
    notes
  )


# Computer Science and Software Engineering Courses ####
compsci_course_link <- "https://www.concordia.ca/academics/undergraduate/calendar/current/section-71-gina-cody-school-of-engineering-and-computer-science/section-71-70-department-of-computer-science-and-software-engineering/section-71-70-10-computer-science-and-software-engineering-courses.html#3527"
compsci_course_page <- read_html(compsci_course_link)

compsci_courses <-
  html_nodes(
    compsci_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.group.panel.xlarge > h3"
  ) %>%
  html_text()

compsci_num_courses <- length(compsci_courses)
compsci_course_description <-
  html_nodes(
    compsci_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p.crse-descr"
  ) %>%
  html_text()

compsci_course_code <-
  str_extract_all(compsci_courses,
                  "((?:COMP|SOEN)\\s[0-9]{3})") %>%
  unlist()
compsci_course_name <-
  gsub(
    "((?:COMP|SOEN)\\s[0-9]{3})(.*)(\\([0-9]?(?:\\.[0-9])?\\scredits\\))",
    "\\2",
    compsci_courses
  ) %>%
  str_squish()
compsci_course_credit_amount <-
  gsub(
    "((?:COMP|SOEN)\\s[0-9]{3})(.*)(?:\\(([0-9]?(?:\\.[0-9])?)\\scredits\\))",
    "\\3",
    compsci_courses
  ) %>%
  str_squish() %>% as.numeric()

course_info <-
  html_nodes(
    compsci_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div"
  ) %>%
  html_text()

course_components <-
  html_nodes(
    compsci_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p > span.components"
  ) %>%
  html_text() %>% str_squish()
course_requisites <-
  html_nodes(
    compsci_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p > span.requisites"
  ) %>%
  html_text() %>% str_squish()
course_notes <-
  html_nodes(
    compsci_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > ul"
  ) %>%
  html_text() %>% str_squish()

has_requisites <- grepl("Prerequisite\\/Corequisite:", course_info)
has_components <- grepl("Component\\(s\\):", course_info)
has_notes <- grepl("Notes:", course_info)

antireq <- vector(mode = "character", compsci_num_courses)
coreq <- vector(mode = "character", compsci_num_courses)
prereq <- vector(mode = "character", compsci_num_courses)
hours <- vector(mode = "character", compsci_num_courses)
lecture <- vector(mode = "logical", compsci_num_courses)
lab <- vector(mode = "logical", compsci_num_courses)
tutorial <- vector(mode = "logical", compsci_num_courses)
research <- vector(mode = "logical", compsci_num_courses)
reading <- vector(mode = "logical", compsci_num_courses)
notes <- vector(mode = "logical", compsci_num_courses)

i <- 1
req_index <- 1
comp_index <- 1
note_index <- 1
while (i <= compsci_num_courses) {
  if (has_requisites[i]) {
    item <-
      course_requisites[req_index] %>% str_split("\\.") %>% unlist()
    prereq[i] <-
      grep("course(s)?\\smust\\sbe\\scompleted\\spreviously:",
           item,
           value = TRUE) %>%
      paste(collapse = " ")
    coreq[i] <-
      grep("previously\\sor\\sconcurrently:", item, value = TRUE) %>%
      paste(collapse = " ")
    req_index <- req_index + 1
  }
  if (has_components[i]) {
    item <- course_components[comp_index]
    lecture[i] <- grepl("Lecture", item)
    lab[i] <- grepl("Laboratory", item)
    tutorial[i] <- grepl("Tutorial", item)
    research[i] <- grepl("Research", item)
    reading[i] <- grepl("Reading", item)
    comp_index <- comp_index + 1
  }
  if (has_notes[i]) {
    item <- course_notes[note_index] %>% str_split("\\.") %>% unlist()
    antireq[i] <-
      grep("(receive(d)?\\scredit)", item, value = TRUE) %>%
      paste(collapse = " ")
    notes[i] <-
      grep("(receive(d)?\\scredit)",
           item,
           value = TRUE,
           invert = TRUE) %>%
      paste(collapse = " ")
    note_index <- note_index + 1
  }
  i <- i + 1
}

compsci_course_dataframe <-
  data.frame(
    compsci_course_code,
    compsci_course_name,
    compsci_course_description,
    compsci_course_credit_amount,
    antireq,
    coreq,
    prereq,
    lecture,
    lab,
    tutorial,
    reading,
    research,
    notes
  )

# Program Requirements ####

program_link <-
  "https://www.concordia.ca/academics/undergraduate/calendar/current/section-31-faculty-of-arts-and-science/section-31-200-department-of-mathematics-and-statistics/ba-bsc-joint-major-in-data-science.html"
program_page <- read_html(program_link)

required_courses <- html_nodes(program_page, "#21945+ table .formatted-course") %>% 
  html_text()  

other_requirements <-
  html_nodes(program_page, ".joint-major-in-data-science tr+ tr td") %>%
  html_text() %>% str_squish()
