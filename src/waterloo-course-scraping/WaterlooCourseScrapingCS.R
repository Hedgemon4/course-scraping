# Course Scraping Waterloo Data Science Requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# TODO List ####
# TODO: Find a better way to scrape credits

# Functions ####
seperate_information <-
  function(match_item,
           column_name,
           web_link,
           node,
           course_dataframe,
           vector_type,
           break_if_match) {
    information <-
      read_html(web_link) %>% html_nodes(node) %>% html_text() %>% str_squish() %>% stri_remove_empty()
    columns <- list()
    
    i = 1
    for (item in match_item) {
      columns[[i]] <-
        vector(mode = vector_type, length = nrow(course_dataframe))
      i = i + 1
    }
    
    i <- 1
    for (item in match_item) {
      if (vector_type == "logical") {
        columns[[i]] <- grepl(item, information)
      } else{
        columns[[i]] <- grep(item, information, value = TRUE)
      }
      i = i + 1
    }
    
    seperate <- data.frame(columns)
    colnames(seperate) <- column_name
    course_dataframe <- cbind.data.frame(course_dataframe, seperate)
    return(course_dataframe)
  }

get_other_course_info <- function(link, n) {
  # This function reads the other course information from the academic calendar,
  # including prerequisites, notes, and others, and returns a data frame with this
  # information in it.
  
  # n = number of courses on course calendar
  i <- 1
  prereq <- vector(mode = "character", length = n)
  antireq <- vector(mode = "character", length = n)
  coreq <- vector(mode = "character", length = n)
  note <- vector(mode = "character", length = n)
  other <- vector(mode = "character", length = n)
  web_page <- read_html(link)
  while (i <= n) {
    course_information <-
      html_elements(web_page,
                    xpath = paste0("/html/body/main/center[", i, "]/div/div/em")) %>% html_text() %>% stri_remove_empty()
    prereq[i] <-
      paste(grep("Prereq:", course_information, value = TRUE),
            collapse = "")
    antireq[i] <-
      paste(grep("Antireq:", course_information, value = TRUE),
            collapse = "")
    coreq[i] <-
      paste(grep("Coreq:", course_information, value = TRUE), collapse = "")
    note[i] <-
      paste(grep("Note:", course_information, value = TRUE), collapse = "")
    other[i] <- paste(
      grep(
        "Prereq:|Antireq:|Coreq:|Note:",
        course_information,
        value = TRUE,
        invert = TRUE
      ),
      collapse = ""
    )
    i = i + 1
  }
  return(data.frame(prereq, antireq, coreq, note, other))
}

# Program Requirements ####
web_link <-
  "http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1"

web_page <- read_html("http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1")
  
waterloo_course_requirements <-
  get_text_dataframe(web_link,
                       "#ctl00_contentMain_lblContent ul ul a")

colnames(waterloo_course_requirements) <- "Course Code"

# Category Requirements ####

category_description <- get_text_xpath(web_link, "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li/text()")

number_of_categories <- length(category_description)

number_of_courses <- nrow(waterloo_course_requirements)

category_names <- get_item_vector("Category", number_of_categories)

course_code <-
  vector(mode = "character", length = number_of_courses)

course_category <-
  vector(mode = "character", length = number_of_courses)

course_category_description <-
  vector(mode = "character", length = number_of_courses)

i <- 1
j <- 1
while (i <= number_of_categories) {
  courses_in_category <-
    read_html <- html_nodes(
      web_page,
      paste0(
        "#ctl00_contentMain_lblContent > ul > li:nth-child(",
        i,
        ") > ul > li > a"
      )
    ) %>% html_text() %>% str_squish()
  for (course in courses_in_category) {
    course_code[j] <- course
    course_category[j] <- category_names[i]
    course_category_description[j] <- category_description[i]
    j = j + 1
  }
  k <- 0
  i = i + 1
}

course_categories <-
  data.frame(course_code,
             course_category,
             course_category_description)
colnames(course_categories) <-
  c("Course Code", "Category", "Category Requirement")

waterloo_course_requirements <- course_categories

# Course Information ####

cs_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(cs_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

math_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(math_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

stat_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(stat_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

# Clean Data
clean_from_data <-
  "LEC|LAB|\\,|TST|TUT|0\\.50|PRJ|RDG|STU|0\\.25|0\\.00|2\\.50"
cs_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", cs_courses_waterloo$`Course Code`) %>% str_squish()
math_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", math_courses_waterloo$`Course Code`) %>% str_squish()
stat_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", stat_courses_waterloo$`Course Code`) %>% str_squish()

# Get other course information

other_info_column_names <-
  c("Prerequisite",
    "Antirequisite",
    "Corequisite",
    "Note",
    "Other Information")

math_other_course_info <-
  get_other_course_info("http://ugradcalendar.uwaterloo.ca/courses/MATH",
                        nrow(math_courses_waterloo))
colnames(math_other_course_info) <- other_info_column_names
math_courses_waterloo <-
  cbind(math_courses_waterloo, math_other_course_info)

cs_other_course_info <-
  get_other_course_info("http://ugradcalendar.uwaterloo.ca/courses/CS",
                        nrow(cs_courses_waterloo))
colnames(cs_other_course_info) <- other_info_column_names
cs_courses_waterloo <-
  cbind(cs_courses_waterloo, cs_other_course_info)

stat_other_course_info <-
  get_other_course_info("http://ugradcalendar.uwaterloo.ca/courses/STAT",
                        nrow(stat_courses_waterloo))
colnames(stat_other_course_info) <- other_info_column_names
stat_courses_waterloo <-
  cbind(stat_courses_waterloo, stat_other_course_info)

# Scrape Course Component Data
course_component <-
  c("LEC", "LAB", "TST", "TUT", "PRJ", "RDG", "STU")

course_component_name <-
  c("Lecture",
    "Lab",
    "Test Slot",
    "Tutorial",
    "Project",
    "Reading",
    "Studio")

cs_courses_waterloo <-
  seperate_information(
    course_component,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".divTableCell:nth-child(1) strong",
    cs_courses_waterloo,
    "logical",
    FALSE
  )

math_courses_waterloo <-
  seperate_information(
    course_component,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".divTableCell:nth-child(1) strong",
    math_courses_waterloo,
    "logical",
    FALSE
  )

stat_courses_waterloo <-
  seperate_information(
    course_component,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".divTableCell:nth-child(1) strong",
    stat_courses_waterloo,
    "logical",
    FALSE
  )

# Scrape Credit Data
# TODO: Find a better way to scrape credits
# credit_value <- "0.00|0.25|0.50|2.50"
# name <- c("Credit Amount")
# 
# cs_courses_waterloo <-
#   seperate_information(
#     credit_value,
#     name,
#     "http://ugradcalendar.uwaterloo.ca/courses/CS",
#     ".divTableCell:nth-child(1) strong",
#     cs_courses_waterloo,
#     "character",
#     TRUE
#   )
# 
# math_courses_waterloo <-
#   seperate_information(
#     credit_value,
#     name,
#     "http://ugradcalendar.uwaterloo.ca/courses/MATH",
#     ".divTableCell:nth-child(1) strong",
#     math_courses_waterloo,
#     "character",
#     TRUE
#   )
# 
# stat_courses_waterloo <-
#   seperate_informatio(
#     credit_value,
#     name,
#     "http://ugradcalendar.uwaterloo.ca/courses/STAT",
#     ".divTableCell:nth-child(1) strong",
#     stat_courses_waterloo,
#     "character",
#     TRUE
#   )

# Merge Data ####
cs <-
  merge(
    waterloo_course_requirements,
    cs_courses_waterloo,
    by = c("Course Code")
  )

math <-
  merge(
    waterloo_course_requirements,
    math_courses_waterloo,
    by = c("Course Code")
  )

stat <-
  merge(
    waterloo_course_requirements,
    stat_courses_waterloo,
    by = c("Course Code")
  )

courses <- rbind(cs, math, stat)

requirements <- rbind(math, cs, stat)
requirements <-
  requirements[, c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Category",
    "Category Requirement",
    "Prerequisite",
    "Antirequisite",
    "Corequisite",
    "Note",
    "Other Information",
    "Lecture",
    "Lab",
    "Test Slot",
    "Tutorial",
    "Project",
    "Reading",
    "Studio"
  )]

View(requirements)
