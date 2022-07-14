# Course Scraping Waterloo Data Science Requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# TODO List ####

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

# Course Calendars ####

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

cs_course_credits <- get_text_css(
  "http://ugradcalendar.uwaterloo.ca/courses/CS",
  ".divTableCell:nth-child(1) strong"
) %>% sub(".*([0-9]\\.[0-9]+)", "\\1", .) %>% str_squish() %>% as.numeric() * 6
cs_credit_datafram <- data.frame(cs_course_credits)
colnames(cs_credit_datafram) <- "Credit Amount"
cs_courses_waterloo <-
  cbind(cs_courses_waterloo, cs_credit_datafram)

math_course_credits <- get_text_css(
  "http://ugradcalendar.uwaterloo.ca/courses/MATH",
  ".divTableCell:nth-child(1) strong"
) %>% sub(".*([0-9]\\.[0-9]+)", "\\1", .) %>% str_squish() %>% as.numeric() * 6
math_credit_dataframe <- data.frame(math_course_credits)
colnames(math_credit_dataframe) <- "Credit Amount"
math_courses_waterloo <-
  cbind(math_courses_waterloo, math_credit_dataframe)

stat_course_credits <- get_text_css(
  "http://ugradcalendar.uwaterloo.ca/courses/STAT",
  ".divTableCell:nth-child(1) strong"
) %>% sub(".*([0-9]\\.[0-9]+)", "\\1", .) %>% str_squish() %>% as.numeric() * 6
stat_credit_dataframe <- data.frame(stat_course_credits)
colnames(stat_credit_dataframe) <- "Credit Amount"
stat_courses_waterloo <-
  cbind(stat_courses_waterloo, stat_credit_dataframe)

course_calendar <-
  rbind(cs_courses_waterloo,
        math_courses_waterloo,
        stat_courses_waterloo)

# Program Requirements ####

program_link <-
  "http://ugradcalendar.uwaterloo.ca/page/MATH-Data-Science1"
program_page <- read_html(program_link)

category_description <-
  html_elements(program_page, xpath = "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li/text()") %>% html_text() %>% str_squish()
num_categories <- length(category_description)
category <- vector(mode = "character", length = num_categories)
category_min <- vector(mode = "numeric", length = num_categories)
category_max <- vector(mode = "numeric", length = num_categories)
isCore <- vector(mode = "logical", length = num_categories)

i <- 1
for (item in category_description) {
  description <- paste0(item, ": ")
  courses_in_category <-
    html_nodes(
      program_page,
      paste0(
        "#ctl00_contentMain_lblContent > ul > li:nth-child(",
        i,
        ") > ul > li"
      )
    ) %>%
    html_text %>% str_squish() %>% str_extract_all("(CS|MATH|STAT).([0-9]{3})([A-Z]?)") %>% unlist() %>% str_squish()
  courses_collapsed <- paste(courses_in_category, collapse = ", ")
  category_description[i] <-
    paste0(description, courses_collapsed)
  category[i] <- paste0("Category G", i)
  credit <-
    filter(course_calendar, `Course Code` %in% courses_in_category) %>% select(`Credit Amount`) %>% .[[1]] %>% sort(decreasing = TRUE)
  sum_credit <- sum(credit)
  if (grepl("One.of", item)) {
    category_max[i] <- max(credit)
    category_min[i] <- min(credit)
  } else if (grepl("Two", item)) {
    category_max[i] <- credit[1] + credit[2]
    category_min[i] <- credit[length(credit)] + credit[length(credit) - 1]
  } else if (grepl("All.of", item)) {
    category_max[i] <- sum_credit
    category_min[i] <- sum_credit
  } else{
    category_max[i] <- -1.0
    category_min[i] <- -1.0
  }
  if (sum_credit == category_max[i] & sum_credit == category_min[i])
    isCore[i] = TRUE
  i <- i + 1
}

program_requirements <-
  data.frame(category,
             category_description,
             category_min,
             category_max,
             isCore)
colnames(program_requirements) <- c(
  "Requirement Category",
  "Category Description",
  "Category Minimum Credit Amount",
  "Category Maximum Credit Amount",
  "Core Course"
)

# Write CSV Files ####

# write.csv(program_requirements, "University of Waterloo Data Science Program Requirements.csv")
# write.csv(cs_courses_waterloo, "University of Waterloo Computer Science Course Calendar.csv")
# write.csv(math_courses_waterloo, "University of Waterloo Mathematics Course Calendar.csv")
# write.csv(stat_courses_waterloo, "University of Waterloo Statistics Course Calendar.csv")
