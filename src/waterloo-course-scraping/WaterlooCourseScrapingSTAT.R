# Course Scraping Waterloo Data Science Requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Source for Utility functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# TODO List ####
# TODO: Spell checking library if needed (stringdist)

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

# Get Course Requirements ####

web_link <-
  "http://ugradcalendar.uwaterloo.ca/page/MATH-Statistics1"

waterloo_course_requirements <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/page/MATH-Statistics1",
    "#ctl00_contentMain_lblContent ul ul a"
  )

number_of_courses <- nrow(waterloo_course_requirements)

colnames(waterloo_course_requirements) <- c("Course Code")

# Get Categories ####

# Pull strings for categories (like requirement v on degree navigator)

# Html Nodes for the categories
category_html_nodes <-
  read_html(web_link) %>% html_elements(xpath = "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li")

number_of_categories <- length(category_html_nodes)

# Descriptions of Categories
category_description <-
  vector(mode = "character", length = number_of_categories)

# Gets descriptions of categories base on if they have sublists or not
i <- 1
web_page <- read_html(web_link)
for (item in category_html_nodes) {
  if (grepl("ul", item))
    category_description[i] <-
      html_elements(
        web_page,
        xpath =  paste0(
          "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li[",
          i,
          "]/text()"
        )
      ) %>% html_text() %>% str_squish()
  else
    category_description[i] <-
      read_html(web_link) %>% html_elements(xpath = paste0("//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li[", i, "]")) %>% html_text() %>% str_squish()
  i = i + 1
}

category_names <- get_item_vector("Category", number_of_categories)

course_category <-
  vector(mode = "character", length = number_of_courses)

course_category_description <-
  vector(mode = "character", length = number_of_courses)

category_is_used <-
  vector(mode = "logical", length = number_of_categories)

course_code <-
  vector(mode = "character", length = number_of_courses)

general_requirement_description <- vector(mode = "character")
general_requirement_index <- vector(mode = "numeric")

# Loop gets course codes under each category, and puts their course code,
# requirement category, and category into vectors. Also keeps track of which
# categories are used, and fetches the description for any general requirements

i <- 1
j <- 1
k <- 0

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
    k <- k + 1
    if (!grepl("[0-9]", course)) {
      general_requirement_description[length(general_requirement_description) + 1] <-
        read_html(web_link) %>% html_node(
          paste0(
            "#ctl00_contentMain_lblContent > ul > li:nth-child(" ,
            i,
            ") > ul > li:nth-child(",
            k,
            ")"
          )
        ) %>% html_text() %>% str_squish()
      general_requirement_index <- j - 1
    }
    course_code[j] <- course
    course_category[j] <- category_names[i]
    course_category_description[j] <- category_description[i]
    category_is_used[i] <- TRUE
    j = j + 1
  }
  k <- 0
  i = i + 1
}

# Creates a data frame using the course category information fetched above

course_categories <-
  data.frame(course_code,
             course_category,
             course_category_description)
colnames(course_categories) <-
  c("Course Code", "Category", "Category Requirement")

# Merges original

waterloo_course_requirements <-
  merge(waterloo_course_requirements, course_categories, by = "Course Code")

# Get Course Information ####

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

engl_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/ENGL",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(engl_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

amath_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/AMATH",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(amath_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

mthel_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/MTHEL",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(mthel_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

# Clean Data (Removes excess information from course code)
clean_from_data <-
  "LEC|LAB|\\,|TST|TUT|WSP|ESS|DIS|0\\.50|PRJ|RDG|STU|0\\.25|0\\.00|2\\.50|SEM"
cs_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", cs_courses_waterloo$`Course Code`) %>% str_squish()
math_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", math_courses_waterloo$`Course Code`) %>% str_squish()
stat_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", stat_courses_waterloo$`Course Code`) %>% str_squish()
engl_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", engl_courses_waterloo$`Course Code`) %>% str_squish()
amath_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", amath_courses_waterloo$`Course Code`) %>% str_squish()
mthel_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", mthel_courses_waterloo$`Course Code`) %>% str_squish()

# Get other information using html codes
# TODO: Change to use get_text_xpath(cs_link, "/html/body/main/center[8]/div/div/em")

other_info_column_names <-
  c("Prerequisite",
    "Antirequisite",
    "Corequisite",
    "Note",
    "Other Information")

amath_other_course_info <-
  get_other_course_info("http://ugradcalendar.uwaterloo.ca/courses/AMATH",
                        nrow(amath_courses_waterloo))
colnames(amath_other_course_info) <- other_info_column_names
amath_courses_waterloo <-
  cbind(amath_courses_waterloo, amath_other_course_info)

engl_other_course_info <-
  get_other_course_info("http://ugradcalendar.uwaterloo.ca/courses/ENGL",
                        nrow(engl_courses_waterloo))
colnames(engl_other_course_info) <- other_info_column_names
engl_courses_waterloo <-
  cbind(engl_courses_waterloo, engl_other_course_info)

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

mthel_other_course_info <-
  get_other_course_info("http://ugradcalendar.uwaterloo.ca/courses/MTHEL",
                        nrow(mthel_courses_waterloo))
colnames(mthel_other_course_info) <- other_info_column_names
mthel_courses_waterloo <-
  cbind(mthel_courses_waterloo, mthel_other_course_info)

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

amath_courses_waterloo <-
  seperate_information(
    course_component,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/AMATH",
    ".divTableCell:nth-child(1) strong",
    amath_courses_waterloo,
    "logical",
    FALSE
  )

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

engl_courses_waterloo <-
  seperate_information(
    course_component,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/ENGL",
    ".divTableCell:nth-child(1) strong",
    engl_courses_waterloo,
    "logical",
    FALSE
  )

mthel_courses_waterloo <-
  seperate_information(
    course_component,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/MTHEL",
    ".divTableCell:nth-child(1) strong",
    mthel_courses_waterloo,
    "logical",
    FALSE
  )

amath <-
  merge(waterloo_course_requirements, amath_courses_waterloo, by = "Course Code")
math <-
  merge(waterloo_course_requirements, math_courses_waterloo, by = "Course Code")
cs <-
  merge(waterloo_course_requirements, cs_courses_waterloo, by = "Course Code")
stat <-
  merge(waterloo_course_requirements, stat_courses_waterloo, by = "Course Code")
engl <-
  merge(waterloo_course_requirements, engl_courses_waterloo, by = "Course Code")
mthel <-
  merge(waterloo_course_requirements, mthel_courses_waterloo, by = "Course Code")
# Reorganize Information ####
requirements <- rbind(amath, math, cs, stat, engl, mthel)
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

# General Requirements ####
# Adding of general requirements/categories to the course dataframe

i <- 1
for (item in general_requirement_index) {
  general_requirement <- rep(NA, ncol(requirements))
  general_requirement[1] <-
    waterloo_course_requirements$`Course Code`[item]
  general_requirement[2] <- "Other"
  general_requirement[3] <- general_requirement_description[i]
  general_requirement[4] <-
    waterloo_course_requirements$Category[item]
  general_requirement[5] <-
    waterloo_course_requirements$`Category Requirement`[item]
  requirements <- rbind(requirements, general_requirement)
  i <- i + 1
}

i <- 1
for (item in category_is_used) {
  if (!item) {
    other_category <- rep(NA, ncol(requirements))
    other_category[1] <- "Category Requirement"
    other_category[2] <- "Other Requirement"
    other_category[3] <- category_description[i]
    other_category[4] <- category_names[i]
    other_category[5] <- category_description[i]
    requirements <- rbind(requirements, other_category)
  }
  i <- i + 1
}
