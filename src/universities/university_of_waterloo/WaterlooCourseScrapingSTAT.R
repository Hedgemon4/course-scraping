# Course Scraping Waterloo Statistics Requirements ####

# The statistics requirements are needed in addition to all the data science
# program requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Source for Utility functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Functions ####
seperate_information <-
  function(match_item,
           column_name,
           web_link,
           node,
           course_dataframe,
           vector_type,
           break_if_match) {
    # This takes information scraped into a single vector or text block and separates
    # it out into multiple vectors depending on the parameters specified
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

# Scrapes the course calendars for any subjects which contain courses required
# for the statistics major

# Generates a data frame for the course calendars of the required subjects, and
# gets the course codes, names, and descriptions for those courses

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

other_info_column_names <-
  c("Prerequisite",
    "Antirequisite",
    "Corequisite",
    "Note",
    "Other Information")

# Gets other course information from course calendars (prereq, antireq, etc.)
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

# Gets course component data (lectures, labs, etc.)

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

# Gets course credit amounts and standarizes the values to the same scale used
# at UBC

cs_course_credits <- get_text_css(
  "http://ugradcalendar.uwaterloo.ca/courses/CS",
  ".divTableCell:nth-child(1) strong"
) %>% sub(".*([0-9]\\.[0-9]+)", "\\1", .) %>% as.numeric() * 6
cs_credit_dataframe <- data.frame(cs_course_credits)
colnames(cs_credit_dataframe) <- "Credit Amount"
cs_courses_waterloo <-
  cbind(cs_courses_waterloo, cs_credit_dataframe)

math_course_credits <- get_text_css(
  "http://ugradcalendar.uwaterloo.ca/courses/MATH",
  ".divTableCell:nth-child(1) strong"
) %>% sub(".*([0-9]\\.[0-9]+)", "\\1", .) %>% as.numeric() * 6
math_credit_dataframe <- data.frame(math_course_credits)
colnames(math_credit_dataframe) <- "Credit Amount"
math_courses_waterloo <-
  cbind(math_courses_waterloo, math_credit_dataframe)

stat_course_credits <- get_text_css(
  "http://ugradcalendar.uwaterloo.ca/courses/STAT",
  ".divTableCell:nth-child(1) strong"
) %>% sub(".*([0-9]\\.[0-9]+)", "\\1", .) %>% as.numeric() * 6
stat_credit_dataframe <- data.frame(stat_course_credits)
colnames(stat_credit_dataframe) <- "Credit Amount"
stat_courses_waterloo <-
  cbind(stat_courses_waterloo, stat_credit_dataframe)

engl_course_credits <- get_text_css(
  "http://ugradcalendar.uwaterloo.ca/courses/ENGL",
  ".divTableCell:nth-child(1) strong"
) %>% sub(".*([0-9]\\.[0-9]+)", "\\1", .) %>% as.numeric() * 6
engl_credit_dataframe <- data.frame(engl_course_credits)
colnames(engl_credit_dataframe) <- "Credit Amount"
engl_courses_waterloo <-
  cbind(engl_courses_waterloo, engl_credit_dataframe)

mthel_course_credits <- get_text_css(
  "http://ugradcalendar.uwaterloo.ca/courses/MTHEL",
  ".divTableCell:nth-child(1) strong"
) %>% sub(".*([0-9]\\.[0-9]+)", "\\1", .) %>% as.numeric() * 6
mthel_credit_dataframe <- data.frame(mthel_course_credits)
colnames(mthel_credit_dataframe) <- "Credit Amount"
mthel_courses_waterloo <-
  cbind(mthel_courses_waterloo, mthel_credit_dataframe)

amath_course_credits <- get_text_css(
  "http://ugradcalendar.uwaterloo.ca/courses/AMATH",
  ".divTableCell:nth-child(1) strong"
) %>% sub(".*([0-9]\\.[0-9]+)", "\\1", .) %>% as.numeric() * 6
amath_credit_dataframe <- data.frame(amath_course_credits)
colnames(amath_credit_dataframe) <- "Credit Amount"
amath_courses_waterloo <-
  cbind(amath_courses_waterloo, amath_credit_dataframe)

# Course calendar combining all the different subject calendars scraped
course_calendar <-
  rbind(
    cs_courses_waterloo,
    math_courses_waterloo,
    stat_courses_waterloo,
    engl_courses_waterloo,
    mthel_courses_waterloo,
    amath_courses_waterloo
  )

# Program Requirements ####

# Reads webpage for statistics program requirement
program_link <-
  "http://ugradcalendar.uwaterloo.ca/page/MATH-Statistics1"
program_page <- read_html(program_link)

# Scrapes category descriptions
category_description <-
  html_elements(program_page, "#ctl00_contentMain_lblContent > ul > li") %>% html_text() %>% str_squish()

# Course codes in each category
subcategory_items <-
  html_elements(program_page, "#ctl00_contentMain_lblContent > ul > li > ul > li") %>% html_text() %>% str_squish()

# Removes excess information (course name, code, etc.) from category descriptions
for (item in subcategory_items) {
  category_description <-
    str_remove_all(category_description, fixed(item)) %>% str_squish()
}

num_categories <- length(category_description)

category <- vector(mode = "character", length = num_categories)
category_min <- vector(mode = "numeric", length = num_categories)
category_max <- vector(mode = "numeric", length = num_categories)
isCore <- vector(mode = "logical", length = num_categories)

# This loop works by parsing through each category description, scraping any
# other relevant information for the category, and using regex to sort and clean
# the data into the correct vectors

# Tracks which index of vector the loop is using, and in generating category
# name numbering
i <- 1

other_requirements <- ""
description <- ""

for (item in category_description) {
  # Get category requirements (courses)
  category_requirements <-
    html_nodes(
      program_page,
      paste0(
        "#ctl00_contentMain_lblContent > ul > li:nth-child(",
        i,
        ") > ul > li"
      )
    ) %>%
    html_text %>% str_squish()
  # Creates category description
  description <- paste0(item, ": ")
  other_requirements <-
    grep(
      "((O|o)ne)|((T|t)wo)|((T|t)hree)|((F|f)our)|((A|a)ll)",
      category_requirements,
      value = TRUE
    ) %>% paste(collapse = ", ")
  # Uses regex to get all the courses in the current requirement
  courses_in_category <-
    str_extract_all(category_requirements,
                    "(CS|MATH|STAT|AMATH|ENGL).([0-9]{3})([A-Z]?)") %>% unlist() %>% str_squish()
  courses_collapsed <- paste(courses_in_category, collapse = ", ")
  category_description[i] <-
    paste0(description, courses_collapsed, other_requirements)
  # Generate category name
  category[i] <- paste0("Category G", i)
  # Gets credits for all courses in the current category
  credit <-
    filter(course_calendar, `Course Code` %in% courses_in_category) %>%
    select(`Credit Amount`) %>% .[[1]] %>% as.numeric() %>% sort(decreasing = TRUE)
  # Determines the category min and max based on the category description \
  # (if it requires one, two, three, etc courses)
  if (length(credit) == 0)
    credit = rep(10000, 5)
  sum_credit <- sum(credit)
  if (grepl("(O|o)ne", item)) {
    category_max[i] <- min(max(credit), 3.0)
    category_min[i] <- min(min(credit), 3.0)
  } else if (grepl("(T|t)wo", item)) {
    category_max[i] <- min((credit[1] + credit[2]), 6.0)
    category_min[i] <-
      min((credit[length(credit)] + credit[length(credit) - 1]), 6.0)
  } else if (grepl("(A|a)ll", item)) {
    category_max[i] <- sum_credit
    category_min[i] <- sum_credit
  } else if (grepl("(T|t)hree", item)) {
    category_max[i] <- min((credit[1] + credit[2] + credit[3]), 9.0)
    category_min[i] <-
      min((credit[length(credit)] + credit[length(credit) - 1] + credit[length(credit) - 2]), 9.0)
  } else if (grepl("(F|f)our", item)) {
    category_max[i] <-
      min((credit[1] + credit[2] + credit[3] + credit[4]), 12.0)
    category_min[i] <-
      min((credit[length(credit)] + credit[length(credit) - 1] + credit[length(credit) - 2] + credit[length(credit) - 3]), 12.0)
  } else{
    category_max[i] <- -1.0
    category_min[i] <- -1.0
  }
  # Determines if the category is required (all courses in category must be taken)
  if (sum_credit == category_max[i] & sum_credit == category_min[i])
    isCore[i] = TRUE
  i <- i + 1
  other_requirements <- ""
}

# Creates a data frame for the statistics program requirements
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
  "Core Requirement"
)

# Write CSV Files ####

# write.csv(program_requirements,
#           "University of Waterloo Statistics Program Requirements.csv")
# write.csv(cs_courses_waterloo,
#           "University of Waterloo Computer Science Course Calendar.csv")
# write.csv(
#   amath_courses_waterloo,
#   "University of Waterloo Applied Mathematics Course Calendar.csv"
# )
# write.csv(
#   mthel_courses_waterloo,
#   "University of Waterloo Mathematics Electives Course Calendar.csv"
# )
# write.csv(engl_courses_waterloo,
#           "University of Waterloo English Course Calendar.csv")
# write.csv(math_courses_waterloo,
#           "University of Waterloo Mathematics Course Calendar.csv")
# write.csv(stat_courses_waterloo,
#           "University of Waterloo Statistics Course Calendar.csv")
