# University of Toronto Data Science Program Requirements Scraping
# Note: Credits are using the standardized definition
# (ie 3 credits per course, 120 credits for four year degree)

# Packages ####
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Functions ####

# Source for util functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# TODO List ####
# TODO: Redo categories based on criteria discussed with Irene

# Required Courses ####

# Scraps information about required courses for the program

# Link to program page
program_link <-
  "https://artsci.calendar.utoronto.ca/program/asspe1687"

# Scraps the course codes for the required courses
courses <-
  get_text_css(program_link, "#block-fas-content a") %>% unique() %>%
  grep("CSC|JSC|MAT|STA", .  , value = TRUE)

# Course links for the required courses
program_page <- read_html(program_link)
course_links <-
  html_nodes(program_page, "#block-fas-content a") %>% html_attr("href") %>%
  unique() %>% grep("CSC|JSC|MAT|STA", .  , value = TRUE)

academic_calendar_link <- "https://artsci.calendar.utoronto.ca"

number_of_courses <- length(courses)

# Vectors for storing information about the courses
course_codes <-
  vector(mode = "character", length = number_of_courses)
course_names <-
  vector(mode = "character", length = number_of_courses)
course_descriptions <-
  vector(mode = "character", length = number_of_courses)
prereq <- vector(mode = "character", length = number_of_courses)
coreq <- vector(mode = "character", length = number_of_courses)
# Breadth Requirement
breadth <- vector(mode = "character", length = number_of_courses)
antireq <- vector(mode = "character", length = number_of_courses)
# Distribution Requirement
distribution <-
  vector(mode = "character", length = number_of_courses)
# Delivery Format
delivery <- vector(mode = "character", length = number_of_courses)
antireq <- vector(mode = "character", length = number_of_courses)
# Any courses recommended before taking (not prereqs)
recommended <-
  vector(mode = "character", length = number_of_courses)
credits <- vector(mode = "numeric", length = number_of_courses)
hours <- vector(mode = "character", length = number_of_courses)
other <- vector(mode = "character", length = number_of_courses)

# The following loop works by scraping the information about each course from
# each link, and then uses regex to clean and sort the data into the proper
# vector.

i <- 1

for (link in course_links) {
  course_page <- read_html(paste0(academic_calendar_link, link))
  course_title <-
    html_nodes(course_page, ".page-title") %>% html_text() %>%
    str_split(":") %>% unlist() %>% str_squish()
  course_codes[i] <- course_title[1] %>% str_squish()
  course_names[i] <- course_title[2] %>% str_squish()
  course_descriptions[i] <-
    html_nodes(course_page, "#block-fas-content .field--label-hidden p") %>%
    html_text() %>% str_squish() %>% paste(collapse = " ")
  # Contains any other info about the course not in the above vectors
  other_course_information <-
    html_nodes(course_page, "#block-fas-content > div > article > div > div") %>%
    html_text() %>% str_squish()
  # Removes the course description if present in the other information vector
  dupliate_info <-
    grep(course_descriptions[i], other_course_information, fixed = TRUE)
  for (dup in dupliate_info) {
    other_course_information <- other_course_information[-dup]
  }
  # Searches through the other course info and places it into the correct vector
  prereq[i] <-
    grep("Prerequisite", other_course_information, value = TRUE) %>% paste(collapse = " ")
  coreq[i] <-
    grep("Corequisite", other_course_information, value = TRUE) %>% paste(collapse = " ")
  breadth[i] <-
    grep("Breadth Requirements", other_course_information, value = TRUE) %>% paste(collapse = " ")
  distribution[i] <-
    grep("Distribution Requirements",
         other_course_information,
         value = TRUE) %>% paste(collapse = " ")
  delivery[i] <-
    grep("Mode of Delivery", other_course_information, value = TRUE) %>% paste(collapse = " ")
  antireq[i] <-
    grep("Exclusion", other_course_information, value = TRUE) %>% paste(collapse = " ")
  recommended[i] <-
    grep("Recommended Preparation", other_course_information, value = TRUE) %>% paste(collapse = " ")
  # Assigns the correct credit amount for the course, with the credits being
  # equivalent to UBC credits (Based on H/Y in course code: H = 3, Y = 6)
  if (grepl("(CSC|JSC|MAT|STA)([0-9]*)(H)([0-9]*)", course_codes[i])) {
    credits[i] <- 3
  } else if (grepl("(CSC|JSC|MAT|STA)([0-9]*)(Y)([0-9]*)", course_codes[i])) {
    credits[i] <- 6
  }
  hours[i] <-
    grep("Hours", other_course_information, value = TRUE) %>% paste(collapse = " ")
  # Places any unused infromation into the other vector
  other[i] <-
    grep(
      "Prerequisite|Corequisite|Breadth Requirements|Hours|Distribution Requirements|Hours|Mode of Delivery|Exclusion|Recommended Preparation",
      other_course_information,
      value = TRUE,
      invert = TRUE
    ) %>% paste(collapse = " ")
  i <- i + 1
}

# Logical vectors for if the course has a lecture, lab, etc.
lecture <- grepl("L", hours)
tutorial <- grepl("T", hours)
lab <- grepl("P", hours)
seminar <- grepl("S", hours)

# Cleans data (remove extra labels in text)
antireq <- str_remove_all(antireq, "Exclusion |NOTE: ")
coreq <- str_remove_all(coreq, "Corequisite ")
prereq <- str_remove_all(prereq, "Prerequisite ")
hours <- str_remove_all(hours, "Hours ")
delivery <- str_remove_all(delivery, "Mode of Delivery ")
breadth <- str_remove_all(breadth, "Breadth Requirements ")
distribution <-
  str_remove_all(distribution, "Distribution Requirements ")

# Creates a data frame for the required courses, filtering out those without
# a course calendar entry
required_courses <-
  data.frame(
    course_codes,
    course_names,
    course_descriptions,
    credits,
    antireq,
    coreq,
    prereq,
    recommended,
    hours,
    lecture,
    lab,
    tutorial,
    seminar,
    breadth,
    delivery,
    distribution,
    other
  ) %>%
  filter(!course_codes == "Sorry, this course is not in the current Calendar.")

colnames(required_courses) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Recommended Courses",
    "Hours",
    "Lecture",
    "Lab",
    "Tutorial",
    "Seminar",
    "Breadth Requirement",
    "Delivery Format",
    "Distribution Requirement",
    "Note"
  )

# Program Requirements ####

# Scraps program requirements from the data science program website

# How many credits required in each year
year_requirement_categories <-
  html_nodes(program_page,
             ".field--name-field-completion-requirements em") %>% html_text() %>% str_squish()

# Program requirement descriptions
other_requirement_information <-
  html_nodes(program_page, ".field--name-field-completion-requirements p") %>% html_text() %>% str_squish()

# First year requirements
first <-
  grep("First",
       other_requirement_information,
       value = TRUE,
       ignore.case = TRUE)

# Second year requirements
second <-
  grep("Second",
       other_requirement_information,
       value = TRUE,
       ignore.case = TRUE)

# Upper year (3/4) requirements
upper <-
  html_nodes(program_page,
             ".field--name-field-completion-requirements li") %>% html_text() %>% str_squish()

# Cleans and separates the requirement information each year into the format the
# categories appear in the final data frame (courses seperate)

# First Year
first_year_requirements <-
  str_split(first, "Note") %>% unlist() %>% .[1] %>%
  strsplit(year_requirement_categories[1], fixed = TRUE) %>% unlist() %>% .[2] %>%
  str_remove("\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}).is recommended\\)") %>%
  str_split("(?=,)(?<!\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}))") %>% unlist()

first_year_category_description <-
  str_replace_all(first_year_requirements, "\\/", " or") %>%
  str_replace_all("(,)(?<![0-9]{1},)", "") %>% str_squish() %>% str_replace_all(",", " and")

# Second Year
second_year_requirements <-
  str_split(second, "Note") %>% unlist() %>% .[1] %>%
  strsplit(year_requirement_categories[2], fixed = TRUE) %>% unlist() %>% .[2] %>%
  str_remove("\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}).is recommended\\)") %>%
  str_split("(?=,)(?<!\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}))") %>% unlist()

second_year_category_description <-
  str_replace_all(second_year_requirements, "\\/", " or") %>%
  str_replace_all("(,)(?<![0-9]{1},)", "") %>% str_replace_all(",", " and") %>% str_squish()

# Upper Year
upper_year_requirements1 <-
  str_remove(upper[1],
             "\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}).is recommended\\)") %>%
  str_split("(?=,)(?<!\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}))") %>%
  unlist() %>% str_remove_all(",") %>% str_replace_all("\\/", " or") %>% str_squish()

upper_year_requirements2 <-
  upper[2] %>% str_replace_all("\\/", " or")

# Upper year requirements were in two vectors, so they are merged together
upper_year_category_description <-
  c(upper_year_requirements1, upper_year_requirements2)

# General Requirements (electives/options)
general_category <- get_item_vector("General Category ", 2)

upper_year_requirements3 <-
  upper[3] %>% str_replace_all("\\/", " or") %>% str_squish()

general_category_description <-
  c(upper_year_requirements3, upper[4])

# Descriptions for categories
descriptions <-
  c(
    first_year_category_description,
    "Second Year Requirements",
    second_year_category_description,
    "Upper Year Requirements",
    upper_year_category_description
  )

# Vectors for storing program requirement information
category <- vector(mode = "character")
category_description <- vector(mode = "character")
category_min_credit <- vector(mode = "numeric")
category_max_credit <- vector(mode = "numeric")
isCore <- vector(mode = "logical")
alpahbets <- c("A", "B", "C", "D", "E", "F")

# Tracks which index of the above vectors the loop is using
i <- 1

# Used to generate the number in each category name (Category S1, j would be 1)
j <- 1

# Used for categories with subcategories to determine the letter label from
# the alphabets vector (Category U3A means k = 1)
k <- 1

# Year for category name (F = first, S = second, U = upper)
year <- "F"

for (item in descriptions) {
  if (grepl("(Second|Upper)( Year Requirements)", item)) {
    # If the year switches, changes the letter used in generating category names
    j <- 1
    year <- substr(item, 1, 1)
    next
  }
  # Extracts all courses in the current category
  courses_in_category <-
    str_extract_all(item, "(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1})") %>% unlist()
  # Determines the credit amount for all courses, which is used latter to determine
  # if the category is a core requirement
  total_credits <-
    sum(ifelse(grepl("Y", courses_in_category), 6, 3))
  # If the current description has multiple options (subcategories) to fill it
  # (A course or B course or C course ...)
  if (grepl("\\(.+?and.+?\\)", item)) {
    category[i] <- paste0("Category ", year, j)
    category_description[i] <- item
    # Splits the item up into its subcategories (each or group)
    subcategories <-
      str_split(item, "or") %>% unlist() %>% str_squish()
    temp <- i
    i <- i + 1
    maximum <- 0
    minimum <- 100000
    for (sub in subcategories) {
      # Extracts all courses in the subcategory, generates the description, credit
      # amount, and labels it as a non-core requirement
      courses_in_sub <-
        str_extract_all(sub, "(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1})") %>% unlist()
      category[i] <- paste0(category[temp], alpahbets[k])
      category_description[i] <- sub
      category_min_credit[i] <-
        sum(ifelse(grepl("Y", courses_in_sub), 6, 3))
      category_max_credit[i] <-
        sum(ifelse(grepl("Y", courses_in_sub), 6, 3))
      maximum <- max(maximum, category_max_credit[i])
      minimum <- min(minimum, category_min_credit[i])
      isCore[i] <- FALSE
      k <- k + 1
      i <- i + 1
    }
    category_max_credit[temp] <- maximum
    category_min_credit[temp] <- minimum
    isCore[temp] <-
      (
        category_max_credit[temp] == category_min_credit[temp] &
          category_max_credit[temp] == total_credits
      )
    k <- 1
  } else{
    # Any description which does not contain subcategories
    category[i] <- paste0("Category ", year, j)
    category_description[i] <- item
    category_min_credit[i] <-
      min(ifelse(grepl("Y", courses_in_category), 6, 3))
    category_max_credit[i] <-
      min(ifelse(grepl("Y", courses_in_category), 6, 3))
    isCore[i] <-
      (
        category_max_credit[i] == category_min_credit[i] &
          category_max_credit[i] == total_credits
      )
    i <- i + 1
  }
  j <- j + 1
}

# Requirements Dataframe ####

program_requirements <-
  data.frame(category,
             category_description,
             category_min_credit,
             category_max_credit,
             isCore)

colnames(program_requirements) <-
  c(
    "Requirement Category",
    "Category Description",
    "Category Minimum Credit Amount",
    "Category Maximum Credit Amount",
    "Core Course"
  )

# Add general requirements to data frame
general_requirement <- rep(NA, ncol(program_requirements))
i <- 1
for (item in general_category_description) {
  general_requirement[1] <- general_category[i]
  general_requirement[2] <- general_category_description[i]
  # Extracts and standardizes credit amount to UBC equivalent
  credit <-
    as.numeric(str_extract(item, "([0-9]{1}\\.[0-9]{1})(?=.credit)")) * 6
  general_requirement[3] <-
    ifelse(grepl("an additional", item), 0, credit)
  general_requirement[4] <- credit
  general_requirement[5] <- FALSE
  program_requirements <-
    rbind(program_requirements, general_requirement)
  i <- i + 1
}

# Generate CSV Files ####
# write.csv(program_requirements,
#           "University of Toronto Data Science Program Requirements.csv")
# write.csv(required_courses,
#           "University of Toronto Data Science Required Courses.csv")
