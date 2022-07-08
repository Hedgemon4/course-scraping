# University of Toronto Data Science Program Requirements Scraping

# Packages ####
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Functions ####

# Source for util functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# TODO List ####
# TODO: Clean Code

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

course_codes <-
  vector(mode = "character", length = number_of_courses)
course_names <-
  vector(mode = "character", length = number_of_courses)
course_descriptions <-
  vector(mode = "character", length = number_of_courses)
prereq <- vector(mode = "character", length = number_of_courses)
coreq <- vector(mode = "character", length = number_of_courses)
breadth <- vector(mode = "character", length = number_of_courses)
antireq <- vector(mode = "character", length = number_of_courses)
distribution <-
  vector(mode = "character", length = number_of_courses)
delivery <- vector(mode = "character", length = number_of_courses)
antireq <- vector(mode = "character", length = number_of_courses)
recommended <-
  vector(mode = "character", length = number_of_courses)
credits <- vector(mode = "character", length = number_of_courses)
hours <- vector(mode = "character", length = number_of_courses)
other <- vector(mode = "character", length = number_of_courses)

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
  other_course_information <-
    html_nodes(course_page, "#block-fas-content > div > article > div > div") %>%
    html_text() %>% str_squish()
  dupliate_info <-
    grep(course_descriptions[i], other_course_information, fixed = TRUE)
  for (dup in dupliate_info) {
    other_course_information <- other_course_information[-dup]
  }
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
  if (grepl("(CSC|JSC|MAT|STA)([0-9]*)(H)([0-9]*)", course_codes[i])) {
    credits[i] <- "0.5"
  } else if (grepl("(CSC|JSC|MAT|STA)([0-9]*)(Y)([0-9]*)", course_codes[i])) {
    credits[i] <- "1.0"
  }
  hours[i] <-
    grep("Hours", other_course_information, value = TRUE) %>% paste(collapse = " ")
  other[i] <-
    grep(
      "Prerequisite|Corequisite|Breadth Requirements|Hours|Distribution Requirements|Hours|Mode of Delivery|Exclusion|Recommended Preparation",
      other_course_information,
      value = TRUE,
      invert = TRUE
    ) %>% paste(collapse = " ")
  i <- i + 1
}

lecture <- grepl("L", hours)
tutorial <- grepl("T", hours)
lab <- grepl("P", hours)
seminar <- grepl("S", hours)

# Clean Data
antireq <- str_remove_all(antireq, "Exclusion |NOTE: ")
coreq <- str_remove_all(coreq, "Corequisite ")
prereq <- str_remove_all(prereq, "Prerequisite ")
hours <- str_remove_all(hours, "Hours ")
delivery <- str_remove_all(delivery, "Mode of Delivery ")
breadth <- str_remove_all(breadth, "Breadth Requirements ")
distribution <-
  str_remove_all(distribution, "Distribution Requirements ")

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
year_requirement_categories <-
  html_nodes(program_page,
             ".field--name-field-completion-requirements em") %>% html_text() %>% str_squish()
other_requirement_information <-
  html_nodes(program_page, ".field--name-field-completion-requirements p") %>% html_text() %>% str_squish()
first <-
  grep("First",
       other_requirement_information,
       value = TRUE,
       ignore.case = TRUE)
second <-
  grep("Second",
       other_requirement_information,
       value = TRUE,
       ignore.case = TRUE)
upper <-
  html_nodes(program_page,
             ".field--name-field-completion-requirements li") %>% html_text() %>% str_squish()

# First Year
first_year_requirements <-
  str_split(first, "Note") %>% unlist() %>% .[1] %>%
  strsplit(year_requirement_categories[1], fixed = TRUE) %>% unlist() %>% .[2] %>%
  str_remove("\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}).is recommended\\)") %>%
  str_split("(?=,)(?<!\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}))") %>% unlist()

first_year_category <-
  get_item_vector("First Year Category", length(first_year_requirements))
first_year_category_description <-
  str_replace_all(first_year_requirements, "\\/", " or") %>%
  str_replace_all("(,)(?<![0-9]{1},)", "") %>% str_squish() %>% str_replace_all(",", " and")

# Second Year
second_year_requirements <-
  str_split(second, "Note") %>% unlist() %>% .[1] %>%
  strsplit(year_requirement_categories[2], fixed = TRUE) %>% unlist() %>% .[2] %>%
  str_remove("\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}).is recommended\\)") %>%
  str_split("(?=,)(?<!\\(.(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1}))") %>% unlist()

second_year_category <-
  get_item_vector("Second Year Category", length(second_year_requirements))
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

upper_year_category <-
  get_item_vector(
    "Upper Year Category",
    length(upper_year_requirements1) + length(upper_year_requirements2)
  )
upper_year_category_description <-
  c(upper_year_requirements1, upper_year_requirements2)

# General Requirements
general_category <- get_item_vector("General Category", 2)

upper_year_requirements3 <-
  upper[3] %>% str_replace_all("\\/", " or") %>% str_squish()

general_category_description <-
  c(upper_year_requirements3, upper[4])

categories <-
  c(first_year_category,
    second_year_category,
    upper_year_category)
category_descriptions <-
  c(
    first_year_category_description,
    second_year_category_description,
    upper_year_category_description
  )

courses_list <-
  str_extract_all(category_descriptions,
                  "(CSC|JSC|MAT|STA)([0-9]{3})(Y|H)([0-9]{1})")
courses <- unlist(courses_list)

course_category <-
  vector(mode = "character", length = length(courses))
course_category_description <-
  vector(mode = "character", length = length(courses))

i <- 1
j <- 1
for (item in courses_list) {
  course_vector <- unlist(item)
  for (value in course_vector) {
    course_category[j] <- categories[i]
    course_category_description[j] <- category_descriptions[i]
    j <- j + 1
  }
  i <- i + 1
}

# Requirements Dataframe ####

course_requirements_with_categories <-
  data.frame(courses, course_category, course_category_description)
colnames(course_requirements_with_categories) <-
  c("Course Code", "Category", "Category Description")

program_requirements <-
  merge(course_requirements_with_categories, required_courses, by = "Course Code") %>%
  select(
    "Course Code",
    "Course Name",
    "Course Description",
    "Category",
    "Category Description",
    "Credit Amount",
    "Delivery Format",
    "Antirequisite",
    "Prerequisite",
    "Corequisite",
    "Recommended Courses",
    "Hours",
    "Lecture",
    "Lab",
    "Tutorial",
    "Seminar",
    "Breadth Requirement",
    "Distribution Requirement",
    "Note"
  )

# Add general requirement
general_requirement <- rep(NA, ncol(program_requirements))
i <- 1
for (item in general_category) {
  general_requirement[1] <- general_category[i]
  general_requirement[4] <- general_category[i]
  general_requirement[5] <- general_category_description[i]
  program_requirements <-
    rbind(program_requirements, general_requirement)
  i <- i + 1
}

# Generate CSV Files ####
# write.csv(general_requirement, "University of Toronto Data Science Program Requirements.csv")
# write.csv(required_courses, "University of Toronto Data Science Required Courses.csv")
