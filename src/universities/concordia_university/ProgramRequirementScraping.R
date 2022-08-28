# Concordia University Data Science Program Requirement Scraping ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Required Course Calendars ####

# Get the course calendars which contain courses required by the program

# Math and Stat Courses ####

# Reads webpage containing the information about math and science courses at Concordia
math_stat_course_link <-
  "https://www.concordia.ca/academics/undergraduate/calendar/current/section-31-faculty-of-arts-and-science/section-31-200-department-of-mathematics-and-statistics/mathematics-and-statistics-courses.html#2565"
math_stat_course_page <- read_html(math_stat_course_link)

# Scraps math/stat course titles (names, codes, and credit amounts)
math_stat_courses <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.group.panel.xlarge > h3"
  ) %>%
  html_text()

# Number of math/stat courses
math_stat_num_courses <- length(math_stat_courses)

# Scraps the description for math/stat courses
math_stat_course_description <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p.crse-descr, #content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p.crse-descr"
  ) %>% html_text() %>% str_remove_all("Description:") %>% unlist() %>% str_squish()

# Scraps the course code for math/stat courses
math_stat_course_code <-
  str_extract_all(math_stat_courses,
                  "((?:ACTU|MACF|MATH|MAST|STAT)\\s[0-9]{3})") %>%
  unlist()

# Extracts the course name from the course title
math_stat_course_name <-
  gsub(
    "((?:ACTU|MACF|MATH|MAST|STAT)\\s[0-9]{3})(.*)(\\([0-9]?\\scredits\\))",
    "\\2",
    math_stat_courses
  ) %>%
  str_squish()

# Extracts the credit amount from the course title
math_stat_course_credit_amount <-
  gsub(
    "((?:ACTU|MACF|MATH|MAST|STAT)\\s[0-9]{3})(.*)(?:\\(([0-9]?)\\scredits\\))",
    "\\3",
    math_stat_courses
  ) %>%
  str_squish() %>% as.numeric()

# Scraps all the information about each course
course_info <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div"
  ) %>%
  html_text() %>% str_squish()

# Scarps the components for each course (lecture, labs, etc.)
course_components <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p > span.components"
  ) %>%
  html_text() %>% str_squish()

# Scraps the pre/co-requisites for each course
course_requisites <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p > span.requisites"
  ) %>%
  html_text() %>% str_squish()

# Scraps the antirequisites and other random information for the courses
course_notes <-
  html_nodes(
    math_stat_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > ul"
  ) %>%
  html_text() %>% str_squish()

# Creates a vector of indices which is true if the corresponding course has
# any pre/co requisites
has_requisites <- grepl("Prerequisite\\/Corequisite:", course_info)

# Creates a vector of indices which is true if the corresponding course has
# any course components listed (labs, lecture, etc.)
has_components <- grepl("Component\\(s\\):", course_info)

# Creates a vector of indices which is true if the corresponding course has
# any course notes listed (includes antireqs and other info)
has_notes <- grepl("Notes:", course_info)

# Vectors for course information to be placed into
antireq <- vector(mode = "character", math_stat_num_courses)
coreq <- vector(mode = "character", math_stat_num_courses)
prereq <- vector(mode = "character", math_stat_num_courses)
hours <- vector(mode = "character", math_stat_num_courses)
lecture <- vector(mode = "logical", math_stat_num_courses)
lab <- vector(mode = "logical", math_stat_num_courses)
tutorial <- vector(mode = "logical", math_stat_num_courses)
research <- vector(mode = "logical", math_stat_num_courses)
reading <- vector(mode = "logical", math_stat_num_courses)
notes <- vector(mode = "character", math_stat_num_courses)

# loop index
i <- 1

# Tracks which item in the requirements vector the loop is on
# (as not all courses have other requirements)
req_index <- 1

# Tracks which item in the components vector the loop as on
# (as not all courses have their components listed, or don't have any)
comp_index <- 1

# Tracks which item the note index is on
# (as not all courses have notes)
note_index <- 1

# This loop sorts through the above information, and sorts it into the proper
# vector and row for each course using regular expressions
while (i <= math_stat_num_courses) {
  if (has_requisites[i]) {
    item <-
      course_requisites[req_index] %>% str_split("\\.") %>% unlist()
    # Get prereqs
    prereq[i] <-
      grep("course(s)?\\smust\\sbe\\scompleted\\spreviously:",
           item,
           value = TRUE) %>%
      paste(collapse = " ")
    # Get coreqs
    coreq[i] <-
      grep("previously\\sor\\sconcurrently:", item, value = TRUE) %>%
      paste(collapse = " ")
    req_index <- req_index + 1
  }
  if (has_components[i]) {
    # Sort lecture components
    item <- course_components[comp_index]
    lecture[i] <- grepl("Lecture", item)
    lab[i] <- grepl("Laboratory", item)
    tutorial[i] <- grepl("Tutorial", item)
    research[i] <- grepl("Research", item)
    reading[i] <- grepl("Reading", item)
    comp_index <- comp_index + 1
  }
  if (has_notes[i]) {
    # Gets anti requisites and other notes
    item <-
      course_notes[note_index] %>% str_split("\\.") %>% unlist()
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

# Generates data frame to hold the math and stat courses

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

colnames(math_stat_course_dataframe) <- c(
  "Course Code",
  "Course Name",
  "Course Description",
  "Credit Amount",
  "Antirequisite",
  "Corequisite",
  "Prerequisite",
  "Lecture",
  "Lab",
  "Tutorial",
  "Reading",
  "Research",
  "Notes"
)

# Computer Science and Software Engineering Courses ####

# As the code is nearly identical to the math/stat code, it is not commented.

compsci_course_link <-
  "https://www.concordia.ca/academics/undergraduate/calendar/current/section-71-gina-cody-school-of-engineering-and-computer-science/section-71-70-department-of-computer-science-and-software-engineering/section-71-70-10-computer-science-and-software-engineering-courses.html#3527"
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
  html_text() %>% str_remove_all("Description:") %>% unlist() %>% str_squish()

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
notes <- vector(mode = "character", compsci_num_courses)

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

colnames(compsci_course_dataframe) <- c(
  "Course Code",
  "Course Name",
  "Course Description",
  "Credit Amount",
  "Antirequisite",
  "Corequisite",
  "Prerequisite",
  "Lecture",
  "Lab",
  "Tutorial",
  "Reading",
  "Research",
  "Notes"
)

# Engineering Courses ####

# As the code below is copied from the math/stat code, it is not commented

engineering_course_link <-
  "https://www.concordia.ca/academics/undergraduate/calendar/current/section-71-gina-cody-school-of-engineering-and-computer-science/section-71-60-engineering-course-descriptions.html#3482"
engineering_course_page <- read_html(engineering_course_link)

engineering_courses <-
  html_nodes(
    engineering_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.group.panel.xlarge > h3"
  ) %>%
  html_text()

engineering_num_courses <- length(engineering_courses)
engineering_course_description <-
  html_nodes(
    engineering_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p.crse-descr"
  ) %>%
  html_text() %>% str_remove_all("Description:") %>% unlist() %>% str_squish()

engineering_course_code <-
  str_extract_all(
    engineering_courses,
    "((?:ENCS|ENGR|AERO|BCEE|BLDG|CIVI|COEN|ELEC|IADI|INDU|MECH|MIAE)\\s[0-9]{3})"
  ) %>%
  unlist()
engineering_course_name <-
  gsub(
    "((?:ENCS|ENGR|AERO|BCEE|BLDG|CIVI|COEN|ELEC|IADI|INDU|MECH|MIAE)\\s[0-9]{3})(.*)(\\([0-9]?(?:\\.[0-9])?\\scredits\\))",
    "\\2",
    engineering_courses
  ) %>%
  str_squish()
engineering_course_credit_amount <-
  gsub(
    "((?:ENCS|ENGR|AERO|BCEE|BLDG|CIVI|COEN|ELEC|IADI|INDU|MECH|MIAE)\\s[0-9]{3})(.*)(?:\\(([0-9]?(?:\\.[0-9]{0,2})?)\\scredits\\))",
    "\\3",
    engineering_courses
  ) %>%
  str_squish() %>% as.numeric()

course_info <-
  html_nodes(
    engineering_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div"
  ) %>%
  html_text()

course_components <-
  html_nodes(
    engineering_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p > span.components"
  ) %>%
  html_text() %>% str_squish()
course_requisites <-
  html_nodes(
    engineering_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > p > span.requisites"
  ) %>%
  html_text() %>% str_squish()
course_notes <-
  html_nodes(
    engineering_course_page,
    "#content-main > div > div > div.content-main.parsys > div.wysiwyg.parbase.section > div > div > div > div > div > div > div > div.content.accordion_accordion_panel > ul"
  ) %>%
  html_text() %>% str_squish()

has_requisites <- grepl("Prerequisite\\/Corequisite:", course_info)
has_components <- grepl("Component\\(s\\):", course_info)
has_notes <- grepl("Notes:", course_info)

antireq <- vector(mode = "character", engineering_num_courses)
coreq <- vector(mode = "character", engineering_num_courses)
prereq <- vector(mode = "character", engineering_num_courses)
hours <- vector(mode = "character", engineering_num_courses)
lecture <- vector(mode = "logical", engineering_num_courses)
lab <- vector(mode = "logical", engineering_num_courses)
tutorial <- vector(mode = "logical", engineering_num_courses)
research <- vector(mode = "logical", engineering_num_courses)
reading <- vector(mode = "logical", engineering_num_courses)
notes <- vector(mode = "character", engineering_num_courses)

i <- 1
req_index <- 1
comp_index <- 1
note_index <- 1
while (i <= engineering_num_courses) {
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

engineering_course_dataframe <-
  data.frame(
    engineering_course_code,
    engineering_course_name,
    engineering_course_description,
    engineering_course_credit_amount,
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

colnames(engineering_course_dataframe) <- c(
  "Course Code",
  "Course Name",
  "Course Description",
  "Credit Amount",
  "Antirequisite",
  "Corequisite",
  "Prerequisite",
  "Lecture",
  "Lab",
  "Tutorial",
  "Reading",
  "Research",
  "Notes"
)

# All Courses Data Frame

# Merges the three scraped frames into one
all_courses <-
  rbind(
    math_stat_course_dataframe,
    compsci_course_dataframe,
    engineering_course_dataframe
  )

# Program Requirements ####

# Gets the requirements for the data science program

# Reads the webpage on the Concordia academic calendar containing the required courses
program_link <-
  "https://www.concordia.ca/academics/undergraduate/calendar/current/section-31-faculty-of-arts-and-science/section-31-200-department-of-mathematics-and-statistics/ba-bsc-joint-major-in-data-science.html"
program_page <- read_html(program_link)

# Extracts the course codes for the required courses
required_courses <-
  html_nodes(program_page, ".formatted-course") %>%
  html_text() %>% str_squish() %>% gsub("(.*)((?:COMP|SOEN|ACTU|MACF|MATH|MAST|STAT|ENCS)\\s[0-9]{3})(.*)",
                                        "\\2",
                                        .)

# Gets any non-course requirements for the program (electives)
other_requirement_description <-
  html_nodes(program_page, ".joint-major-in-data-science tr+ tr td+ td") %>%
  html_text() %>% str_squish()

# Gets the amount of credits required by non-course requirements
other_requirements_credits <-
  html_nodes(program_page,
             ".joint-major-in-data-science tr+ tr td:nth-child(1)") %>%
  html_text() %>% str_squish()

# Merges the other requirement description and credits into one vector
other_requirements <-
  outer(other_requirements_credits,
        other_requirement_description,
        paste,
        " ") %>%
  .[1, ] %>% str_squish()

# Number of rows for the data frame
num_categories <-
  length(required_courses) + length(other_requirements)

# Generates the category names for the data frame
requirement_category <-
  mapply(paste0, "Category G", rep(1:num_categories)) %>% as.vector()

# Merges all category descriptions into one vector (course and non-course requirements)
category_description <- c(required_courses, other_requirements)

# Selects the credit amounts from all_courses data frame or from the category
# description (for non-course requirements)
credits <-
  c((
    filter(all_courses, `Course Code` %in% required_courses) %>% select(`Credit Amount`) %>% unlist() %>% as.vector()
  ),
  (
    gsub("([0-9]+)(\\scredit)(.*)", "\\1", other_requirements) %>% as.numeric()
  ))

# As there are no subcategories, all requirement maximums == minimum
category_min <- credits
category_max <- credits

# As only the non-course requirements are not core, the vector is generated by
# counting the number of course and non-course requirements and assigning the
# correct Boolean x times
isCore <-
  c(rep(TRUE, length(required_courses)), rep(FALSE, length(other_requirements)))

# Creates a data frame for the course requirements
program_requirements <-
  data.frame(requirement_category,
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
#           "Concordia University Data Science Program Requirements.csv")
# write.csv(
#   math_stat_course_dataframe,
#   "Concordia University Mathematics and Statistics Courses.csv"
# )
# write.csv(compsci_course_dataframe,
#           "Concordia University Computer Science Courses.csv")
# write.csv(engineering_course_dataframe,
#           "Concordia University Engineering Courses.csv")
