# Data Science Requirement Scraping from the University of Manitoba ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Program Requirements ####

# Link to program requirements
web_link <-
  "https://catalog.umanitoba.ca/undergraduate-studies/science/data-science/data-science-bsc-major"

# Read the webpage containing the program requirement information
web_page <- read_html(web_link)

# Gets the course codes for the required courses
course_codes <-
  html_nodes(web_page, "#degreerequirementstextcontainer .code") %>% html_text()

# Course Information ####

# Gathers the information about each course potentially required by the program
# (course code, name, description, credit amount, etc.)

# Gets the links to the required courses
courses_main_link <- "https://catalog.umanitoba.ca/"
course_info_links <-
  html_nodes(web_page, "#degreerequirementstextcontainer .code") %>% html_attr("href")
number_of_courses <- length(course_info_links)

# Vectors for Course Information
course_name <-
  vector(mode = "character", length = number_of_courses)
course_description <-
  vector(mode = "character", length = number_of_courses)
course_credit <-
  vector(mode = "character", length = number_of_courses)
prereq <- vector(mode = "character", length = number_of_courses)
coreq <- vector(mode = "character", length = number_of_courses)
notes <- vector(mode = "character", length = number_of_courses)
equiv <- vector(mode = "character", length = number_of_courses)
exclusive <- vector(mode = "character", length = number_of_courses)

# Goes to each course code link, and separates out the various details about the
# courses into the column vectors declared above using regex

i <- 1
for (item in course_info_links) {
  course_link <- paste0(courses_main_link, course_info_links[i])
  course_page <- read_html(course_link)
  course_information <-
    html_nodes(course_page, ".courseblockextra") %>% html_text() %>% str_squish()
  course_name[i] <-
    html_node(course_page, ".detail-title strong") %>% html_text() %>% str_squish()
  course_description[i] <- course_information[1]
  course_credit[i] <-
    html_node(course_page, ".detail-hours_html strong") %>% html_text() %>% str_squish()
  equiv[i] <-
    paste(grep("Equiv To:", course_information, value = TRUE), collapse = "")
  exclusive[i] <-
    paste(grep("Mutually Exclusive:", course_information, value = TRUE),
          collapse = "")
  notes[i] <-
    paste(grep("Attributes:", course_information, value = TRUE),
          collapse = "")
  # Needed as pre/co-requisites stored in the same text block
  prcr <-
    grep("PR/CR", course_information, value = TRUE) %>% strsplit(split = "\\.") %>% unlist()
  prereq[i] <-
    grep("Prerequisite(s|)",
         prcr,
         ignore.case = TRUE,
         value = TRUE) %>% paste(collapse = "")
  coreq[i] <-
    grep(
      "Co-requisite|corequisite|co requisite",
      prcr,
      ignore.case = TRUE,
      value = TRUE
    ) %>% paste(collapse = "")
  i = i + 1
}

# Logical vector for if the class requires a lab
labs <- grepl("lab|Lab", course_description)

# Data frame for storing the information about all the potentially required courses
# for the program
course_info <-
  data.frame(
    course_codes,
    course_name,
    course_description,
    course_credit,
    prereq,
    coreq,
    equiv,
    exclusive,
    notes,
    labs
  )

colnames(course_info) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Prerequisite",
    "Corequisite",
    "Equivalency",
    "Antirequisite",
    "Note",
    "Lab"
  )

# Program Requirements ####

# Scraps the table which stores the required courses
program_table <-
  html_nodes(web_page,
             "#degreerequirementstextcontainer > table.sc_plangrid") %>%
  html_table() %>% .[[1]]
colnames(program_table) <- c("code", "name", "credit")

# Removes empty rows
program_table <- filter(program_table, code != "")

# Vectors for program requirement information
category <- vector(mode = "character")
category_description <- vector(mode = "character")
category_min_credit <- vector(mode = "numeric")
category_max_credit <- vector(mode = "numeric")
category_isCore <- vector(mode = "logical")

# Tracks if the item is a sub category and needs to be placed into a category
# as it is a non-core requirement (choose one of a list)
isSub <- FALSE

sub_category <- ""
sub_category_description <- ""
year <- "F"

# Tracks which item in the program requirement vector the loop is on
i <- 1

# Tracks which category number in each year the loop is on
# (ie in U1, j would be 1)
j <- 1

# Tracks which row of the table the loop is on
rownum <- 0

# Tracks the general requirement number
k <- 1

# This loop reads each row from the table containing the program requirements, and
# places the items into the above vectors
for (item in program_table$code) {
  rownum <- rownum + 1
  # If the item is in a sub category and is a course code, add it to the current
  # category
  if (isSub) {
    if (grepl("(COMP|MATH|STAT|DATA|SCI).([0-9]{4})", item) &
        program_table$credit[rownum] == "") {
      sub_category_description <-
        paste(" ", sub_category_description, item)
      next
    } else{
      # The program reached the end of sub category items, and the information about
      # that category needs to be calculated (credit amount)
      category[i] <- sub_category
      category_description[i] <-
        sub_category_description %>% str_squish()
      credit <-
        str_extract(sub_category_description, "([0-9])") %>% unlist() %>% as.numeric()
      category_min_credit[i] <- credit
      category_max_credit[i] <- credit
      # As there are multiple options to fulfill the category, it is not a core
      # requirement
      category_isCore[i] <- FALSE
      i <- i + 1
      j <- j + 1
      isSub <- FALSE
    }
  }
  if (grepl("Year(s|) (1|2|3\\-4)", item)) {
    # Switches the letter label for the year
    year_num <- str_extract(item, "[0-9]{1}") %>% as.numeric()
    year <- switch(year_num, "F", "S", "U")
    isSub <- FALSE
    j <- 1
    next
  } else if (grepl("Co\\-op.Requirements", item)) {
    # Switches the letter category label to C (co-op)
    year <- "C"
    isSub <- FALSE
    j <- 1
    next
  } else if (grepl("([0-9]?).credit.hours.from:", item)) {
    # start of a sub category
    isSub <- TRUE
    sub_category <- paste0("Category ", year, j)
    sub_category_description <- item %>% str_squish()
    next
  } else if (grepl("(COMP|MATH|STAT|DATA|SCI).([0-9]{4})", item)) {
    # Item is a core course
    category_isCore[i] <- TRUE
    category[i] <- paste0("Category ", year, j)
    category_description[i] <- item
    credit <- program_table$credit[rownum] %>% as.numeric()
    category_max_credit[i] <- credit
    category_min_credit[i] <- credit
    i <- i + 1
    j <- j + 1
  } else{
    # Item is a general requirement (elective)
    category_isCore[i] <- FALSE
    category[i] <- paste0("Category G", k)
    category_description[i] <- item
    credit <- program_table$credit[rownum] %>% as.numeric()
    category_max_credit[i] <- credit
    category_min_credit[i] <- credit
    i <- i + 1
    k <- k + 1
  }
}

# Creates a data frame for the program requirements
program_requirements <-
  data.frame(
    category,
    category_description,
    category_min_credit,
    category_max_credit,
    category_isCore
  )

colnames(program_requirements) <- c(
  "Requirement Category",
  "Category Description",
  "Category Minimum Credit Amount",
  "Category Maximum Credit Amount",
  "Core Course"
)

View(program_requirements)

# Write CSV Files ####
# write.csv(program_requirements,
#           "University of Manitoba Data Science Program Requirements.csv")
# write.csv(course_info, "University of Manitoba Data Science Required Courses.csv")
