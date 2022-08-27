# University of California Berkeley Data Science Program Requirement Scraping ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Required Courses ####

# Gets the potentially required courses and collects their information into a 
# data frame

# Reads the html page for the UC Berkeley Data Science Requirements
program_page <- read_html(
  curl(
    "http://guide.berkeley.edu/undergraduate/degree-programs/data-science/#majorrequirementstext",
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  )
)

# Scraps the links to the required courses from the page
course_links <-
  html_nodes(program_page,
             "div#majorrequirementstextcontainer > table > tbody > tr > td.codecol > a") %>%
  html_attr("href")

# Gets the link names, which are the course codes
link_text <-
  html_nodes(program_page,
             "div#majorrequirementstextcontainer > table > tbody > tr > td.codecol > a") %>%
  html_text()
course_code <- vector(mode = "character")
num_links <- length(link_text)

# Due to some equivalent course codes, the links need to be cleaned before they are useed
# The loop separates out anything which is not part of the first course code

i <- 1
while (i <= num_links) {
  item <- link_text[i]
  if (grepl("\\/", item)) {
    name <- str_extract(item, "(.+?)(?=\\/)")
    if (!grepl("([A-Z]?[0-9]{1,3}[A-Z]{0,1})", name)) {
      name <-
        paste0(name,
               str_extract(item, "(\\s[A-Z]?[0-9]{1,3}[A-Z]{0,1})"))
    }
    course_code[i] <- name
  } else{
    course_code[i] <- item
  }
  i <- i + 1
}

# Vectors for course information
course_name <- vector(mode = "character")
credit_amount <- vector(mode = "numeric")
course_description <- vector(mode = "character")
antireq <- vector(mode = "character")
prereq <- vector(mode = "character")
equiv <- vector(mode = "character")
hours <- vector(mode = "character")
lecture <- vector(mode = "logical")
lab <- vector(mode = "logical")
tutorial <- vector(mode = "logical")
seminar <- vector(mode = "logical")

# This loops goes through each link scraped from the requirements page, and gets
# the course name, credit amount, description, anti requisites, prerequisites,
# equivalencies, hours of course components (lab, lecture, etc.), and generates
# a true false variable for if the course has a lecture, lab, tutorial, or seminar.

i <- 1
for (link in course_links) {
  # The loops works by scraping the information from each course link, and then
  # uses regular expressions to search the text vector for expressions which indicate
  # what information they contain. The information is cleaned and then placed into
  # the correct vector
  course_page <- read_html(curl(
    paste0("http://guide.berkeley.edu", link),
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  ))
  courses <-
    html_nodes(course_page,
               "#fssearchresults > div.searchresult.search-courseresult > h2") %>%
    html_text() %>% str_squish()
  # This cleans the link names so they are the same as the scraped course codes
  # These are used to make sure the correct course is scraped and prevents
  # problems from duel listings
  pattern <-
    paste0("(", "^", str_replace_all(course_code[i], "\\s", " "), ")")
  index <- grep(pattern, courses, perl = TRUE)
  # The course title contains the course name, code, and credit amount
  course_title <-
    html_nodes(course_page,
               "#fssearchresults > div.searchresult.search-courseresult > h2") %>%
    .[[index]] %>% html_text() %>% str_squish()
  course_name[i] <-
    gsub(
      "(.+?[A-Z]?[0-9]{1,3}[A-Z]{0,2})(\\s.+?)((?:[0-9]\\s\\-\\s)?[0-9]\\sUnits)",
      "\\2",
      course_title
    ) %>%
    str_squish()
  # Gets the credit amount from the course title
  credit_amount[i] <-
    gsub("(.*[A-Z]?[0-9]{1,3}[A-Z]{0,2})(\\s.*)([0-9])(\\sUnits)",
         "\\3",
         course_title) %>%
    as.numeric()
  course_description[i] <-
    html_nodes(
      course_page,
      "#fssearchresults > div.searchresult.search-courseresult > div > div > p.courseblockdesc"
    ) %>%
    .[[index]] %>% html_text() %>% str_squish() %>% str_remove(paste0(course_name[i], ":\\sRead\\sMore.*"))
  # Other information is a vector containing remaing info about the course
  # This includes prereqs, antireqs, course hours, etc.
  other_info <-
    html_nodes(course_page,
               "#fssearchresults > div.searchresult.search-courseresult") %>%
    .[[index]] %>% html_nodes("div > div > div > div > p") %>% html_text() %>% str_squish()
  prereq[i] <- grep("Prerequisites:", other_info, value = TRUE) %>%
    str_remove("Prerequisites:\\s") %>% paste0("")
  antireq[i] <-
    grep("Credit\\sRestrictions:\\s", other_info, value = TRUE) %>%
    str_remove("Credit\\sRestrictions:\\s") %>% paste0("")
  equiv[i] <-
    grep("Also\\slisted\\sas:\\s", other_info, value = TRUE) %>%
    str_remove("Also\\slisted\\sas:\\s") %>% paste0("")
  hours[i] <-
    grep("Fall\\sand\\/or\\sspring:", other_info, value = TRUE) %>%
    str_remove("Fall\\sand\\/or\\sspring:") %>% paste0("")
  lecture[i] <- grepl("lecture", hours[i])
  lab[i] <- grepl("laboratory ", hours[i])
  tutorial[i] <- grepl("discussion", hours[i])
  seminar[i] <- grepl("seminar", hours[i])
  i <- i + 1
}

# Generates and names columns of data frame from gathered course information
required_courses <-
  data.frame(
    course_code,
    course_name,
    course_description,
    credit_amount,
    antireq,
    prereq,
    equiv,
    hours,
    lecture,
    lab,
    seminar,
    tutorial
  )

colnames(required_courses) <- c(
  "Course Code",
  "Course Name",
  "Course Description",
  "Credit Amount",
  "Antirequisite",
  "Prerequisite",
  "Equivalency",
  "Hours",
  "Lecture",
  "Lab",
  "Seminar",
  "Tutorial"
)


# Program Requirements ####

# Gathers and places the program requirements into a data frame

# Reads the html page for the UC Berkeley Data Science Requirements
program_page <- read_html(
  curl(
    "http://guide.berkeley.edu/undergraduate/degree-programs/data-science/#majorrequirementstext",
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  )
)

# Scraps all the required courses, which are in separate tables, into one table
all_tables <-
  html_nodes(program_page, "div#majorrequirementstextcontainer") %>% .[[1]] %>% html_table()

# Vectors containing information about course codes and credits from webpage
courses <- select(all_tables, Code) %>% unlist() %>% unname()
course_credit_amount <-
  select(all_tables, Units) %>% unlist() %>% unname()
grep("or", courses)

# Vectors to store program requirement information
num_courses <- length(courses)
categories <- "Category L1"
category_description <- "All of"
min_credits <- vector(mode = "character")
max_credits <- vector(mode = "character")
isCore <- vector(mode = "logical")
letter <- "L"

# Counts where the item should be placed in the above vectors
j <- 2

# Tracks the category number
k <- 1

# Tracks the number of subcategories
num_count <- 1

# Tracks which row of the course units from the webpage the loop is on
credit_count <- 1

# Vector containing numbering for subcategories
numbers <- rep(1:100) %>% str_pad(3, side = "left", pad = "0")

# Tracks the previous category
prev_category <- 1

# The Loop parses through the information in the table and outputs it into vectors
# in accordance with how the data should be formatted, and fetches the credit amounts
# from the data frame containing course information

# Loop counter
i <- 1

# Loop needs to run one extra time due the need to determine the amount of credits
# for the last category
while (i <= num_courses + 1) {
  item <- courses[i]
  credits <- course_credit_amount[credit_count]
  if (grepl("Code", item) | is.na(item)) {
    isCore[prev_category] <- FALSE
    letter <- "U"
    k <- k + 1
    # Determine Credit amounts for items in previous category based on the category description
    # and if the category/courses are core requirements (must be taken)
    if (grepl("(All)", category_description[prev_category])) {
      min_credits[prev_category] <-
        sum(min_credits[(prev_category + 1):(j - 1)] %>% as.numeric())
      max_credits[prev_category] <-
        sum(max_credits[(prev_category + 1):(j - 1)] %>% as.numeric())
      # Determine which courses are core requirements
      x <- prev_category + 1
      # If all items in a category are required, they could be core requirements
      while (x < j) {
        # If there is no alternatives (or) it is a core requirement
        if (!grepl("or", category_description[x]))
          isCore[x] <- TRUE
        x <- x + 1
      }
    } else if (grepl("(one)", category_description[prev_category])) {
      min_credits[prev_category] <-
        min(min_credits[(prev_category + 1):(j - 1)] %>% as.numeric())
      max_credits[prev_category] <-
        max(max_credits[(prev_category + 1):(j - 1)] %>% as.numeric())
      # If the category has only one requirement and is choose 1, it is a core as long as the category
      # has no or option
      if ((prev_category + 1) == (j - 1)) {
        isCore[prev_category + 1] <-
          !grepl("or", category_description[prev_category + 1])
      }
    } else if (grepl("two", category_description[prev_category])) {
      min_credits_category <-
        min_credits[(prev_category + 1):(j - 1)] %>% as.numeric() %>% sort()
      max_credits_category <-
        max_credits[(prev_category + 1):(j - 1)] %>% as.numeric() %>% sort(decreasing = T)
      min_credits[prev_category] <-
        min_credits_category[1] + min_credits_category[2] %>% as.numeric()
      max_credits[prev_category] <-
        max_credits_category[1] + max_credits_category[2] %>% as.numeric()
      if (grepl("[0-9]", category_description[prev_category])) {
        min_credits[prev_category] <-
          str_extract(category_description[prev_category], "[0-9]") %>%
          as.numeric()
      }
      # If the category is choose two and has only two requirements, they could be core requirements
      if ((prev_category + 1) == (j - 2)) {
        # Makes sure the category has no alternative options
        isCore[prev_category + 1] <-
          !grepl("or", category_description[prev_category + 1])
        isCore[prev_category + 2] <-
          !grepl("or", category_description[prev_category + 2])
      }
    }
    # Exits the loop from the last run
    if (is.na(item)) {
      i <- i + 1
      next
    }
    # Generates next category
    next_item <- courses[i + 1]
    categories[j] <- paste0("Category ", letter, k)
    if (grepl("(one)|(two)|(three)|(four)|(all)", next_item)) {
      category_description[j] <- next_item
      i <- i + 1
    } else{
      # All the categories that do not have a description require one course
      category_description[j] <- "Choose one of the following: "
    }
    num_count <- 1
    # Saves row of previous category to calculate category credit amounts
    prev_category <- j
    j <- j + 1
  } else if (grepl("or", item)) {
    new_description <-
      paste(category_description[j - 1], item, sep = " ")
    category_description[j - 1] <- new_description
    isCore[j - 1] <- FALSE
  } else {
    category_description[j] <- item
    categories[j] <-
      paste0("Category ", letter, k, numbers[num_count])
    num_count <- num_count + 1
    # Matches the credit amount to the correct row from the table pulled off the
    # website
    while (!grepl("[0-9]", credits)) {
      credit_count <- 1 + credit_count
      credits <- course_credit_amount[credit_count]
    }
    # Dash indicates the courses in the subcategory have different credit amounts
    if (grepl("\\-", credits)) {
      min_credits[j] <- str_extract(credits, "(^[0-9])") %>% as.numeric()
      max_credits[j] <-
        str_extract(credits, "([0-9]$)") %>% as.numeric()
    } else{
      min_credits[j] <- credits %>% as.numeric()
      max_credits[j] <- credits %>% as.numeric()
    }
    isCore[j] <- FALSE
    credit_count <- credit_count + 1
    j <- j + 1
  }
  i <- i + 1
}

# Generates data frame which holds the program requirements

program_requirements <-
  data.frame(categories,
             category_description,
             min_credits,
             max_credits,
             isCore)

colnames(program_requirements) <- c(
  "Requirement Category",
  "Category Description",
  "Category Minimum Credit Amount",
  "Category Maximum Credit Amount",
  "Core Requirement"
)

# Write CSV Files ####

write.csv(
  program_requirements,
  "University of California Berkeley Data Science Program Requirements.csv"
)
write.csv(required_courses,
          "University of California Berkeley Required Courses.csv")
