# Program Requirement Scraping for Wilfrid Laurier University ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

options(timeout = max(1000000, getOption("timeout")))

# Course Information ####
calendar_link <-
  "https://academic-calendar.wlu.ca/program.php?cal=1&d=2589&p=5763&s=1034&y=85"
calendar_page <- read_html(calendar_link)

# Want to get concentration courses as well
concentration_link <-
  "https://academic-calendar.wlu.ca/program.php?cal=1&d=2589&p=5764&s=1034&y=85"
concentration_page <- read_html(concentration_link)

link_text <-
  c((html_nodes(calendar_page, "a") %>% html_text()), (html_nodes(concentration_page, "a")) %>% html_text())
links <-
  c((html_nodes(calendar_page, "a") %>% html_attr("href")), (html_nodes(concentration_page, "a") %>% html_attr("href")))
course_indices <-
  grep("(CP|ST|MA|DATA|EC|BU|ENTR)([0-9]{3})", link_text)
course_codes <- link_text[course_indices] %>% unique()
course_links <- links[course_indices] %>% unique()

calendar_main_link <- "https://academic-calendar.wlu.ca/"

closeAllConnections()

num_courses <- length(course_codes)
course_name <- vector("character", num_courses)
course_description <- vector("character", num_courses)
credit_amount <- vector("numeric", num_courses)
antireq <- vector("character", num_courses)
coreq <- vector("character", num_courses)
prereq <- vector("character", num_courses)
lecture <- vector("logical", num_courses)
lab <- vector("logical", num_courses)
tutorial <- vector("logical", num_courses)
note <- vector("character", num_courses)
hours <- vector("character", num_courses)

i <- 1
for (item in course_links) {
  course_page <- read_html(paste0(calendar_main_link, item))
  course_name[i] <-
    html_nodes(course_page, "span:nth-child(2)") %>% html_text()
  credit_amount[i] <-
    html_nodes(course_page, "span~ span") %>% html_text() %>% .[[1]] %>%
    str_extract("(([0-9]{1})(\\.)([0-9]{1}))") %>% as.numeric()
  course_description[i] <-
    html_nodes(course_page, "p") %>% html_text() %>% paste(collapse = "")  %>%
    str_squish()
  if (course_description[i] == "")
    course_description[i] <-
    html_nodes(course_page, ".content div span") %>% html_text() %>% paste(collapse = "")
  hours[i] <-
    html_nodes(course_page, ".hours") %>% html_text() %>% paste(collapse = "")
  lecture[i] <- grepl("Lecture", hours[i])
  lab[i] <- grepl("Lab", hours[i])
  tutorial[i] <- grepl("Tutorial", hours[i])
  other_info <-
    html_nodes(course_page, "#main > div.content > div.reqs > dl") %>%
    html_text() %>% paste("") %>% str_split("\\.|\\[") %>% unlist()
  prereq[i] <-
    grep("Prerequisites", other_info, value = TRUE) %>% paste0("") %>% str_remove("Prerequisites")
  antireq[i] <-
    grep("Exclusions", other_info, value = TRUE) %>% paste0("") %>% str_remove("Exclusions")
  coreq[i] <-
    grep("Co\\-requisites", other_info, value = TRUE) %>% paste0("") %>% str_remove("Co\\-requisites")
  note[i] <-
    grep("Note(s|:)", other_info, value = TRUE) %>% paste0("") %>% str_remove("Note(s|:)") %>%
    str_squish()
  i <- i + 1
}

credit_amount <- credit_amount * 6

course_information <-
  data.frame(
    course_codes,
    course_name,
    course_description,
    credit_amount,
    antireq,
    coreq,
    prereq,
    hours,
    lecture,
    lab,
    tutorial,
    note
  )
colnames(course_information) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Hours",
    "Lecture",
    "Lab",
    "Tutorial",
    "Note"
  )

closeAllConnections()

# Requirement Information ####
requirement_link <-
  "https://students.wlu.ca/programs/science/data-science/program-requirements.html"
requirement_page <- read_html(requirement_link)

category_description <-
  html_nodes(requirement_page, "ul:nth-child(4) li") %>% html_text()

num_categories <- length(category_description)
requirement_category <-
  vector(mode = "character", length = num_categories)
category_min <- vector(mode = "numeric", length = num_categories)
category_max <- vector(mode = "numeric", length = num_categories)
isCore <- vector(mode = "logical", length = num_categories)

credit_values <-
  c("0\\.5", "1\\.0", "1\\.5", "2\\.0", "2\\.5", "3\\.0")
new_credit_values <- c("3", "6", "9", "12", "15", "18")
i <- 1
for (item in credit_values) {
  category_description <-
    str_replace_all(category_description, item, new_credit_values[i])
  i <- i + 1
}

i <- 1
for (item in category_description) {
  requirement_category[i] <- paste0("Category G", i)
  courses <-
    str_extract_all(item, "(CP|ST|MA|DATA|EC|BU|ENTR)([0-9]{3})", item) %>% unlist()
  item <-
    str_remove_all(item, "(CP|ST|MA|DATA|EC|BU|ENTR)([0-9]{3})")
  credit <- str_extract_all(item, "([0-9]+)") %>% unlist()
  if (length(credit) == 0) {
    credit <-
      filter(course_information, `Course Code` %in% courses) %>%
      select(`Credit Amount`)
    if (grepl("or", item)) {
      category_min[i] <- min(credit)
      category_max[i] <- max(credit)
    } else{
      category_min[i] <- sum(credit)
      category_max[i] <- sum(credit)
    }
    if (category_min[i] == sum(credit) &
        category_max[i] == sum(credit)) {
      isCore[i] = TRUE
    }
  } else{
    isCore[i] <- FALSE
    category_min[i] <- credit[1]
    category_max[i] <- credit[1]
  }
  i <- i + 1
}

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
  "Core Course"
)

# Concentration Requirements ####
requirement_category <- vector(mode = "character")
category_description <- vector(mode = "character")
category_min <- vector(mode = "numeric")
category_max <- vector(mode = "numeric")
isCore <- vector(mode = "logical")
alphabets <- c("A", "B", "C", "D", "E", "F", "G")

i <- 1
j <- 11
k <- 0
l <- 1
last_category <- -1
while (j < 18) {
  if (j %% 2 == 1) {
    # TODO: Calculate previous category min/max
    if (last_category != -1) {
      category_min[last_category] <-
        sum(category_min[rep((last_category + 1):(i - 1))])
      category_max[last_category] <-
        sum(category_max[rep((last_category + 1):(i - 1))])
    }
    if (j == 17)
      break
    # Get new category information
    category_description[i] <-
      html_nodes(requirement_page, paste0("p:nth-child(", j, ") strong")) %>%
      html_text()
    k <- k + 1
    l <- 1
    requirement_category[i] <- paste0("Category S", k)
    isCore[i] <- NA
    last_category <- i
    i <- i + 1
  } else{
    requirements_in_subcategory <-
      html_nodes(requirement_page,
                 paste0("#text-1 > ul:nth-child(", j, ") > li")) %>%
      html_text()
    for (item in requirements_in_subcategory) {
      requirement_category[i] <- paste0("Category S", k, alphabets[l])
      z <- 1
      for (credit in credit_values) {
        item <-
          str_replace_all(item, credit, new_credit_values[z])
        z <- i + 1
      }
      category_description[i] <- item
      if (grepl("((O|o)ne\\s)|(3.*credit)", item)) {
        category_max[i] <- 3
        category_min[i] <- 3
        isCore[i] <- FALSE
      } else if (grepl("((T|t)wo\\s)|(6.*credit)", item)) {
        category_max[i] <- 6
        category_min[i] <- 6
        isCore[i] = FALSE
      } else{
        courses_in_subcategory <-
          str_extract_all(item, "(CP|ST|MA|DATA|EC|BU|ENTR)([0-9]{3})") %>% unlist()
        category_description[i] <-
          paste(courses_in_subcategory, collapse = " ")
        credits <-
          filter(course_information,
                 `Course Code` %in% courses_in_subcategory) %>% select(`Credit Amount`) %>% sum()
        category_max[i] <- credits
        category_min[i] <- credits
        isCore[i] = ((category_max[i] == category_min[i]) &
                       (category_max[i] == sum(credits)))
      }
      l <- l + 1
      i <- i + 1
    }
  }
  j <- j + 1
}

concentration_requirements <-
  data.frame(requirement_category,
             category_description,
             category_min,
             category_max,
             isCore)
colnames(concentration_requirements) <- c(
  "Requirement Category",
  "Category Description",
  "Category Minimum Credit Amount",
  "Category Maximum Credit Amount",
  "Core Course"
)

# Write CSV Files ####
# write.csv(
#   program_requirements,
#   "Wilfrid Laurier University Data Science Program Requirements.csv"
# )
# write.csv(course_information,
#           "Wilfrid Laurier University Required Courses.csv")
# write.csv(
#   concentration_requirements,
#   "Wilfrid Laurier University Data Science Program Specialization Requirements.csv"
# )

closeAllConnections()
