# University of Western Ontario Data Science Program Requirement Scraping

# Note: The course calendar scraping file must be run before this one for this file
# to work properly.

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Data Science Program Requirements ####

# Reads the academic calendar page containing the requirements for the program
program_page <-
  read_html(
    curl(
      "https://www.westerncalendar.uwo.ca/Modules.cfm?ModuleID=21057&SelectedCalendar=Live&ArchiveID=",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

# Scraps the information outlining the requirements for the program
program_info <- html_nodes(program_page, ".moduleInfo p") %>%
  html_text() %>% str_squish() %>% str_split("(?=[0-9]\\.[0-9]\\scourse)") %>%
  unlist() %>% grep(
    "(Data Science)|(Computer Science)|(Statistical Sciences)|(Software Engineering)",
    .,
    value = TRUE
  )

# Scraps and changes the category credit amounts to the same scale used at UBC
category_credit_amount <-
  str_extract_all(program_info, "([0-9]\\.[0-9])") %>% unlist() %>%
  as.numeric()
category_credit_amount <- category_credit_amount * 6

# Cleans program info vector
program_info <-
  program_info %>% str_extract_all("(?<=:)(.*)") %>% unlist() %>%
  str_squish()

# Vectors for the program requirement information
requirement_category <- vector(mode = "character")
category_description <- vector(mode = "character")
min_credit <- vector(mode = "numeric")
max_credit <- vector(mode = "numeric")
isCore <- vector(mode = "logical")

alphabets <- c("A", "B", "C", "D", "E", "F", "G")

i <- 1
k <- 1
j <- 1
# Goes through each required bullet in the program requirements
for (item in program_info) {
  # Creates the category name
  requirement_category[i] <- paste0("Category G", k)
  category_description[i] <-
    paste0(category_credit_amount[k],
           " credits from: ",
           item %>% str_squish())
  courses_in_category <-
    str_extract_all(
      item,
      "(?<!former\\s)((Computer\\sScience)|(Data\\sScience)|(Mathematics)|(Software\\sEngineering)|(Statistical\\sSciences))\\s[0-9]{4}([A-Z](\\/)?)*"
    ) %>% unlist()
  # Gets the credits for the courses in this category from the combined course calendar
  credits_in_category <-
    filter(combined_course_calendar,
           `Course Code` %in% courses_in_category) %>%
    select(`Credit Amount`) %>% as.vector %>% unlist() %>% unname()
  # Detects if the requirement has subcategories, and separates the items out
  # if it does
  if (grepl("(\\()|(\\))", item)) {
    temp <- i
    subcategories <-
      c(
        str_extract_all(item, "([^,]+\\(.+?\\))") %>% unlist(),
        str_remove_all(item, "(,?[^,]+\\(.+?\\))") %>% str_split(",") %>%
          unlist() %>% stri_remove_empty()  %>% str_squish()
      )
    min_credit[i] <- category_credit_amount[k]
    max_credit[i] <- category_credit_amount[k]
    temp <- i
    i <- i + 1
    # Generates the subcategories, and extracts the credit values from the course
    # calendar
    for (sub in subcategories) {
      requirement_category[i] <- paste0("Category G", k, alphabets[j])
      category_description[i] <- sub
      courses_in_subcategory <-
        str_extract_all(
          sub,
          "(?<!former\\s)((Computer\\sScience)|(Data\\sScience)|(Mathematics)|(Software\\sEngineering)|(Statistical\\sSciences))\\s[0-9]{4}([A-Z](\\/)?)*"
        ) %>% unlist()
      credits_in_subcategory <-
        filter(combined_course_calendar,
               `Course Code` %in% courses_in_subcategory) %>%
        select(`Credit Amount`) %>% as.vector %>% unlist() %>% unname()
      min_credit[i] <- min(credits_in_subcategory)
      max_credit[i] <- max(credits_in_subcategory)
      # If there is not multiple options in the capcategory (ie only one course)
      # it is a core requirement
      isCore[i] <-
        (min_credit[i] == max_credit[i]) &
        (min_credit[i] == sum(credits_in_subcategory))
      i <- i + 1
      j <- j + 1
    }
    isCore[temp] <- FALSE
  } else{
    # The requirement is considered to be core if all the courses are required
    # and there is no alternative options
    # (ie the min and max credit amounts are the same)
    min_credit[i] <- category_credit_amount[k]
    max_credit[i] <- category_credit_amount[k]
    isCore[i] <-
      (category_credit_amount[k] == sum(credits_in_category))
    i <- i + 1
  }
  j <- 1
  k <- k + 1
}

# Generates a data frame for the program requirements

program_requirements <- data.frame(requirement_category,
                                   category_description,
                                   min_credit,
                                   max_credit,
                                   isCore)

colnames(program_requirements) <- c(
  "Requirement Category",
  "Category Description",
  "Category Minimum Credit Amount",
  "Category Maximum Credit Amount",
  "Core Requirement"
)

# Write CSV File ####

write.csv(
  program_requirements,
  "University of Western Ontario Data Science Program Requirements.csv"
)
