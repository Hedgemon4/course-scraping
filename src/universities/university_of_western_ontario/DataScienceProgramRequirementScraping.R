# University of Western Ontario Data Science Program Requirement Scraping

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Data Science Program Requirements ####

program_page <-
  read_html(
    curl(
      "https://www.westerncalendar.uwo.ca/Modules.cfm?ModuleID=21057&SelectedCalendar=Live&ArchiveID=",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

program_info <- html_nodes(program_page, ".moduleInfo p") %>%
  html_text() %>% str_squish() %>% str_split("(?=[0-9]\\.[0-9]\\scourse)") %>%
  unlist() %>% grep(
    "(Data Science)|(Computer Science)|(Statistical Sciences)|(Software Engineering)",
    .,
    value = TRUE
  )

category_credit_amount <-
  str_extract_all(program_info, "([0-9]\\.[0-9])") %>% unlist() %>%
  as.numeric()
category_credit_amount <- category_credit_amount * 6

requirement_category <- vector(mode = "character")
category_description <- vector(mode = "character")
min_credit <- vector(mode = "numeric")
max_credit <- vector(mode = "numeric")
isCore <- vector(mode = "logical")

alphabets <- c("A", "B", "C", "D", "E", "F", "G")

test <- str_split(program_info[2], ",")

i <- 1
k <- 1
for (item in program_info) {
  requirement_category[i] <- paste0("Category G", k)
  category_description[i] <-
    str_extract(item, "(?<=:)(.*)") %>% str_squish()
  courses_in_category <-
    str_extract_all(
      item,
      "((Computer\\sScience)|(Data\\sScience)|(Mathematics)|(Software\\sEngineering)|(Statistical\\sSciences))\\s[0-9]{4}([A-Z](\\/)?)*"
    ) %>% unlist()
  if (grepl("(\\()|(\\))", item)) {
    subcategories <- str_split(item, ",")
  for(sub in subcategories){
    
  }
    requirement_category[i] <- paste0("Category G", k)
  }
  k <- k + 1
  i <- i + 1
}
