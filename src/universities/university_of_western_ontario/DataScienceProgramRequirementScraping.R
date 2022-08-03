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
