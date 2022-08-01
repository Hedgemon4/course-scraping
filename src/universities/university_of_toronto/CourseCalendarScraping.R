# University of Toronto Course Calendar Scraping

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Computer Science Course Calendar ####

cs_page <-
  read_html(
    curl(
      "https://artsci.calendar.utoronto.ca/section/Computer-Science",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

cs_course_title <-
  html_nodes(
    cs_page,
    "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div > h3"
  ) %>%
  html_text()
cs_course_code <-
  str_extract_all(cs_course_title, "(?:CSC|J(?:S|C)C)[0-9]{3}(Y|H)[0-9]{1}") %>% unlist()
cs_course_name <-
  gsub(
    "((?:CSC|J(?:S|C)C)[0-9]{3}(?:Y|H)[0-9]{1})(\\s\\-\\s)(.*)",
    "\\3",
    cs_course_title
  ) %>%
  str_squish()
cs_num_courses <- length(cs_course_title)
cs_course_description <-
  vector(mode = "character", length = cs_num_courses)
cs_note <- vector(mode = "character", length = cs_num_courses)
cs_credit_amount <-
  vector(mode = "numeric", length = cs_num_courses)
cs_antireq <- vector(mode = "character", length = cs_num_courses)
cs_coreq <- vector(mode = "character", length = cs_num_courses)
cs_prereq <- vector(mode = "character", length = cs_num_courses)
cs_hours <- vector(mode = "character", length = cs_num_courses)
cs_mode <- vector(mode = "character", length = cs_num_courses)
cs_dist <- vector(mode = "character", length = cs_num_courses)
cs_breadth <- vector(mode = "character", length = cs_num_courses)
cs_rec_prep <- vector(mode = "character", length = cs_num_courses)
cs_lecture <- vector(mode = "logical", length = cs_num_courses)
cs_lab <- vector(mode = "logical", length = cs_num_courses)
cs_tutorial <- vector(mode = "logical", length = cs_num_courses)
cs_seminar <- vector(mode = "logical", length = cs_num_courses)

test <- vector(mode = "character", length = cs_num_courses)

i <- 1
while (i <= cs_num_courses) {
  cs_course_text <- html_nodes(
    cs_page,
    paste0(
      "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div:nth-child(",
      i,
      ") > div > div"
    )
  ) %>%
    html_text() %>% str_squish() %>% paste(collapse = ". ") %>% str_split("((Note(s|))|(NOTE(S|))):") %>% unlist()
  cs_course_description[i] <- cs_course_text[1] %>% paste("")
  cs_note[i] <-
    if_else(is.na(cs_course_text[2]), "", cs_course_text[2] %>% paste("")) %>%
    str_squish()
  cs_credit_amount[i] <- ifelse(grepl("Y", cs_course_code[i]), 6, 3)
  cs_course_info <- html_nodes(
    cs_page,
    paste0(
      "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div:nth-child(",
      i,
      ") > div > span"
    )
  ) %>%
    html_text() %>% str_squish()
  test[i] <- paste(cs_course_info, collapse = "")
  cs_antireq[i] <-
    grep("Exclusion:", cs_course_info, value = TRUE) %>% paste0(collapse = "")
  i <- i + 1
}

cs_course_calendar <-
  data.frame(
    cs_course_code,
    cs_course_name,
    cs_course_description,
    cs_credit_amount,
    cs_antireq,
    test,
    cs_note
  )
