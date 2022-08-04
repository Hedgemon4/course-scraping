# University of Western Ontario Course Calendar Scraping

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
      "https://www.westerncalendar.uwo.ca/Courses.cfm?Subject=COMPSCI&SelectedCalendar=Live&ArchiveID=",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

cs_course_links <- html_nodes(cs_page, ".col-md-12 .btn-info") %>%
  html_attr("href")

cs_course_title <-
  html_nodes(cs_page,
             "div.col-md-12 > div.panel-group > div > div.panel-heading") %>%
  html_text() %>% str_squish()
cs_course_code <-
  str_extract_all(cs_course_title,
                  "Computer\\sScience\\s[0-9]{4}([A-Z](\\/)?)*") %>%
  unlist()
cs_course_name <-
  gsub("(Computer\\sScience\\s[0-9]{4}(?:[A-Z](?:\\/)?)*)(.*)",
       "\\2",
       cs_course_title) %>%
  str_squish() %>% str_to_title()

cs_num_courses <- length(cs_course_links)
cs_course_description <-
  vector(mode = "character", length = cs_num_courses)
cs_credit_amount <-
  vector(mode = "numeric", length = cs_num_courses)
cs_antireq <- vector(mode = "character", length = cs_num_courses)
cs_coreq <- vector(mode = "character", length = cs_num_courses)
cs_prereq <- vector(mode = "character", length = cs_num_courses)

i <- 1
while (i <= cs_num_courses) {
  course_page <- read_html(curl(
    paste0("https://www.westerncalendar.uwo.ca/", cs_course_links[i]),
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  ))
  course_info <-
    html_nodes(course_page, "#CourseInformationDiv .col-xs-12") %>%
    html_text() %>% str_squish()
  cs_course_description[i] <-
    grep("Course\\sDescription", course_info, value = TRUE) %>%
    str_remove("Course\\sDescription\\s") %>% str_squish()
  cs_credit_amount[i] <-
    grep("Course\\sWeight", course_info, value = TRUE) %>%
    str_extract("[0-9]\\.[0-9]+") %>% as.numeric()
  cs_antireq[i] <-
    grep("Antirequisite", course_info, value = TRUE) %>%
    str_remove_all("Antirequisite\\(s\\)(:)?") %>% str_squish() %>% paste0(collapse = "")
  pre_co_req <-
    grep("Pre\\sor\\sCorequisites", course_info, value = TRUE) %>%
    str_remove_all("Pre\\sor\\sCorequisites") %>%
    str_split("(\\.)(?![0-9])(?<![0-9])") %>%
    unlist()
  cs_coreq[i] <- grep("Corequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Corequisite\\(s\\)(:)?)|(Pre\\-or\\sCorequisite\\(s\\)(:)?)") %>%
    unlist() %>% str_squish() %>% paste0(collapse = "")
  cs_prereq[i] <- grep("Prerequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Prerequisite\\(s\\)(:)?)") %>% str_squish() %>%
    unlist() %>% paste0(collapse = "")
  i <- i + 1
}

cs_credit_amount <- cs_credit_amount * 6

cs_course_calendar <- data.frame(
  cs_course_code,
  cs_course_name,
  cs_course_description,
  cs_credit_amount,
  cs_antireq,
  cs_coreq,
  cs_prereq
)
colnames(cs_course_calendar) <- c(
  "Course Code",
  "Course Name",
  "Course Description",
  "Credit Amount",
  "Antirequisite",
  "Corequisite",
  "Prerequisite"
)
