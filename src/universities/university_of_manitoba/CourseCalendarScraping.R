# Program Calendar Scraping - University of Manitoba ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Computer Science Program Calendar ####

cs_page <-
  read_html(
    curl(
      "https://catalog.umanitoba.ca/undergraduate-studies/science/computer-science/#coursestext",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

cs_course_code <-
  html_nodes(
    cs_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-3.detail-code.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()
cs_course_name <-
  html_nodes(
    cs_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-7.detail-title.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()
cs_credit_amount <-
  html_nodes(
    cs_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.detail-hours_html.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text() %>% str_remove_all("[^0-9]") %>% unlist() %>% as.numeric()
cs_num_courses <- length(cs_course_code)
cs_course_description <-
  vector(mode = "character", length = cs_num_courses)
cs_prereq <- vector(mode = "character", length = cs_num_courses)
cs_coreq <- vector(mode = "character", length = cs_num_courses)
cs_antireq <- vector(mode = "character", length = cs_num_courses)
cs_equiv <- vector(mode = "character", length = cs_num_courses)
cs_note <- vector(mode = "character", length = cs_num_courses)
cs_lab <- vector(mode = "logical", length = cs_num_courses)

i <- 1
while (i <= cs_num_courses) {
  cs_course_info <-
    html_nodes(cs_page,
               paste0("#coursestextcontainer > div > div:nth-child(", i, ") > div > p")) %>%
    html_text()
  cs_lab[i] <- grepl("Lab\\sRequired", cs_course_info[1])
  cs_course_description[i] <-
    cs_course_info[1] %>% str_remove("\\(Lab\\sRequired\\)") %>% str_squish()
  prcr <-
    grep("PR\\/CR:", cs_course_info, value = TRUE) %>% str_split("\\.") %>% unlist()
  cs_prereq[i] <- grep("Prerequisite(s|):", prcr, value = TRUE) %>% paste("") %>%
    str_remove("Prerequisite(s|):") %>% str_squish()
  cs_coreq[i] <-
    grep(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration",
      prcr,
      value = TRUE
    ) %>% paste("") %>% 
    str_remove("Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration") %>% 
    str_squish()
  cs_antireq[i] <-
    grep("Mutually\\sExclusive:", cs_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Mutually\\sExclusive:") %>% str_squish()
  cs_equiv[i] <- grep("Equiv\\sTo:", cs_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Equiv\\sTo:") %>% str_squish()
  cs_note[i] <- grep("Attributes:", cs_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Attributes:") %>% str_squish()
  i <- i + 1
}

cs_course_calendar <-
  data.frame(
    cs_course_code,
    cs_course_name,
    cs_course_description,
    cs_credit_amount,
    cs_antireq,
    cs_coreq,
    cs_prereq,
    cs_equiv,
    cs_lab,
    cs_note
  )
colnames(cs_course_calendar) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Equivalency",
    "Lab",
    "Note"
  )

# Mathematics Program Calendar ####

math_page <-
  read_html(
    curl(
      "https://catalog.umanitoba.ca/undergraduate-studies/science/mathematics/#coursestext",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

math_course_code <-
  html_nodes(
    math_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-3.detail-code.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()
math_course_name <-
  html_nodes(
    math_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-7.detail-title.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()
math_credit_amount <-
  html_nodes(
    math_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.detail-hours_html.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text() %>% str_remove_all("[^0-9]") %>% unlist() %>% as.numeric()
math_num_courses <- length(math_course_code)
math_course_description <-
  vector(mode = "character", length = math_num_courses)
math_prereq <- vector(mode = "character", length = math_num_courses)
math_coreq <- vector(mode = "character", length = math_num_courses)
math_antireq <- vector(mode = "character", length = math_num_courses)
math_equiv <- vector(mode = "character", length = math_num_courses)
math_note <- vector(mode = "character", length = math_num_courses)
math_lab <- vector(mode = "logical", length = math_num_courses)

i <- 1
while (i <= math_num_courses) {
  math_course_info <-
    html_nodes(math_page,
               paste0("#coursestextcontainer > div > div:nth-child(", i, ") > div > p")) %>%
    html_text()
  math_lab[i] <- grepl("Lab\\s(R|r)equired", math_course_info[1])
  math_course_description[i] <-
    math_course_info[1] %>% str_remove("\\(Lab\\s(R|r)equired\\)") %>% str_squish()
  prcr <-
    grep("PR\\/CR:", math_course_info, value = TRUE) %>% str_split("\\.") %>% unlist()
  math_prereq[i] <- grep("Prerequisite(s|):", prcr, value = TRUE) %>% paste("") %>%
    str_remove("Prerequisite(s|):") %>% str_squish()
  math_coreq[i] <-
    grep(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite",
      prcr,
      value = TRUE
    ) %>% paste("") %>% 
    str_remove("Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite:") %>% 
    str_squish()
  math_antireq[i] <-
    grep("Mutually\\sExclusive:", math_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Mutually\\sExclusive:") %>% str_squish()
  math_equiv[i] <- grep("Equiv\\sTo:", math_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Equiv\\sTo:") %>% str_squish()
  math_note[i] <- grep("Attributes:", math_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Attributes:") %>% str_squish()
  i <- i + 1
}

math_course_calendar <-
  data.frame(
    math_course_code,
    math_course_name,
    math_course_description,
    math_credit_amount,
    math_antireq,
    math_coreq,
    math_prereq,
    math_equiv,
    math_lab,
    math_note
  )
colnames(math_course_calendar) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Equivalency",
    "Lab",
    "Note"
  )
