# Simon Fraser University Data Science Program Requirement Scraping ####
# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Courses ####

# Business Courses
business_course_link <-
  "https://www.sfu.ca/students/calendar/courses/bus"
business_course_page <- read_html(business_course_link)

business_courses <-
  html_nodes(business_course_page, ".main > h3") %>%
  html_text() %>% str_squish()
business_course_code <-
  gsub("(BUS\\s[0-9]{3}[A-Z]?)\\s\\-\\s(.*)(\\([0-9]*\\))?",
       "\\1",
       business_courses)
business_course_name <-
  gsub(
    "(BUS\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(\\([0-9]\\))?",
    "\\2",
    business_courses
  ) %>%
  str_squish()
business_course_credit_amount <-
  gsub(
    "(BUS\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(?:\\(([0-9]*)\\))?",
    "\\3",
    business_courses
  ) %>%
  str_squish() %>% as.numeric() %>% str_replace_na("0") %>% as.numeric()

business_course_information <-
  html_nodes(business_course_page, ".main > p") %>%
  html_text() %>% str_squish()

business_course_prereq <-
  gsub("(.*)(Prerequisite.*?\\.)(.*)",
       "\\2",
       business_course_information)
business_course_prereq <-
  grep("Prerequisite", business_course_prereq, invert = TRUE) %>%
  replace(business_course_prereq, ., "")

business_course_coreq <-
  gsub("(.*)(Corequisite.*?\\.)(.*)",
       "\\2",
       business_course_information)
business_course_coreq <-
  grep("Corequisite", business_course_coreq, invert = TRUE) %>%
  replace(business_course_coreq, ., "")

business_course_antireq <-
  gsub(
    "(.*)(Students\\swith\\scredit\\sfor.*?\\.)(.*)",
    "\\2",
    business_course_information
  )
business_course_antireq <-
  grep("Students\\swith\\scredit\\sfor",
       business_course_antireq,
       invert = TRUE) %>%
  replace(business_course_antireq, ., "")

business_course_breadth <-
  gsub("(.*)(Breadth.*?\\.)(.*)",
       "\\2",
       business_course_information)
business_course_breadth <-
  grep("Breadth", business_course_breadth, invert = TRUE) %>%
  replace(business_course_breadth, ., "")

business_course_quantitative <-
  grepl("Quantitative\\.", business_course_information)
business_course_writing <-
  grepl("Writing\\.", business_course_information)

business_course_description <-
  str_remove_all(
    business_course_information,
    "(Prerequisite.*?\\.)|(Corequisite.*?\\.)|(Students\\swith\\scredit\\sfor.*?\\.)|(Breadth.*?\\.)|(Quantitative\\.)|(Writing\\.)"
  ) %>%
  str_squish()

business_course_calendar <-
  data.frame(
    business_course_code,
    business_course_name,
    business_course_description,
    business_course_credit_amount,
    business_course_antireq,
    business_course_coreq,
    business_course_prereq,
    business_course_breadth,
    business_course_quantitative,
    business_course_writing
  )
colnames(business_course_calendar) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Breadth Category",
    "Quantitative Requirement",
    "Writing Requirement"
  )

# Computer Science Courses

computer_science_course_link <-
  "https://www.sfu.ca/students/calendar/courses/cmpt"
computer_science_course_page <-
  read_html(computer_science_course_link)

computer_science_courses <-
  html_nodes(computer_science_course_page, ".main > h3") %>%
  html_text() %>% str_squish()
computer_science_course_code <-
  gsub(
    "(CMPT\\s[0-9]{3}[A-Z]?)\\s\\-\\s(.*)(\\([0-9]*\\))?",
    "\\1",
    computer_science_courses
  )
computer_science_course_name <-
  gsub(
    "(CMPT\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(\\([0-9]\\))?",
    "\\2",
    computer_science_courses
  ) %>%
  str_squish()
computer_science_course_credit_amount <-
  gsub(
    "(CMPT\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(?:\\(([0-9]*)\\))?",
    "\\3",
    computer_science_courses
  ) %>%
  str_squish() %>% as.numeric() %>% str_replace_na("0") %>% as.numeric()

computer_science_course_information <-
  html_nodes(computer_science_course_page, ".main > p") %>%
  html_text() %>% str_squish()

computer_science_course_prereq <-
  gsub("(.*)(Prerequisite.*?\\.)(.*)",
       "\\2",
       computer_science_course_information)
computer_science_course_prereq <-
  grep("Prerequisite", computer_science_course_prereq, invert = TRUE) %>%
  replace(computer_science_course_prereq, ., "")

computer_science_course_coreq <-
  gsub("(.*)(Corequisite.*?\\.)(.*)",
       "\\2",
       computer_science_course_information)
computer_science_course_coreq <-
  grep("Corequisite", computer_science_course_coreq, invert = TRUE) %>%
  replace(computer_science_course_coreq, ., "")

computer_science_course_antireq <-
  gsub(
    "(.*)((?:Students\\swith\\scredit\\sfor.*?\\.)|(?:Students\\swho\\shave\\staken.*?\\.))(.*)",
    "\\2",
    computer_science_course_information
  )
computer_science_course_antireq <-
  grep(
    "(Students\\swith\\scredit\\sfor)|(Students\\swho\\shave\\staken)",
    computer_science_course_antireq,
    invert = TRUE
  ) %>%
  replace(computer_science_course_antireq, ., "")

computer_science_course_breadth <-
  gsub("(.*)(Breadth.*?\\.)(.*)",
       "\\2",
       computer_science_course_information)
computer_science_course_breadth <-
  grep("Breadth", computer_science_course_breadth, invert = TRUE) %>%
  replace(computer_science_course_breadth, ., "")

computer_science_course_quantitative <-
  grepl("Quantitative\\.", computer_science_course_information)
computer_science_course_writing <-
  grepl("Writing\\.", computer_science_course_information)

computer_science_course_description <-
  str_remove_all(
    computer_science_course_information,
    "(Prerequisite.*?\\.)|(Corequisite.*?\\.)|(Students\\swith\\scredit\\sfor.*?\\.)|(Breadth.*?\\.)|(?:Students\\swho\\shave\\staken.*?\\.)(Quantitative\\.)|(Writing\\.)"
  ) %>%
  str_squish()

computer_science_course_calendar <-
  data.frame(
    computer_science_course_code,
    computer_science_course_name,
    computer_science_course_description,
    computer_science_course_credit_amount,
    computer_science_course_antireq,
    computer_science_course_coreq,
    computer_science_course_prereq,
    computer_science_course_breadth,
    computer_science_course_quantitative,
    computer_science_course_writing
  )
colnames(computer_science_course_calendar) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Breadth Category",
    "Quantitative Requirement",
    "Writing Requirement"
  )

# Math and Computer Science Courses

math_and_cs_course_link <-
  "https://www.sfu.ca/students/calendar/courses/macm"
math_and_cs_course_page <-
  read_html(math_and_cs_course_link)

math_and_cs_courses <-
  html_nodes(math_and_cs_course_page, ".main > h3") %>%
  html_text() %>% str_squish()
math_and_cs_course_code <-
  gsub(
    "(MACM\\s[0-9]{3}[A-Z]?)\\s\\-\\s(.*)(\\([0-9]*\\))?",
    "\\1",
    math_and_cs_courses
  )
math_and_cs_course_name <-
  gsub(
    "(MACM\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(\\([0-9]\\))?",
    "\\2",
    math_and_cs_courses
  ) %>%
  str_squish()
math_and_cs_course_credit_amount <-
  gsub(
    "(MACM\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(?:\\(([0-9]*)\\))?",
    "\\3",
    math_and_cs_courses
  ) %>%
  str_squish() %>% as.numeric() %>% str_replace_na("0") %>% as.numeric()

math_and_cs_course_information <-
  html_nodes(math_and_cs_course_page, ".main > p") %>%
  html_text() %>% str_squish()

math_and_cs_course_prereq <-
  gsub("(.*)(Prerequisite.*?\\.)(.*)",
       "\\2",
       math_and_cs_course_information)
math_and_cs_course_prereq <-
  grep("Prerequisite", math_and_cs_course_prereq, invert = TRUE) %>%
  replace(math_and_cs_course_prereq, ., "")

math_and_cs_course_coreq <-
  gsub(
    "(.*?)((?:Corequisite.*?\\.)|(?:[A-Z]{3,4}\\s[0-9]{3}[^\\.]*?can\\sbe\\staken.*?\\.))(.*)",
    "\\2",
    math_and_cs_course_information
  )
math_and_cs_course_coreq <-
  grep("Corequisite|(can\\sbe\\staken\\sas)",
       math_and_cs_course_coreq,
       invert = TRUE) %>%
  replace(math_and_cs_course_coreq, ., "")

math_and_cs_course_antireq <-
  gsub(
    "(.*)((?:Students\\swith\\scredit\\sfor.*?\\.)|(?:Students\\swho\\shave\\staken.*?\\.)|(?:Students\\sin\\sexcess.*?\\.))(.*)",
    "\\2",
    math_and_cs_course_information
  )
math_and_cs_course_antireq <-
  grep(
    "(Students\\swith\\scredit\\sfor)|(Students\\swho\\shave\\staken)|(Students\\sin\\sexcess)",
    math_and_cs_course_antireq,
    invert = TRUE
  ) %>%
  replace(math_and_cs_course_antireq, ., "")

math_and_cs_course_breadth <-
  gsub("(.*)(Breadth.*?\\.)(.*)",
       "\\2",
       math_and_cs_course_information)
math_and_cs_course_breadth <-
  grep("Breadth", math_and_cs_course_breadth, invert = TRUE) %>%
  replace(math_and_cs_course_breadth, ., "")

math_and_cs_course_quantitative <-
  grepl("Quantitative\\.", math_and_cs_course_information)
math_and_cs_course_writing <-
  grepl("Writing\\.", math_and_cs_course_information)

math_and_cs_course_description <-
  str_remove_all(
    math_and_cs_course_information,
    "(Prerequisite.*?\\.)|(Corequisite.*?\\.)|(Students\\swith\\scredit\\sfor.*?\\.)|(Breadth.*?\\.)|(Students\\swho\\shave\\staken.*?\\.)|(Students\\sin\\sexcess.*?\\.)|([A-Z]{3,4}\\s[0-9]{3}[^\\.]*?can\\sbe\\staken.*?\\.)|(Quantitative\\.)|(Writing\\.)"
  ) %>%
  str_squish()

math_and_cs_course_calendar <-
  data.frame(
    math_and_cs_course_code,
    math_and_cs_course_name,
    math_and_cs_course_description,
    math_and_cs_course_credit_amount,
    math_and_cs_course_antireq,
    math_and_cs_course_coreq,
    math_and_cs_course_prereq,
    math_and_cs_course_breadth,
    math_and_cs_course_quantitative,
    math_and_cs_course_writing
  )
colnames(math_and_cs_course_calendar) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Breadth Category",
    "Quantitative Requirement",
    "Writing Requirement"
  )

# Data Courses
data_course_link <-
  "https://www.sfu.ca/students/calendar/courses/data"
data_course_page <- read_html(data_course_link)

data_courses <-
  html_nodes(data_course_page, ".main > h3") %>%
  html_text() %>% str_squish()
data_course_code <-
  gsub("(DATA\\s[0-9]{3}[A-Z]?)\\s\\-\\s(.*)(\\([0-9]*\\))?",
       "\\1",
       data_courses)
data_course_name <-
  gsub(
    "(DATA\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(\\([0-9]\\))?",
    "\\2",
    data_courses
  ) %>%
  str_squish()
data_course_credit_amount <-
  gsub(
    "(DATA\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(?:\\(([0-9]*)\\))?",
    "\\3",
    data_courses
  ) %>%
  str_squish() %>% as.numeric() %>% str_replace_na("0") %>% as.numeric()

data_course_information <-
  html_nodes(data_course_page, ".main > p") %>%
  html_text() %>% str_squish()

data_course_prereq <-
  gsub("(.*)(Prerequisite.*?\\.)(.*)",
       "\\2",
       data_course_information)
data_course_prereq <-
  grep("Prerequisite", data_course_prereq, invert = TRUE) %>%
  replace(data_course_prereq, ., "")

data_course_coreq <-
  gsub("(.*)(Corequisite.*?\\.)(.*)",
       "\\2",
       data_course_information)
data_course_coreq <-
  grep("Corequisite", data_course_coreq, invert = TRUE) %>%
  replace(data_course_coreq, ., "")

data_course_antireq <-
  gsub("(.*)(Students\\swith\\scredit\\sfor.*?\\.)(.*)",
       "\\2",
       data_course_information)
data_course_antireq <-
  grep("Students\\swith\\scredit\\sfor",
       data_course_antireq,
       invert = TRUE) %>%
  replace(data_course_antireq, ., "")

data_course_breadth <-
  gsub("(.*)(Breadth.*?\\.)(.*)",
       "\\2",
       data_course_information)
data_course_breadth <-
  grep("Breadth", data_course_breadth, invert = TRUE) %>%
  replace(data_course_breadth, ., "")

data_course_quantitative <-
  grepl("Quantitative\\.", data_course_information)
data_course_writing <- grepl("Writing\\.", data_course_information)

data_course_description <-
  str_remove_all(
    data_course_information,
    "(Prerequisite.*?\\.)|(Corequisite.*?\\.)|(Students\\swith\\scredit\\sfor.*?\\.)|(Breadth.*?\\.)|(Quantitative\\.)|(Writing\\.)"
  ) %>%
  str_squish()

data_course_calendar <-
  data.frame(
    data_course_code,
    data_course_name,
    data_course_description,
    data_course_credit_amount,
    data_course_antireq,
    data_course_coreq,
    data_course_prereq,
    data_course_breadth,
    data_course_quantitative,
    data_course_writing
  )
colnames(data_course_calendar) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Breadth Category",
    "Quantitative Requirement",
    "Writing Requirement"
  )

# Math Courses
math_course_link <-
  "https://www.sfu.ca/students/calendar/courses/math"
math_course_page <- read_html(math_course_link)

math_courses <-
  html_nodes(math_course_page, ".main > h3") %>%
  html_text() %>% str_squish()
math_course_code <-
  gsub("(MATH\\s[0-9]{3}[A-Z]?)\\s\\-\\s(.*)(\\([0-9]*\\))?",
       "\\1",
       math_courses)
math_course_name <-
  gsub(
    "(MATH\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(\\([0-9]\\))?",
    "\\2",
    math_courses
  ) %>%
  str_squish()
math_course_credit_amount <-
  gsub(
    "(MATH\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(?:\\(([0-9]*)\\))?",
    "\\3",
    math_courses
  ) %>%
  str_squish() %>% as.numeric() %>% str_replace_na("0") %>% as.numeric()

math_course_information <-
  html_nodes(math_course_page, ".main > p") %>%
  html_text() %>% str_squish()

math_course_prereq <-
  gsub("(.*)(Prerequisite.*?\\.)(.*)",
       "\\2",
       math_course_information)
math_course_prereq <-
  grep("Prerequisite", math_course_prereq, invert = TRUE) %>%
  replace(math_course_prereq, ., "")

math_course_coreq <-
  gsub(
    "(.*)((?:Corequisite.*?\\.)|(?:Recommended\\s(?:c|C)orequisite.*?\\.))(.*)",
    "\\2",
    math_course_information
  )
math_course_coreq <-
  grep("Corequisite|Recommended\\scorequisite",
       math_course_coreq,
       invert = TRUE) %>%
  replace(math_course_coreq, ., "")

math_course_antireq <-
  gsub("(.*)(Students\\swith\\scredit\\sfor.*?\\.)(.*)",
       "\\2",
       math_course_information)
math_course_antireq <-
  grep("Students\\swith\\scredit\\sfor",
       math_course_antireq,
       invert = TRUE) %>%
  replace(math_course_antireq, ., "")

math_course_breadth <-
  gsub("(.*)(Breadth.*?\\.)(.*)",
       "\\2",
       math_course_information)
math_course_breadth <-
  grep("Breadth", math_course_breadth, invert = TRUE) %>%
  replace(math_course_breadth, ., "")

math_course_quantitative <-
  grepl("Quantitative\\.", math_course_information)
math_course_writing <- grepl("Writing\\.", math_course_information)

math_course_description <-
  str_remove_all(
    math_course_information,
    "(Prerequisite.*?\\.)|(Corequisite.*?\\.)|(Students\\swith\\scredit\\sfor.*?\\.)|(Breadth.*?\\.)|(Recommended\\s(?:c|C)orequisite.*?\\.)|(Writing\\.)|(Quantitative\\.)"
  ) %>%
  str_squish()

math_course_calendar <-
  data.frame(
    math_course_code,
    math_course_name,
    math_course_description,
    math_course_credit_amount,
    math_course_antireq,
    math_course_coreq,
    math_course_prereq,
    math_course_breadth,
    math_course_quantitative,
    math_course_writing
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
    "Breadth Category",
    "Quantitative Requirement",
    "Writing Requirement"
  )

# Economics Courses
econ_course_link <-
  "https://www.sfu.ca/students/calendar/courses/econ"
econ_course_page <- read_html(econ_course_link)

econ_courses <-
  html_nodes(econ_course_page, ".main > h3") %>%
  html_text() %>% str_squish()
econ_course_code <-
  gsub("(ECON\\s[0-9]{3}[A-Z]?)\\s\\-\\s(.*)(\\([0-9]*\\))?",
       "\\1",
       econ_courses)
econ_course_name <-
  gsub(
    "(ECON\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(\\([0-9]\\))?",
    "\\2",
    econ_courses
  ) %>%
  str_squish()
econ_course_credit_amount <-
  gsub(
    "(ECON\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(?:\\(([0-9]*)\\))?",
    "\\3",
    econ_courses
  ) %>%
  str_squish() %>% as.numeric() %>% str_replace_na("0") %>% as.numeric()

econ_course_information <-
  html_nodes(econ_course_page, ".main > p") %>%
  html_text() %>% str_squish()

econ_course_prereq <-
  gsub("(.*)(Prerequisite.*?\\.)(.*)",
       "\\2",
       econ_course_information)
econ_course_prereq <-
  grep("Prerequisite", econ_course_prereq, invert = TRUE) %>%
  replace(econ_course_prereq, ., "")

econ_course_coreq <-
  gsub("(.*)(Corequisite.*?\\.)(.*)",
       "\\2",
       econ_course_information)
econ_course_coreq <-
  grep("Corequisite", econ_course_coreq, invert = TRUE) %>%
  replace(econ_course_coreq, ., "")

econ_course_antireq <-
  gsub(
    "(.*)((?:Students\\swith\\scredit\\sfor.*?\\.)|(Students\\swho\\shave\\staken.*?\\.))(.*)",
    "\\2",
    econ_course_information
  )
econ_course_antireq <-
  grep(
    "(Students\\swith\\scredit\\sfor)|(Students\\swho\\shave\\staken)",
    econ_course_antireq,
    invert = TRUE
  ) %>%
  replace(econ_course_antireq, ., "")

econ_course_breadth <-
  gsub("(.*)(Breadth.*?\\.)(.*)",
       "\\2",
       econ_course_information)
econ_course_breadth <-
  grep("Breadth", econ_course_breadth, invert = TRUE) %>%
  replace(econ_course_breadth, ., "")

econ_course_quantitative <-
  grepl("Quantitative(?:\\.|\\/)", econ_course_information)
econ_course_writing <-
  grepl("Writing(?:\\.|\\/)", econ_course_information)

econ_course_description <-
  str_remove_all(
    econ_course_information,
    "(Prerequisite.*?\\.)|(Corequisite.*?\\.)|(Students\\swith\\scredit\\sfor.*?\\.)|(Breadth.*?\\.)|(Quantitative(?:\\.|\\/))|(Writing(?:\\.|\\/))|(Students\\swho\\shave\\staken.*?\\.)"
  ) %>%
  str_squish()

econ_course_calendar <-
  data.frame(
    econ_course_code,
    econ_course_name,
    econ_course_description,
    econ_course_credit_amount,
    econ_course_antireq,
    econ_course_coreq,
    econ_course_prereq,
    econ_course_breadth,
    econ_course_quantitative,
    econ_course_writing
  )
colnames(econ_course_calendar) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Breadth Category",
    "Quantitative Requirement",
    "Writing Requirement"
  )

# Statistics Courses
stat_course_link <-
  "https://www.sfu.ca/students/calendar/courses/stat"
stat_course_page <- read_html(stat_course_link)

stat_courses <-
  html_nodes(stat_course_page, ".main > h3") %>%
  html_text() %>% str_squish()
stat_course_code <-
  gsub("(STAT\\s[0-9]{3}[A-Z]?)\\s\\-\\s(.*)(\\([0-9]*\\))?",
       "\\1",
       stat_courses)
stat_course_name <-
  gsub(
    "(STAT\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(\\([0-9]\\))?",
    "\\2",
    stat_courses
  ) %>%
  str_squish()
stat_course_credit_amount <-
  gsub(
    "(STAT\\s[0-9]{3}[A-Z]?)\\s\\-\\s([^\\()0-9]*)(?:\\(([0-9]*)\\))?",
    "\\3",
    stat_courses
  ) %>%
  str_squish() %>% as.numeric() %>% str_replace_na("0") %>% as.numeric()

stat_course_information <-
  html_nodes(stat_course_page, ".main > p") %>%
  html_text() %>% str_squish()

stat_course_prereq <-
  gsub("(.*)(Prerequisite.*?\\.)(.*)",
       "\\2",
       stat_course_information)
stat_course_prereq <-
  grep("Prerequisite", stat_course_prereq, invert = TRUE) %>%
  replace(stat_course_prereq, ., "")

stat_course_coreq <-
  gsub("(.*)(Corequisite.*?\\.)(.*)",
       "\\2",
       stat_course_information)
stat_course_coreq <-
  grep("Corequisite", stat_course_coreq, invert = TRUE) %>%
  replace(stat_course_coreq, ., "")

stat_course_antireq <-
  gsub(
    "(.*)((?:Students\\swith\\scredit\\sfor.*?\\.)|(?:Students\\swho\\shave\\staken.*?\\.)|(?:Students\\s(?:cannot|may\\snot)\\sobtain\\scredit.*?\\.))(.*)",
    "\\2",
    stat_course_information
  )
stat_course_antireq <-
  grep(
    "(Students\\swith\\scredit\\sfor)|(Students\\swho\\shave\\staken)|(Students\\s(?:cannot|may\\snot)\\sobtain\\scredit)",
    stat_course_antireq,
    invert = TRUE
  ) %>%
  replace(stat_course_antireq, ., "")

stat_course_breadth <-
  gsub("(.*)(Breadth.*?\\.)(.*)",
       "\\2",
       stat_course_information)
stat_course_breadth <-
  grep("Breadth", stat_course_breadth, invert = TRUE) %>%
  replace(stat_course_breadth, ., "")

stat_course_quantitative <-
  grepl("Quantitative(\\.|\\/)", stat_course_information)
stat_course_writing <-
  grepl("Writing(\\.|\\/)", stat_course_information)

stat_course_description <-
  str_remove_all(
    stat_course_information,
    "(Prerequisite.*?\\.)|(Corequisite.*?\\.)|(Students\\swith\\scredit\\sfor.*?\\.)|(Breadth.*?\\.)|(Quantitative(\\.|\\/))|(Writing(\\.|\\/))|(Students\\swho\\shave\\staken.*?\\.)|(Students\\s(?:cannot|may\\snot)\\sobtain\\scredit.*?\\.)"
  ) %>%
  str_squish()

stat_course_calendar <-
  data.frame(
    stat_course_code,
    stat_course_name,
    stat_course_description,
    stat_course_credit_amount,
    stat_course_antireq,
    stat_course_coreq,
    stat_course_prereq,
    stat_course_breadth,
    stat_course_quantitative,
    stat_course_writing
  )
colnames(stat_course_calendar) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Breadth Category",
    "Quantitative Requirement",
    "Writing Requirement"
  )

# All Courses

course_calendar <-
  rbind(
    business_course_calendar,
    computer_science_course_calendar,
    math_and_cs_course_calendar,
    math_course_calendar,
    data_course_calendar,
    econ_course_calendar,
    stat_course_calendar
  )

# Program Requirements ####

program_link <-
  "https://www.sfu.ca/students/calendar/programs/data-science/major/bachelor-of-science/"
program_page <- read_html(program_link)

program_information <-
  html_nodes(program_page,
             ".course > a, #page-content > section > p, .course+ h3") %>%
  html_text() %>% str_squish() %>%
  str_extract_all(
    "((BUS|CMPT|MACM|DATA|MATH|STAT|ECON)\\s([0-9]{3}))|(Upper\\sDivision.*)|(.*Students\\scomplete.*)|((one|both|all)\\sof.*)"
  ) %>%
  unlist() %>% str_remove_all("Students\\scomplete.*units.*") %>% stri_remove_empty()

recomended_courses_index <- grep("Recommended", program_information)
recomended_courses <-
  program_information[rep(recomended_courses_index:length(program_information))]

required_courses <-
  program_information[rep(1:(recomended_courses_index - 1))]

requirement_category <- vector(mode = "character")
category_description <- vector(mode = "character")
category_min <- vector(mode = "numeric")
category_max <- vector(mode = "numeric")
isCore <- vector(mode = "logical")

letter <- "L"
current_category <- ""
courses_in_category <- ""

i <- 0
j <- 0
k <- 1
for (item in required_courses) {
  if (grepl("Upper", item)) {
    letter <- "U"
    j <- 1
  } else if (grepl("((BUS|CMPT|MACM|DATA|MATH|STAT|ECON)\\s([0-9]{3}))",
                   item)) {
    current_category <- paste(current_category, item, ", ", sep = "")
    courses_in_category[k] <- item
    k <- k + 1
  } else {
    if (current_category != "") {
      requirement_category[i] <- paste0("Category ", letter, j)
      category_description[i] <- current_category
      credits <-
        filter(course_calendar, `Course Code` %in% courses_in_category) %>%
        select(`Credit Amount`) %>% as.vector %>% unlist()
      if (grepl("(A|a)ll", current_category)) {
        category_min[i] <- sum(credits)
        category_max[i] <- sum(credits)
        isCore[i] = TRUE
      } else if (grepl("(O|o)ne", current_category)) {
        category_min[i] <- min(credits)
        category_max[i] <- max(credits)
        isCore[i] <-
          (category_max == category_min) &
          (category_max == sum(credits))
      } else if (grepl("(T|t)wo", current_category)) {
        credits <- sort(credits)
        category_min[i] <- credits[1] + credits[2]
        category_max[i] <-
          credits[length(credits)] + credits[length(credits) - 1]
        isCore[i] <-
          (category_max == category_min) &
          (category_max == sum(credits))
      }
    }
    current_category <-
      item %>% str_to_sentence() %>% paste0(., ": ")
    i <- i + 1
    j <- j + 1
    k <- 1
  }
}

program_requirements <-
  data.frame(requirement_category,
             category_description,
             category_min,
             category_max,
             isCore)
