# Course Calendar Scraping for Wilfrid Laurier University

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# Business Courses ####

business_link <-
  curl(
    "https://academic-calendar.wlu.ca/department.php?cal=1&d=2617&s=1036&y=85#Course_Offerings",
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  )
business_page <- read_html(business_link)

business_table <-
  html_nodes(business_page, "#main > div.content > div > table") %>%
  html_table() %>% .[[1]]
colnames(business_table) <-
  c("Course Code", "Course Name", "Credit Amount")

business_course_links <-
  html_nodes(business_page, "#main > div.content > div > table > tr > td > a") %>%
  html_attr("href")

num_business_courses <- nrow(business_table)
business_hours <- vector(mode = "character", num_business_courses)
business_description <-
  vector(mode = "character", num_business_courses)
business_lecture <- vector(mode = "logical", num_business_courses)
business_lab <- vector(mode = "logical", num_business_courses)
business_tutorial <- vector(mode = "logical", num_business_courses)
business_antireq <- vector(mode = "character", num_business_courses)
business_coreq <- vector(mode = "character", num_business_courses)
business_prereq <- vector(mode = "character", num_business_courses)
business_note <- vector("character", num_business_courses)

i <- 1
while (i <= num_business_courses) {
  course_page <-
    read_html(curl(
      paste0(
        "https://academic-calendar.wlu.ca/",
        business_course_links[i]
      ),
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    ))
  business_hours[i] <-
    html_nodes(course_page, "#main > div.content > div.hours") %>% html_text() %>% paste0("") %>% str_squish()
  business_lecture[i] <- grepl("Lecture", business_hours[i])
  business_lab[i] <- grepl("Lab", business_hours[i])
  business_tutorial[i] <- grepl("Tutorial", business_hours[i])
  business_description[i] <-
    html_nodes(course_page, "#main > div.content > p") %>% html_text %>% paste0(collapse = "") %>% str_squish()
  other_info <-
    html_nodes(
      course_page,
      "#main > div.content > div.reqs > dl > dt, #main > div.content > div.reqs > dl > dd.required"
    ) %>%
    html_text() %>% str_squish()
  business_prereq[i] <-
    grep("Prerequisites", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  business_antireq[i] <-
    grep("Exclusions", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  business_coreq[i] <-
    grep("Co\\-requisites", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  business_note[i] <-
    grep("Note(s|)", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  i <- i + 1
}

business_credits <-
  select(business_table, `Credit Amount`) %>% unlist() %>% unname() * 6

business_calendar <-
  data.frame(
    business_table$`Course Code`,
    business_table$`Course Name`,
    business_description,
    business_credits,
    business_antireq,
    business_coreq,
    business_prereq,
    business_hours,
    business_lecture,
    business_lab,
    business_tutorial,
    business_note
  )

colnames(business_calendar) <-  c(
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

# Math and Statistics Courses ####

math_stat_link <-
  curl(
    "https://academic-calendar.wlu.ca/department.php?cal=1&d=2578&s=1034&y=85#Course_Offerings",
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  )
math_stat_page <- read_html(math_stat_link)

math_stat_table <-
  html_nodes(math_stat_page, "#main > div.content > div > table") %>%
  html_table() %>% .[[1]]
colnames(math_stat_table) <-
  c("Course Code", "Course Name", "Credit Amount")

math_stat_course_links <-
  html_nodes(math_stat_page, "#main > div.content > div > table > tr > td > a") %>%
  html_attr("href")

num_math_stat_courses <- nrow(math_stat_table)
math_stat_hours <- vector(mode = "character", num_math_stat_courses)
math_stat_description <-
  vector(mode = "character", num_math_stat_courses)
math_stat_lecture <- vector(mode = "logical", num_math_stat_courses)
math_stat_lab <- vector(mode = "logical", num_math_stat_courses)
math_stat_tutorial <-
  vector(mode = "logical", num_math_stat_courses)
math_stat_antireq <-
  vector(mode = "character", num_math_stat_courses)
math_stat_coreq <- vector(mode = "character", num_math_stat_courses)
math_stat_prereq <-
  vector(mode = "character", num_math_stat_courses)
math_stat_note <- vector("character", num_math_stat_courses)

i <- 1
while (i <= num_math_stat_courses) {
  course_page <-
    read_html(curl(
      paste0(
        "https://academic-calendar.wlu.ca/",
        math_stat_course_links[i]
      ),
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    ))
  math_stat_hours[i] <-
    html_nodes(course_page, "#main > div.content > div.hours") %>% html_text() %>% paste0("") %>% str_squish()
  math_stat_lecture[i] <- grepl("Lecture", math_stat_hours[i])
  math_stat_lab[i] <- grepl("Lab", math_stat_hours[i])
  math_stat_tutorial[i] <- grepl("Tutorial", math_stat_hours[i])
  math_stat_description[i] <-
    html_nodes(course_page, "#main > div.content > p") %>% html_text %>% paste0(collapse = "") %>% str_squish()
  other_info <-
    html_nodes(
      course_page,
      "#main > div.content > div.reqs > dl > dt, #main > div.content > div.reqs > dl > dd.required"
    ) %>%
    html_text() %>% str_squish()
  math_stat_prereq[i] <-
    grep("Prerequisites", other_info) %>% +1 %>% other_info[.] %>% paste0("")
  math_stat_antireq[i] <-
    grep("Exclusions", other_info) %>% +1 %>% other_info[.] %>% paste0("")
  math_stat_coreq[i] <-
    grep("Co\\-requisites", other_info) %>% +1 %>% other_info[.] %>% paste0("")
  math_stat_note[i] <-
    grep("Note(s|)", other_info) %>% +1 %>% other_info[.] %>% paste0("")
  i <- i + 1
}

closeAllConnections()
math_stat_credits <-
  select(math_stat_table, `Credit Amount`) %>% unlist() %>% unname() * 6

math_stat_calendar <-
  data.frame(
    math_stat_table$`Course Code`,
    math_stat_table$`Course Name`,
    math_stat_description,
    math_stat_credits,
    math_stat_antireq,
    math_stat_coreq,
    math_stat_prereq,
    math_stat_hours,
    math_stat_lecture,
    math_stat_lab,
    math_stat_tutorial,
    math_stat_note
  )

colnames(math_stat_calendar) <-  c(
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

# Computer Science Courses ####

cs_link <-
  curl(
    "https://academic-calendar.wlu.ca/department.php?cal=1&d=2576&s=1034&y=85#Course_Offerings",
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  )
cs_page <- read_html(cs_link)

cs_table <-
  html_nodes(cs_page, "#main > div.content > div > table") %>%
  html_table() %>% .[[1]]
colnames(cs_table) <-
  c("Course Code", "Course Name", "Credit Amount")

cs_course_links <-
  html_nodes(cs_page, "#main > div.content > div > table > tr > td > a") %>%
  html_attr("href")

num_cs_courses <- nrow(cs_table)
cs_hours <- vector(mode = "character", num_cs_courses)
cs_description <-
  vector(mode = "character", num_cs_courses)
cs_lecture <- vector(mode = "logical", num_cs_courses)
cs_lab <- vector(mode = "logical", num_cs_courses)
cs_tutorial <-
  vector(mode = "logical", num_cs_courses)
cs_antireq <-
  vector(mode = "character", num_cs_courses)
cs_coreq <- vector(mode = "character", num_cs_courses)
cs_prereq <-
  vector(mode = "character", num_cs_courses)
cs_note <- vector("character", num_cs_courses)

i <- 1
while (i <= num_cs_courses) {
  course_page <-
    read_html(curl(
      paste0("https://academic-calendar.wlu.ca/",
             cs_course_links[i]),
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    ))
  cs_hours[i] <-
    html_nodes(course_page, "#main > div.content > div.hours") %>% html_text() %>% paste0("") %>% str_squish()
  cs_lecture[i] <- grepl("Lecture", cs_hours[i])
  cs_lab[i] <- grepl("Lab", cs_hours[i])
  cs_tutorial[i] <- grepl("Tutorial", cs_hours[i])
  cs_description[i] <-
    html_nodes(course_page, "#main > div.content > p") %>% html_text %>% paste0(collapse = "") %>% str_squish()
  other_info <-
    html_nodes(
      course_page,
      "#main > div.content > div.reqs > dl > dt, #main > div.content > div.reqs > dl > dd.required"
    ) %>%
    html_text() %>% str_squish()
  cs_prereq[i] <-
    grep("Prerequisites", other_info) %>% +1 %>% other_info[.] %>% paste0("")
  cs_antireq[i] <-
    grep("Exclusions", other_info) %>% +1 %>% other_info[.] %>% paste0("")
  cs_coreq[i] <-
    grep("Co\\-requisites", other_info) %>% +1 %>% other_info[.] %>% paste0("")
  cs_note[i] <-
    grep("Note(s|)", other_info) %>% +1 %>% other_info[.] %>% paste0("")
  i <- i + 1
}

closeAllConnections()
cs_credits <-
  select(cs_table, `Credit Amount`) %>% unlist() %>% unname() * 6

cs_calendar <-
  data.frame(
    cs_table$`Course Code`,
    cs_table$`Course Name`,
    cs_description,
    cs_credits,
    cs_antireq,
    cs_coreq,
    cs_prereq,
    cs_hours,
    cs_lecture,
    cs_lab,
    cs_tutorial,
    cs_note
  )

colnames(cs_calendar) <-  c(
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

# Data Courses ####

data_link <-
  curl(
    "https://academic-calendar.wlu.ca/department.php?cal=1&d=2589&s=1034&y=85#Course_Offerings",
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  )
data_page <- read_html(data_link)

data_table <-
  html_nodes(data_page, "#main > div.content > div > table") %>%
  html_table() %>% .[[1]]
colnames(data_table) <-
  c("Course Code", "Course Name", "Credit Amount")

data_course_links <-
  html_nodes(data_page, "#main > div.content > div > table > tr > td > a") %>%
  html_attr("href")

num_data_courses <- nrow(data_table)
data_hours <- vector(mode = "character", num_data_courses)
data_description <-
  vector(mode = "character", num_data_courses)
data_lecture <- vector(mode = "logical", num_data_courses)
data_lab <- vector(mode = "logical", num_data_courses)
data_tutorial <- vector(mode = "logical", num_data_courses)
data_antireq <- vector(mode = "character", num_data_courses)
data_coreq <- vector(mode = "character", num_data_courses)
data_prereq <- vector(mode = "character", num_data_courses)
data_note <- vector("character", num_data_courses)

i <- 1
while (i <= num_data_courses) {
  course_page <-
    read_html(curl(
      paste0("https://academic-calendar.wlu.ca/",
             data_course_links[i]),
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    ))
  data_hours[i] <-
    html_nodes(course_page, "#main > div.content > div.hours") %>% html_text() %>% paste0("") %>% str_squish()
  data_lecture[i] <- grepl("Lecture", data_hours[i])
  data_lab[i] <- grepl("Lab", data_hours[i])
  data_tutorial[i] <- grepl("Tutorial", data_hours[i])
  data_description[i] <-
    html_nodes(course_page, "#main > div.content > p") %>% html_text %>% paste0(collapse = "") %>% str_squish()
  other_info <-
    html_nodes(
      course_page,
      "#main > div.content > div.reqs > dl > dt, #main > div.content > div.reqs > dl > dd.required"
    ) %>%
    html_text() %>% str_squish()
  data_prereq[i] <-
    grep("Prerequisites", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  data_antireq[i] <-
    grep("Exclusions", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  data_coreq[i] <-
    grep("Co\\-requisites", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  data_note[i] <-
    grep("Note(s|)", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  i <- i + 1
}

data_credits <-
  select(data_table, `Credit Amount`) %>% unlist() %>% unname() * 6

data_calendar <-
  data.frame(
    data_table$`Course Code`,
    data_table$`Course Name`,
    data_description,
    data_credits,
    data_antireq,
    data_coreq,
    data_prereq,
    data_hours,
    data_lecture,
    data_lab,
    data_tutorial,
    data_note
  )

colnames(data_calendar) <-  c(
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

# Write CSV Files ####

write.csv(business_calendar,
          "Wilfrid Laurier University Business Course Calendar.csv")
write.csv(
  math_stat_calendar,
  "Wilfrid Laurier University Mathematics and Statistics Course Calendar.csv"
)
write.csv(cs_calendar,
          "Wilfrid Laurier University Computer Science Course Calendar.csv")
write.csv(data_calendar,
          "Wilfrid Laurier University Data Science Course Calendar.csv")
