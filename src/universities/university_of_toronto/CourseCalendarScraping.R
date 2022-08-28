# University of Toronto Course Calendar Scraping ####

# Scraps course calendars for subject areas which can be used to fill some of the
# data science degree requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Computer Science Course Calendar ####

# Read webpage with course information
cs_page <-
  read_html(
    curl(
      "https://artsci.calendar.utoronto.ca/section/Computer-Science",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

# Gets course code and name
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

# Vectors for course information
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
cs_seminar <- vector(mode = "logical", length = cs_num_courses)
cs_tutorial <- vector(mode = "logical", length = cs_num_courses)

i <- 1
# The loop scrapes data from each course and sorts the data into the correct vector
# using regex
while (i <= cs_num_courses) {
  # Gets course description
  cs_course_text <- html_nodes(
    cs_page,
    paste0(
      "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div:nth-child(",
      i,
      ") > div > div"
    )
  ) %>%
    html_text() %>% str_squish() %>% paste(collapse = ". ") %>%
    str_split("((Note(s|))|(NOTE(S|))):") %>% unlist()
  cs_course_description[i] <- cs_course_text[1] %>% paste("")
  cs_note[i] <-
    if_else(is.na(cs_course_text[2]), "", cs_course_text[2] %>% paste("")) %>%
    str_squish()
  cs_credit_amount[i] <- ifelse(grepl("Y", cs_course_code[i]), 6, 3)
  # Other course information (everything except description)
  cs_course_info <- html_nodes(
    cs_page,
    paste0(
      "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div:nth-child(",
      i,
      ") > div > span"
    )
  ) %>% html_text() %>% str_squish()
  cs_antireq[i] <-
    grep("Exclusion:", cs_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Exclusion:") %>% str_replace_all("\\/", " or") %>% str_squish()
  cs_coreq[i] <-
    grep("Corequisite:", cs_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Corequisite:") %>% str_replace_all("\\/", " or") %>% str_squish()
  cs_prereq[i] <-
    grep("Prerequisite:", cs_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Prerequisite:") %>% str_replace_all("\\/", " or") %>% str_squish()
  cs_rec_prep[i] <-
    grep("Recommended Preparation:", cs_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Recommended Preparation:") %>% str_replace_all("\\/", " or") %>% str_squish()
  cs_breadth[i] <-
    grep("Breadth Requirements:", cs_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Breadth Requirements:") %>% str_squish()
  cs_dist[i] <-
    grep("Distribution Requirements:", cs_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Distribution Requirements:") %>% str_squish()
  cs_mode[i] <-
    grep("Mode of Delivery:", cs_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Mode of Delivery:") %>% str_squish()
  cs_hours[i] <-
    grep("Hours:", cs_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Hours:") %>% str_squish()
  i <- i + 1
}

# Generats logical vectors based on if the course has labs, lectures, etc.
cs_lecture <- grepl("L", cs_hours)
cs_lab <- grepl("P", cs_hours)
cs_seminar <- grepl("S", cs_hours)
cs_tutorial <- grepl("T", cs_hours)

# Creates course calendar for CS courses
cs_course_calendar <-
  data.frame(
    cs_course_code,
    cs_course_name,
    cs_course_description,
    cs_credit_amount,
    cs_antireq,
    cs_coreq,
    cs_prereq,
    cs_rec_prep,
    cs_hours,
    cs_lecture,
    cs_lab,
    cs_seminar,
    cs_tutorial,
    cs_breadth,
    cs_dist,
    cs_mode,
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
    "Recommended Preperation",
    "Hours",
    "Lecture",
    "Lab",
    "Seminar",
    "Tutorial",
    "Breadth Requirement",
    "Distribution Requirement",
    "Delivery Format",
    "Note"
  )

# Note that the following code for the other course calendars is functionally
# the same as the above code, so some comments are omitted. The only changes
# are to some of the regular expressions depending on how the information is
# displayed on the website.

# Mathematics Course Calendar ####

# Read webpage with course information
math_page <-
  read_html(
    curl(
      "https://artsci.calendar.utoronto.ca/section/Mathematics#courses",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

# Gets course code and name
math_course_title <-
  html_nodes(
    math_page,
    "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div > h3"
  ) %>%
  html_text()

math_course_code <-
  str_extract_all(math_course_title, "(?:MAT|APM|JUM)[0-9]{3}(Y|H)[0-9]{1}") %>% unlist()

math_course_name <-
  gsub(
    "((?:MAT|APM|JUM)[0-9]{3}(?:Y|H)[0-9]{1})(\\s\\-\\s)(.*)",
    "\\3",
    math_course_title
  ) %>%
  str_squish()

math_num_courses <- length(math_course_title)

# Vectors for course information
math_course_description <-
  vector(mode = "character", length = math_num_courses)
math_note <- vector(mode = "character", length = math_num_courses)
math_credit_amount <-
  vector(mode = "numeric", length = math_num_courses)
math_antireq <-
  vector(mode = "character", length = math_num_courses)
math_coreq <- vector(mode = "character", length = math_num_courses)
math_prereq <- vector(mode = "character", length = math_num_courses)
math_hours <- vector(mode = "character", length = math_num_courses)
math_mode <- vector(mode = "character", length = math_num_courses)
math_dist <- vector(mode = "character", length = math_num_courses)
math_breadth <-
  vector(mode = "character", length = math_num_courses)
math_rec_prep <-
  vector(mode = "character", length = math_num_courses)
math_lecture <- vector(mode = "logical", length = math_num_courses)
math_lab <- vector(mode = "logical", length = math_num_courses)
math_seminar <- vector(mode = "logical", length = math_num_courses)
math_tutorial <- vector(mode = "logical", length = math_num_courses)

i <- 1
# The loop scrapes data from each course and sorts the data into the correct vector
while (i <= math_num_courses) {
  math_course_text <- html_nodes(
    math_page,
    paste0(
      "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div:nth-child(",
      i,
      ") > div > div"
    )
  ) %>%
    html_text() %>% str_squish() %>% paste(collapse = ". ") %>% str_split("((Note(s|))|(NOTE(S|))):") %>% unlist()
  math_course_description[i] <- math_course_text[1] %>% paste("")
  math_note[i] <-
    if_else(is.na(math_course_text[2]),
            "",
            math_course_text[2] %>% paste("")) %>%
    str_squish()
  math_credit_amount[i] <-
    ifelse(grepl("Y", math_course_code[i]), 6, 3)
  math_course_info <- html_nodes(
    math_page,
    paste0(
      "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div:nth-child(",
      i,
      ") > div > span"
    )
  ) %>%
    html_text() %>% str_squish()
  math_antireq[i] <-
    grep("Exclusion:", math_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Exclusion:") %>% str_replace_all("\\/", " or") %>% str_squish()
  math_coreq[i] <-
    grep("Corequisite:", math_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Corequisite:") %>% str_replace_all("\\/", " or") %>% str_squish()
  math_prereq[i] <-
    grep("Prerequisite:", math_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Prerequisite:") %>% str_replace_all("\\/", " or") %>% str_squish()
  math_rec_prep[i] <-
    grep("Recommended Preparation:", math_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Recommended Preparation:") %>% str_replace_all("\\/", " or") %>% str_squish()
  math_breadth[i] <-
    grep("Breadth Requirements:", math_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Breadth Requirements:") %>% str_squish()
  math_dist[i] <-
    grep("Distribution Requirements:", math_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Distribution Requirements:") %>% str_squish()
  math_mode[i] <-
    grep("Mode of Delivery:", math_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Mode of Delivery:") %>% str_squish()
  math_hours[i] <-
    grep("Hours:", math_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Hours:") %>% str_squish()
  i <- i + 1
}

math_lecture <- grepl("L", math_hours)
math_lab <- grepl("P", math_hours)
math_seminar <- grepl("S", math_hours)
math_tutorial <- grepl("T", math_hours)

math_course_calendar <-
  data.frame(
    math_course_code,
    math_course_name,
    math_course_description,
    math_credit_amount,
    math_antireq,
    math_coreq,
    math_prereq,
    math_rec_prep,
    math_hours,
    math_lecture,
    math_lab,
    math_seminar,
    math_tutorial,
    math_breadth,
    math_dist,
    math_mode,
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
    "Recommended Preperation",
    "Hours",
    "Lecture",
    "Lab",
    "Seminar",
    "Tutorial",
    "Breadth Requirement",
    "Distribution Requirement",
    "Delivery Format",
    "Note"
  )

# Statistics Course Calendar ####

# Read webpage with course information
stat_page <-
  read_html(
    curl(
      "https://artsci.calendar.utoronto.ca/section/Statistical-Sciences#courses",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

# Gets course code and name
stat_course_title <-
  html_nodes(
    stat_page,
    "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div > h3"
  ) %>%
  html_text()

stat_course_code <-
  str_extract_all(stat_course_title, "(?:STA|JSC)[0-9]{3}(Y|H)[0-9]{1}") %>% unlist()

stat_course_name <-
  gsub("((?:STA|JSC)[0-9]{3}(?:Y|H)[0-9]{1})(\\s\\-\\s)(.*)",
       "\\3",
       stat_course_title) %>%
  str_squish()

stat_num_courses <- length(stat_course_title)

# Vectors for course information
stat_course_description <-
  vector(mode = "character", length = stat_num_courses)
stat_note <- vector(mode = "character", length = stat_num_courses)
stat_credit_amount <-
  vector(mode = "numeric", length = stat_num_courses)
stat_antireq <-
  vector(mode = "character", length = stat_num_courses)
stat_coreq <- vector(mode = "character", length = stat_num_courses)
stat_prereq <- vector(mode = "character", length = stat_num_courses)
stat_hours <- vector(mode = "character", length = stat_num_courses)
stat_mode <- vector(mode = "character", length = stat_num_courses)
stat_dist <- vector(mode = "character", length = stat_num_courses)
stat_breadth <-
  vector(mode = "character", length = stat_num_courses)
stat_rec_prep <-
  vector(mode = "character", length = stat_num_courses)
stat_lecture <- vector(mode = "logical", length = stat_num_courses)
stat_lab <- vector(mode = "logical", length = stat_num_courses)
stat_seminar <- vector(mode = "logical", length = stat_num_courses)
stat_tutorial <- vector(mode = "logical", length = stat_num_courses)

i <- 1
# The loop scrapes data from each course and sorts the data into the correct vector
while (i <= stat_num_courses) {
  stat_course_text <- html_nodes(
    stat_page,
    paste0(
      "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div:nth-child(",
      i,
      ") > div > div"
    )
  ) %>%
    html_text() %>% str_squish() %>% paste(collapse = ". ") %>% str_split("((Note(s|))|(NOTE(S|))):") %>% unlist()
  stat_course_description[i] <- stat_course_text[1] %>% paste("")
  stat_note[i] <-
    if_else(is.na(stat_course_text[2]),
            "",
            stat_course_text[2] %>% paste("")) %>%
    str_squish()
  stat_credit_amount[i] <-
    ifelse(grepl("Y", stat_course_code[i]), 6, 3)
  stat_course_info <- html_nodes(
    stat_page,
    paste0(
      "#block-fas-content > div > div > div > div.view-footer > div.view.view-courses-view.view-id-courses_view > div.view-content > div:nth-child(",
      i,
      ") > div > span"
    )
  ) %>%
    html_text() %>% str_squish()
  stat_antireq[i] <-
    grep("Exclusion:", stat_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Exclusion:") %>% str_replace_all("\\/", " or") %>% str_squish()
  stat_coreq[i] <-
    grep("Corequisite:", stat_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Corequisite:") %>% str_replace_all("\\/", " or") %>% str_squish()
  stat_prereq[i] <-
    grep("Prerequisite:", stat_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Prerequisite:") %>% str_replace_all("\\/", " or") %>% str_squish()
  stat_rec_prep[i] <-
    grep("Recommended Preparation:", stat_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Recommended Preparation:") %>% str_replace_all("\\/", " or") %>% str_squish()
  stat_breadth[i] <-
    grep("Breadth Requirements:", stat_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Breadth Requirements:") %>% str_squish()
  stat_dist[i] <-
    grep("Distribution Requirements:", stat_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Distribution Requirements:") %>% str_squish()
  stat_mode[i] <-
    grep("Mode of Delivery:", stat_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Mode of Delivery:") %>% str_squish()
  stat_hours[i] <-
    grep("Hours:", stat_course_info, value = TRUE) %>% paste0(collapse = "") %>%
    str_remove("Hours:") %>% str_squish()
  i <- i + 1
}

stat_lecture <- grepl("L", stat_hours)
stat_lab <- grepl("P", stat_hours)
stat_seminar <- grepl("S", stat_hours)
stat_tutorial <- grepl("T", stat_hours)

stat_course_calendar <-
  data.frame(
    stat_course_code,
    stat_course_name,
    stat_course_description,
    stat_credit_amount,
    stat_antireq,
    stat_coreq,
    stat_prereq,
    stat_rec_prep,
    stat_hours,
    stat_lecture,
    stat_lab,
    stat_seminar,
    stat_tutorial,
    stat_breadth,
    stat_dist,
    stat_mode,
    stat_note
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
    "Recommended Preperation",
    "Hours",
    "Lecture",
    "Lab",
    "Seminar",
    "Tutorial",
    "Breadth Requirement",
    "Distribution Requirement",
    "Delivery Format",
    "Note"
  )

# Write CSV Files ####

# write.csv(cs_course_calendar,
#           "University of Toronto Computer Science Course Calendar.csv")
# write.csv(math_course_calendar,
#           "University of Toronto Mathematics Course Calendar.csv")
# write.csv(stat_course_calendar,
#           "University of Toronto Statistics Course Calendar.csv")
