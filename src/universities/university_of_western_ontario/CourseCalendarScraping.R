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
cs_hours <- vector(mode = "character", length = cs_num_courses)
cs_notes <- vector(mode = "character", length = cs_num_courses)
cs_breadth <- vector(mode = "character", length = cs_num_courses)

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
  cs_other_info <-
    grep("Extra\\sInformation", course_info, value = TRUE) %>%
    str_split("\\.") %>% unlist() %>% str_remove_all("Extra\\sInformation(:)?") %>%
    str_squish()
  cs_hours[i] <- cs_other_info[1] %>%
    str_squish() %>% unlist() %>% paste0(collapse = "")
  cs_notes[i] <- grep("Note", cs_other_info, value = TRUE) %>%
    str_remove("Note:") %>% str_squish() %>% unlist() %>% paste0(collapse = "")
  cs_breadth[i] <- grep("Breadth:", course_info, value = TRUE) %>%
    str_extract("CATEGORY\\s[A-Z]") %>% str_to_title()
  i <- i + 1
}

cs_lecture <- grepl("lecture", cs_hours)
cs_lab <- grepl("laboratory", cs_hours)
cs_tutorial <-
  grepl("(?<!\\/)(tutorial)(?!\\/)", cs_hours, perl = TRUE)

cs_credit_amount <- cs_credit_amount * 6

cs_course_calendar <- data.frame(
  cs_course_code,
  cs_course_name,
  cs_course_description,
  cs_credit_amount,
  cs_antireq,
  cs_coreq,
  cs_prereq,
  cs_hours,
  cs_lecture,
  cs_lab,
  cs_tutorial,
  cs_breadth,
  cs_notes
)
colnames(cs_course_calendar) <- c(
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
  "Breadth Requirement",
  "Note"
)

# Data Science Course Calendar ####

data_page <-
  read_html(
    curl(
      "https://www.westerncalendar.uwo.ca/Courses.cfm?Subject=DATASCI&SelectedCalendar=Live&ArchiveID=",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

data_course_links <-
  html_nodes(data_page, ".col-md-12 .btn-info") %>%
  html_attr("href")

data_course_title <-
  html_nodes(data_page,
             "div.col-md-12 > div.panel-group > div > div.panel-heading") %>%
  html_text() %>% str_squish()
data_course_code <-
  str_extract_all(data_course_title,
                  "Data\\sScience\\s[0-9]{4}([A-Z](\\/)?)*") %>%
  unlist()
data_course_name <-
  gsub("(Data\\sScience\\s[0-9]{4}(?:[A-Z](?:\\/)?)*)(.*)",
       "\\2",
       data_course_title) %>%
  str_squish() %>% str_to_title()

data_num_courses <- length(data_course_links)
data_course_description <-
  vector(mode = "character", length = data_num_courses)
data_credit_amount <-
  vector(mode = "numeric", length = data_num_courses)
data_antireq <-
  vector(mode = "character", length = data_num_courses)
data_coreq <- vector(mode = "character", length = data_num_courses)
data_prereq <- vector(mode = "character", length = data_num_courses)
data_hours <- vector(mode = "character", length = data_num_courses)
data_notes <- vector(mode = "character", length = data_num_courses)
data_breadth <-
  vector(mode = "character", length = data_num_courses)

i <- 1
while (i <= data_num_courses) {
  course_page <- read_html(curl(
    paste0("https://www.westerncalendar.uwo.ca/", data_course_links[i]),
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  ))
  course_info <-
    html_nodes(course_page, "#CourseInformationDiv .col-xs-12") %>%
    html_text() %>% str_squish()
  data_course_description[i] <-
    grep("Course\\sDescription", course_info, value = TRUE) %>%
    str_remove("Course\\sDescription\\s") %>% str_squish()
  data_credit_amount[i] <-
    grep("Course\\sWeight", course_info, value = TRUE) %>%
    str_extract("[0-9]\\.[0-9]+") %>% as.numeric()
  data_antireq[i] <-
    grep("Antirequisite", course_info, value = TRUE) %>%
    str_remove_all("Antirequisite\\(s\\)(:)?") %>% str_squish() %>% paste0(collapse = "")
  pre_co_req <-
    grep("Pre\\sor\\sCorequisites", course_info, value = TRUE) %>%
    str_remove_all("Pre\\sor\\sCorequisites") %>%
    str_split("(\\.)(?![0-9])(?<![0-9])") %>%
    unlist()
  data_coreq[i] <- grep("Corequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Corequisite\\(s\\)(:)?)|(Pre\\-or\\sCorequisite\\(s\\)(:)?)") %>%
    unlist() %>% str_squish() %>% paste0(collapse = "")
  data_prereq[i] <-
    grep("Prerequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Prerequisite\\(s\\)(:)?)") %>% str_squish() %>%
    unlist() %>% paste0(collapse = "")
  data_other_info <-
    grep("Extra\\sInformation", course_info, value = TRUE) %>%
    str_split("\\.") %>% unlist() %>% str_remove_all("Extra\\sInformation(:)?") %>%
    str_squish()
  data_hours[i] <- data_other_info[1] %>%
    str_squish() %>% unlist() %>% paste0(collapse = "")
  data_notes[i] <- grep("Note", data_other_info, value = TRUE) %>%
    str_remove("Note:") %>% str_squish() %>% unlist() %>% paste0(collapse = "")
  data_breadth[i] <-
    grep("Breadth:", course_info, value = TRUE) %>%
    str_extract("CATEGORY\\s[A-Z]") %>% str_to_title()
  i <- i + 1
}

data_lecture <- grepl("lecture", data_hours)
data_lab <- grepl("laboratory|lab", data_hours)
data_tutorial <-
  grepl("(?<!\\/)(tutorial)(?!\\/)", data_hours, perl = TRUE)

data_credit_amount <- data_credit_amount * 6

data_course_calendar <- data.frame(
  data_course_code,
  data_course_name,
  data_course_description,
  data_credit_amount,
  data_antireq,
  data_coreq,
  data_prereq,
  data_hours,
  data_lecture,
  data_lab,
  data_tutorial,
  data_breadth,
  data_notes
)
colnames(data_course_calendar) <- c(
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
  "Breadth Requirement",
  "Note"
)

# Mathematics Sciences Course Calendar ####

math_page <-
  read_html(
    curl(
      "https://www.westerncalendar.uwo.ca/Courses.cfm?Subject=MATH&SelectedCalendar=Live&ArchiveID=",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

math_course_links <-
  html_nodes(math_page, ".col-md-12 .btn-info") %>%
  html_attr("href")

math_course_title <-
  html_nodes(math_page,
             "div.col-md-12 > div.panel-group > div > div.panel-heading") %>%
  html_text() %>% str_squish()
math_course_code <-
  str_extract_all(math_course_title,
                  "Mathematics\\s[0-9]{4}([A-Z](\\/)?)*") %>%
  unlist()
math_course_name <-
  gsub("(Mathematics\\s[0-9]{4}(?:[A-Z](?:\\/)?)*)(.*)",
       "\\2",
       math_course_title) %>%
  str_squish() %>% str_to_title()

math_num_courses <- length(math_course_links)
math_course_description <-
  vector(mode = "character", length = math_num_courses)
math_credit_amount <-
  vector(mode = "numeric", length = math_num_courses)
math_antireq <-
  vector(mode = "character", length = math_num_courses)
math_coreq <- vector(mode = "character", length = math_num_courses)
math_prereq <- vector(mode = "character", length = math_num_courses)
math_hours <- vector(mode = "character", length = math_num_courses)
math_notes <- vector(mode = "character", length = math_num_courses)
math_breadth <-
  vector(mode = "character", length = math_num_courses)

i <- 1
while (i <= math_num_courses) {
  course_page <- read_html(curl(
    paste0("https://www.westerncalendar.uwo.ca/", math_course_links[i]),
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  ))
  course_info <-
    html_nodes(course_page, "#CourseInformationDiv .col-xs-12") %>%
    html_text() %>% str_squish()
  math_course_description[i] <-
    grep("Course\\sDescription", course_info, value = TRUE) %>%
    str_remove("Course\\sDescription\\s") %>% str_squish()
  math_credit_amount[i] <-
    grep("Course\\sWeight", course_info, value = TRUE) %>%
    str_extract("[0-9]\\.[0-9]+") %>% as.numeric()
  math_antireq[i] <-
    grep("Antirequisite", course_info, value = TRUE) %>%
    str_remove_all("Antirequisite\\(s\\)(:)?") %>% str_squish() %>% paste0(collapse = "")
  pre_co_req <-
    grep("Pre\\sor\\sCorequisites", course_info, value = TRUE) %>%
    str_remove_all("Pre\\sor\\sCorequisites") %>%
    str_split("(\\.)(?![0-9])(?<![0-9])") %>%
    unlist()
  math_coreq[i] <- grep("Corequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Corequisite\\(s\\)(:)?)|(Pre\\-(\\s)?or\\sCorequisite\\(s\\)(:)?)") %>%
    unlist() %>% str_squish() %>% paste0(collapse = "")
  math_prereq[i] <-
    grep("Prerequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Prerequisite\\(s\\)(:)?)") %>% str_squish() %>%
    unlist() %>% paste0(collapse = "")
  math_other_info <-
    grep("Extra\\sInformation", course_info, value = TRUE) %>%
    str_split("\\.") %>% unlist() %>% str_remove_all("Extra\\sInformation(:)?") %>%
    str_squish()
  math_hours[i] <- math_other_info[1] %>%
    str_squish() %>% unlist() %>% paste0(collapse = "")
  math_notes[i] <- grep("Note", math_other_info, value = TRUE) %>%
    str_remove("(Note:)|(Note\\sat\\sMain\\scampus:)") %>% str_squish() %>%
    unlist() %>% paste0(collapse = "")
  math_breadth[i] <-
    grep("Breadth:", course_info, value = TRUE) %>%
    str_extract("CATEGORY\\s[A-Z]") %>% str_to_title()
  i <- i + 1
}

math_lecture <- grepl("lecture", math_hours)
math_lab <- grepl("laboratory|lab", math_hours)
math_tutorial <-
  grepl("(?<!\\/)(tutorial)(?!\\/)", math_hours, perl = TRUE)

main_campus_math <-
  html_nodes(math_page, "div > div.panel-heading > a > div > img") %>%
  html_attr("alt") %>% grep("Western", .)

math_credit_amount <- math_credit_amount * 6

math_course_calendar <- data.frame(
  math_course_code,
  math_course_name,
  math_course_description,
  math_credit_amount,
  math_antireq,
  math_coreq,
  math_prereq,
  math_hours,
  math_lecture,
  math_lab,
  math_tutorial,
  math_breadth,
  math_notes
) %>% slice(main_campus_math)
colnames(math_course_calendar) <- c(
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
  "Breadth Requirement",
  "Note"
)

# Software Engineering Course Calendar ####

se_page <-
  read_html(
    curl(
      "https://www.westerncalendar.uwo.ca/Courses.cfm?Subject=SE&SelectedCalendar=Live&ArchiveID=",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

se_course_links <- html_nodes(se_page, ".col-md-12 .btn-info") %>%
  html_attr("href")

se_course_title <-
  html_nodes(se_page,
             "div.col-md-12 > div.panel-group > div > div.panel-heading") %>%
  html_text() %>% str_squish()
se_course_code <-
  str_extract_all(se_course_title,
                  "Software\\sEngineering\\s[0-9]{4}([A-Z](\\/)?)*") %>%
  unlist()
se_course_name <-
  gsub(
    "(Software\\sEngineering\\s[0-9]{4}(?:[A-Z](?:\\/)?)*)(.*)",
    "\\2",
    se_course_title
  ) %>%
  str_squish() %>% str_to_title()

se_num_courses <- length(se_course_links)
se_course_description <-
  vector(mode = "character", length = se_num_courses)
se_credit_amount <-
  vector(mode = "numeric", length = se_num_courses)
se_antireq <- vector(mode = "character", length = se_num_courses)
se_coreq <- vector(mode = "character", length = se_num_courses)
se_prereq <- vector(mode = "character", length = se_num_courses)
se_hours <- vector(mode = "character", length = se_num_courses)
se_notes <- vector(mode = "character", length = se_num_courses)
se_breadth <- vector(mode = "character", length = se_num_courses)

i <- 1
while (i <= se_num_courses) {
  course_page <- read_html(curl(
    paste0("https://www.westerncalendar.uwo.ca/", se_course_links[i]),
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  ))
  course_info <-
    html_nodes(course_page, "#CourseInformationDiv .col-xs-12") %>%
    html_text() %>% str_squish()
  se_course_description[i] <-
    grep("Course\\sDescription", course_info, value = TRUE) %>%
    str_remove("Course\\sDescription\\s") %>% str_squish()
  se_credit_amount[i] <-
    grep("Course\\sWeight", course_info, value = TRUE) %>%
    str_extract("[0-9]\\.[0-9]+") %>% as.numeric()
  se_antireq[i] <-
    grep("Antirequisite", course_info, value = TRUE) %>%
    str_remove_all("Antirequisite\\(s\\)(:)?") %>% str_squish() %>% paste0(collapse = "")
  pre_co_req <-
    grep("Pre\\sor\\sCorequisites", course_info, value = TRUE) %>%
    str_remove_all("Pre\\sor\\sCorequisites") %>%
    str_split("(\\.)(?![0-9])(?<![0-9])") %>%
    unlist()
  se_coreq[i] <- grep("Corequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Corequisite\\(s\\)(:)?)|(Pre\\-or\\sCorequisite\\(s\\)(:)?)") %>%
    unlist() %>% str_squish() %>% paste0(collapse = "")
  se_prereq[i] <- grep("Prerequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Prerequisite\\(s\\)(:)?)") %>% str_squish() %>%
    unlist() %>% paste0(collapse = "")
  se_other_info <-
    grep("Extra\\sInformation", course_info, value = TRUE) %>%
    str_split("\\.") %>% unlist() %>% str_remove_all("Extra\\sInformation(:)?") %>%
    str_squish()
  se_hours[i] <- se_other_info[1] %>%
    str_squish() %>% unlist() %>% paste0(collapse = "")
  se_notes[i] <- grep("Note", se_other_info, value = TRUE) %>%
    str_remove("Note:") %>% str_squish() %>% unlist() %>% paste0(collapse = "")
  se_breadth[i] <- grep("Breadth:", course_info, value = TRUE) %>%
    str_extract("CATEGORY\\s[A-Z]") %>% str_to_title()
  i <- i + 1
}

se_lecture <- grepl("lecture", se_hours)
se_lab <- grepl("laboratory|lab", se_hours)
se_tutorial <-
  grepl("(?<!\\/)(tutorial)(?!\\/)", se_hours, perl = TRUE)

se_credit_amount <- se_credit_amount * 6

se_course_calendar <- data.frame(
  se_course_code,
  se_course_name,
  se_course_description,
  se_credit_amount,
  se_antireq,
  se_coreq,
  se_prereq,
  se_hours,
  se_lecture,
  se_lab,
  se_tutorial,
  se_breadth,
  se_notes
)
colnames(se_course_calendar) <- c(
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
  "Breadth Requirement",
  "Note"
)

# Statistical Sciences Course Calendar ####

stat_page <-
  read_html(
    curl(
      "https://www.westerncalendar.uwo.ca/Courses.cfm?Subject=STATS&SelectedCalendar=Live&ArchiveID=",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

stat_course_links <-
  html_nodes(stat_page, ".col-md-12 .btn-info") %>%
  html_attr("href")

stat_course_title <-
  html_nodes(stat_page,
             "div.col-md-12 > div.panel-group > div > div.panel-heading") %>%
  html_text() %>% str_squish()
stat_course_code <-
  str_extract_all(stat_course_title,
                  "Statistical\\sSciences\\s[0-9]{4}([A-Z](\\/)?)*") %>%
  unlist()
stat_course_name <-
  gsub(
    "(Statistical\\sSciences\\s[0-9]{4}(?:[A-Z](?:\\/)?)*)(.*)",
    "\\2",
    stat_course_title
  ) %>%
  str_squish() %>% str_to_title()

stat_num_courses <- length(stat_course_links)
stat_course_description <-
  vector(mode = "character", length = stat_num_courses)
stat_credit_amount <-
  vector(mode = "numeric", length = stat_num_courses)
stat_antireq <-
  vector(mode = "character", length = stat_num_courses)
stat_coreq <- vector(mode = "character", length = stat_num_courses)
stat_prereq <- vector(mode = "character", length = stat_num_courses)
stat_hours <- vector(mode = "character", length = stat_num_courses)
stat_notes <- vector(mode = "character", length = stat_num_courses)
stat_breadth <-
  vector(mode = "character", length = stat_num_courses)

i <- 1
while (i <= stat_num_courses) {
  course_page <- read_html(curl(
    paste0("https://www.westerncalendar.uwo.ca/", stat_course_links[i]),
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  ))
  course_info <-
    html_nodes(course_page, "#CourseInformationDiv .col-xs-12") %>%
    html_text() %>% str_squish()
  stat_course_description[i] <-
    grep("Course\\sDescription", course_info, value = TRUE) %>%
    str_remove("Course\\sDescription\\s") %>% str_squish()
  stat_credit_amount[i] <-
    grep("Course\\sWeight", course_info, value = TRUE) %>%
    str_extract("[0-9]\\.[0-9]+") %>% as.numeric()
  stat_antireq[i] <-
    grep("Antirequisite", course_info, value = TRUE) %>%
    str_remove_all("Antirequisite\\(s\\)(:)?") %>% str_squish() %>% paste0(collapse = "")
  pre_co_req <-
    grep("Pre\\sor\\sCorequisites", course_info, value = TRUE) %>%
    str_remove_all("Pre\\sor\\sCorequisites") %>%
    str_split("(\\.)(?![0-9])(?<![0-9])") %>%
    unlist()
  stat_coreq[i] <- grep("Corequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Corequisite\\(s\\)(:)?)|(Pre\\-(\\s)?or\\sCorequisite\\(s\\)(:)?)") %>%
    unlist() %>% str_squish() %>% paste0(collapse = "")
  stat_prereq[i] <-
    grep("Prerequisite", pre_co_req, value = TRUE) %>%
    str_remove_all("(Prerequisite\\(s\\)(:)?)") %>% str_squish() %>%
    unlist() %>% paste0(collapse = "")
  stat_other_info <-
    grep("Extra\\sInformation", course_info, value = TRUE) %>%
    str_split("\\.") %>% unlist() %>% str_remove_all("Extra\\sInformation(:)?") %>%
    str_squish()
  stat_hours[i] <- stat_other_info[1] %>%
    str_squish() %>% unlist() %>% paste0(collapse = "")
  stat_notes[i] <- grep("Note", stat_other_info, value = TRUE) %>%
    str_remove("(Note:)|(Note\\sat\\sMain\\scampus:)") %>% str_squish() %>%
    unlist() %>% paste0(collapse = "")
  stat_breadth[i] <-
    grep("Breadth:", course_info, value = TRUE) %>%
    str_extract("CATEGORY\\s[A-Z]") %>% str_to_title()
  i <- i + 1
}

stat_lecture <- grepl("lecture", stat_hours)
stat_lab <- grepl("laboratory|lab", stat_hours)
stat_tutorial <-
  grepl("(?<!\\/)(tutorial)(?!\\/)", stat_hours, perl = TRUE)

stat_credit_amount <- stat_credit_amount * 6

main_campus_stat <-
  html_nodes(stat_page, "div > div.panel-heading > a > div > img") %>%
  html_attr("alt") %>% grep("Western", .)

stat_course_calendar <- data.frame(
  stat_course_code,
  stat_course_name,
  stat_course_description,
  stat_credit_amount,
  stat_antireq,
  stat_coreq,
  stat_prereq,
  stat_hours,
  stat_lecture,
  stat_lab,
  stat_tutorial,
  stat_breadth,
  stat_notes
) %>% slice(main_campus_stat)
colnames(stat_course_calendar) <- c(
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
  "Breadth Requirement",
  "Note"
)

# Combined Course Calendar ####

combined_course_calendar <- bind_rows(
  cs_course_calendar,
  data_course_calendar,
  math_course_calendar,
  se_course_calendar,
  stat_course_calendar
)

# Write CSV Files ####

write.csv(
  cs_course_calendar,
  "University of Western Ontario Computer Science Course Calendar.csv"
)
write.csv(data_course_calendar,
          "University of Western Ontario Data Science Course Calendar.csv")
write.csv(math_course_calendar,
          "University of Western Ontario Mathematics Course Calendar.csv")
write.csv(
  se_course_calendar,
  "University of Western Ontario Software Engineering Course Calendar.csv"
)
write.csv(
  stat_course_calendar,
  "University of Western Ontario Statistical Sciences Course Calendar.csv"
)
