# Program Calendar Scraping - University of Manitoba ####

# This file scraps the course calendars for course types which can be used
# to fill elective requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Computer Science Course Calendar ####

# Gets the webpage for computer science courses
cs_page <-
  read_html(
    curl(
      "https://catalog.umanitoba.ca/undergraduate-studies/science/computer-science/#coursestext",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

# Scraps computer science course codes
cs_course_code <-
  html_nodes(
    cs_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-3.detail-code.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()

# Scraps computer science course names
cs_course_name <-
  html_nodes(
    cs_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-7.detail-title.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()

# Scraps computer science course credit amounts
cs_credit_amount <-
  html_nodes(
    cs_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.detail-hours_html.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text() %>% str_remove_all("[^0-9]") %>% unlist() %>% as.numeric()

# Number of computer science courses
cs_num_courses <- length(cs_course_code)

# Vectors to store information about computer science courses in
cs_course_description <-
  vector(mode = "character", length = cs_num_courses)
cs_prereq <- vector(mode = "character", length = cs_num_courses)
cs_coreq <- vector(mode = "character", length = cs_num_courses)
cs_antireq <- vector(mode = "character", length = cs_num_courses)
cs_equiv <- vector(mode = "character", length = cs_num_courses)
cs_note <- vector(mode = "character", length = cs_num_courses)
cs_lab <- vector(mode = "logical", length = cs_num_courses)

# This loop scraps the information about each computer science course, and filters
# the resulting vector using regex to place the information into the appropriate
# vectors from above.

i <- 1

while (i <= cs_num_courses) {
  # Scrap the course information for the next course
  cs_course_info <-
    html_nodes(cs_page,
               paste0("#coursestextcontainer > div > div:nth-child(", i, ") > div > p")) %>%
    html_text()
  # Seperate information into the appropriate vectors
  cs_lab[i] <- grepl("Lab\\sRequired", cs_course_info[1])
  cs_course_description[i] <-
    cs_course_info[1] %>% str_remove("\\(Lab\\sRequired\\)") %>% str_squish()
  prcr <-
    grep("PR\\/CR:", cs_course_info, value = TRUE) %>% str_split("\\.") %>% unlist()
  cs_prereq[i] <-
    grep("Prerequisite(s|):", prcr, value = TRUE) %>% paste("") %>%
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
  cs_equiv[i] <-
    grep("Equiv\\sTo:", cs_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Equiv\\sTo:") %>% str_squish()
  cs_note[i] <-
    grep("Attributes:", cs_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Attributes:") %>% str_squish()
  i <- i + 1
}

# Create data frame for computer science courses
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

# The code for the following course calendars has been copied from above, so the
# comments have been omitted, as it works the same with minor changes to the
# regula expressions depending on how the information was layed out.

# Mathematics Course Calendar ####

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
math_antireq <-
  vector(mode = "character", length = math_num_courses)
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
  math_prereq[i] <-
    grep("Prerequisite(s|):", prcr, value = TRUE) %>% paste("") %>%
    str_remove("Prerequisite(s|):") %>% str_squish()
  math_coreq[i] <-
    grep(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite",
      prcr,
      value = TRUE
    ) %>% paste("") %>%
    str_remove(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite:"
    ) %>%
    str_squish()
  math_antireq[i] <-
    grep("Mutually\\sExclusive:", math_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Mutually\\sExclusive:") %>% str_squish()
  math_equiv[i] <-
    grep("Equiv\\sTo:", math_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Equiv\\sTo:") %>% str_squish()
  math_note[i] <-
    grep("Attributes:", math_course_info, value = TRUE) %>% paste("") %>%
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

# Data Science Course Calendar ####

data_page <-
  read_html(
    curl(
      "https://catalog.umanitoba.ca/undergraduate-studies/science/data-science/#coursestext",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

data_course_code <-
  html_nodes(
    data_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-3.detail-code.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()

data_course_name <-
  html_nodes(
    data_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-7.detail-title.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()

data_credit_amount <-
  html_nodes(
    data_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.detail-hours_html.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text() %>% str_remove_all("[^0-9]") %>% unlist() %>% as.numeric()

data_num_courses <- length(data_course_code)

data_course_description <-
  vector(mode = "character", length = data_num_courses)
data_prereq <- vector(mode = "character", length = data_num_courses)
data_coreq <- vector(mode = "character", length = data_num_courses)
data_antireq <-
  vector(mode = "character", length = data_num_courses)
data_equiv <- vector(mode = "character", length = data_num_courses)
data_note <- vector(mode = "character", length = data_num_courses)
data_lab <- vector(mode = "logical", length = data_num_courses)

i <- 1
while (i <= data_num_courses) {
  data_course_info <-
    html_nodes(data_page,
               paste0("#coursestextcontainer > div > div:nth-child(", i, ") > div > p")) %>%
    html_text()
  data_lab[i] <- grepl("Lab\\s(R|r)equired", data_course_info[1])
  data_course_description[i] <-
    data_course_info[1] %>% str_remove("\\(Lab\\s(R|r)equired\\)") %>% str_squish()
  prcr <-
    grep("PR\\/CR:", data_course_info, value = TRUE) %>% str_split("\\.") %>% unlist()
  data_prereq[i] <-
    grep("Prerequisite(s|):", prcr, value = TRUE) %>% paste("") %>%
    str_remove("Prerequisite(s|):") %>% str_squish()
  data_coreq[i] <-
    grep(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite(s|)",
      prcr,
      value = TRUE
    ) %>% paste("") %>%
    str_remove(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite(s|):"
    ) %>%
    str_squish()
  data_antireq[i] <-
    grep("Mutually\\sExclusive:", data_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Mutually\\sExclusive:") %>% str_squish()
  data_equiv[i] <-
    grep("Equiv\\sTo:", data_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Equiv\\sTo:") %>% str_squish()
  data_note[i] <-
    grep("Attributes:", data_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Attributes:") %>% str_squish()
  i <- i + 1
}

data_course_calendar <-
  data.frame(
    data_course_code,
    data_course_name,
    data_course_description,
    data_credit_amount,
    data_antireq,
    data_coreq,
    data_prereq,
    data_equiv,
    data_lab,
    data_note
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
    "Equivalency",
    "Lab",
    "Note"
  )

# Statistics Course Calendar ####

stat_page <-
  read_html(
    curl(
      "https://catalog.umanitoba.ca/undergraduate-studies/science/statistics/#coursestext",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

stat_course_code <-
  html_nodes(
    stat_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-3.detail-code.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()

stat_course_name <-
  html_nodes(
    stat_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.col-7.detail-title.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()

stat_credit_amount <-
  html_nodes(
    stat_page,
    "#coursestextcontainer > div > div > div.cols.noindent > span.text.detail-hours_html.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text() %>% str_remove_all("[^0-9]") %>% unlist() %>% as.numeric()

stat_num_courses <- length(stat_course_code)

stat_course_description <-
  vector(mode = "character", length = stat_num_courses)
stat_prereq <- vector(mode = "character", length = stat_num_courses)
stat_coreq <- vector(mode = "character", length = stat_num_courses)
stat_antireq <-
  vector(mode = "character", length = stat_num_courses)
stat_equiv <- vector(mode = "character", length = stat_num_courses)
stat_note <- vector(mode = "character", length = stat_num_courses)
stat_lab <- vector(mode = "logical", length = stat_num_courses)

i <- 1
while (i <= stat_num_courses) {
  stat_course_info <-
    html_nodes(stat_page,
               paste0("#coursestextcontainer > div > div:nth-child(", i, ") > div > p")) %>%
    html_text()
  stat_lab[i] <- grepl("Lab\\s(R|r)equired", stat_course_info[1])
  stat_course_description[i] <-
    stat_course_info[1] %>% str_remove("\\(Lab\\s(R|r)equired\\)") %>% str_squish()
  prcr <-
    grep("PR\\/CR:", stat_course_info, value = TRUE) %>% str_split("\\.") %>% unlist()
  stat_prereq[i] <-
    grep("Prerequisite(s|):", prcr, value = TRUE) %>% paste("") %>%
    str_remove("Prerequisite(s|):") %>% str_squish()
  stat_coreq[i] <-
    grep(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite(s|)",
      prcr,
      value = TRUE
    ) %>% paste("") %>%
    str_remove(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite(s|):"
    ) %>%
    str_squish()
  stat_antireq[i] <-
    grep("Mutually\\sExclusive:", stat_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Mutually\\sExclusive:") %>% str_squish()
  stat_equiv[i] <-
    grep("Equiv\\sTo:", stat_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Equiv\\sTo:") %>% str_squish()
  stat_note[i] <-
    grep("Attributes:", stat_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Attributes:") %>% str_squish()
  i <- i + 1
}

stat_course_calendar <-
  data.frame(
    stat_course_code,
    stat_course_name,
    stat_course_description,
    stat_credit_amount,
    stat_antireq,
    stat_coreq,
    stat_prereq,
    stat_equiv,
    stat_lab,
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
    "Equivalency",
    "Lab",
    "Note"
  )

# Interdisciplinary Science Course Calendar ####

sci_page <-
  read_html(
    curl(
      "https://catalog.umanitoba.ca/undergraduate-studies/science/interdisciplinary-science-courses/#coursestext",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

sci_course_code <-
  html_nodes(
    sci_page,
    "#coursestextcontainer > div:nth-child(6) > div > div.cols.noindent > span.text.col-3.detail-code.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()

sci_course_name <-
  html_nodes(
    sci_page,
    "#coursestextcontainer > div:nth-child(6) > div > div.cols.noindent > span.text.col-7.detail-title.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text()

sci_credit_amount <-
  html_nodes(
    sci_page,
    "#coursestextcontainer > div:nth-child(6) > div > div.cols.noindent > span.text.detail-hours_html.margin--tiny.text--semibold.text--big"
  ) %>%
  html_text() %>% str_remove_all("[^0-9]") %>% unlist() %>% as.numeric()

sci_num_courses <- length(sci_course_code)

sci_course_description <-
  vector(mode = "character", length = sci_num_courses)
sci_prereq <- vector(mode = "character", length = sci_num_courses)
sci_coreq <- vector(mode = "character", length = sci_num_courses)
sci_antireq <- vector(mode = "character", length = sci_num_courses)
sci_equiv <- vector(mode = "character", length = sci_num_courses)
sci_note <- vector(mode = "character", length = sci_num_courses)
sci_lab <- vector(mode = "logical", length = sci_num_courses)

i <- 1
while (i <= sci_num_courses) {
  sci_course_info <-
    html_nodes(
      sci_page,
      paste0(
        "#coursestextcontainer > div:nth-child(6) > div:nth-child(",
        i,
        ") > div > p"
      )
    ) %>%
    html_text()
  sci_lab[i] <- grepl("Lab\\s(R|r)equired", sci_course_info[1])
  sci_course_description[i] <-
    sci_course_info[1] %>% str_remove("\\(Lab\\s(R|r)equired\\)") %>% str_squish()
  prcr <-
    grep("PR\\/CR:", sci_course_info, value = TRUE) %>% str_split("\\.") %>% unlist()
  sci_prereq[i] <-
    grep("Prerequisite(s|):", prcr, value = TRUE) %>% paste("") %>%
    str_remove("Prerequisite(s|):") %>% str_squish()
  sci_coreq[i] <-
    grep(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite",
      prcr,
      value = TRUE
    ) %>% paste("") %>%
    str_remove(
      "Co-requisite|corequisite|co requisite|Prerequisite or concurrent registration|Pre- or corequisite:"
    ) %>%
    str_squish()
  sci_antireq[i] <-
    grep("Mutually\\sExclusive:", sci_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Mutually\\sExclusive:") %>% str_squish()
  sci_equiv[i] <-
    grep("Equiv\\sTo:", sci_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Equiv\\sTo:") %>% str_squish()
  sci_note[i] <-
    grep("Attributes:", sci_course_info, value = TRUE) %>% paste("") %>%
    str_remove("Attributes:") %>% str_squish()
  i <- i + 1
}

sci_course_calendar <-
  data.frame(
    sci_course_code,
    sci_course_name,
    sci_course_description,
    sci_credit_amount,
    sci_antireq,
    sci_coreq,
    sci_prereq,
    sci_equiv,
    sci_lab,
    sci_note
  )

colnames(sci_course_calendar) <-
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

# Write CSV Files ####

# write.csv(cs_course_calendar,
#           "University of Manitoba Computer Science Course Calendar.csv")
# write.csv(
#   math_course_calendar,
#   "University of Manitoba Mathematics Science Course Calendar.csv"
# )
# write.csv(data_course_calendar,
#           "University of Manitoba Data Science Course Calendar.csv")
# write.csv(stat_course_calendar,
#           "University of Manitoba Statistics Course Calendar.csv")
# write.csv(
#   sci_course_calendar,
#   "University of Manitoba Interdisciplinary Science Course Calendar.csv"
# )
