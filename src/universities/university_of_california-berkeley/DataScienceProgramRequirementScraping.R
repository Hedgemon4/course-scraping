# University of California Berkeley Data Science Program Requirement Scraping ####

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Required Courses ####

program_page <- read_html(
  curl(
    "http://guide.berkeley.edu/undergraduate/degree-programs/data-science/#majorrequirementstext",
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  )
)

course_links <-
  html_nodes(program_page,
             "div#majorrequirementstextcontainer > table > tbody > tr > td.codecol > a") %>%
  html_attr("href")
link_text <-
  html_nodes(program_page,
             "div#majorrequirementstextcontainer > table > tbody > tr > td.codecol > a") %>%
  html_text()
course_code <- vector(mode = "character")
num_links <- length(link_text)

# Due to some of the link names having equivalent courses in them, they meed to be
# parsed to reduce them down to the first name

i <- 1
while (i <= num_links) {
  item <- link_text[i]
  if (grepl("\\/", item)) {
    name <- str_extract(item, "(.+?)(?=\\/)")
    if (!grepl("([A-Z]?[0-9]{1,3}[A-Z]{0,1})", name)) {
      name <-
        paste0(name,
               str_extract(item, "(\\s[A-Z]?[0-9]{1,3}[A-Z]{0,1})"))
    }
    course_code[i] <- name
  } else{
    course_code[i] <- item
  }
  i <- i + 1
}

course_name <- vector(mode = "character")
credit_amount <- vector(mode = "numeric")
course_description <- vector(mode = "character")
course_title <- vector(mode = "character")
prereq <- vector(mode = "character")
hours <- vector(mode = "character")
lecture <- vector(mode = "logical")
lab <- vector(mode = "logical")
test <- vector(mode = "character")

i <- 1
for (link in course_links) {
  course_page <- read_html(curl(
    paste0("http://guide.berkeley.edu", link),
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  ))
  courses <-
    html_nodes(course_page,
               "#fssearchresults > div.searchresult.search-courseresult > h2") %>%
    html_text() %>% str_squish()
  pattern <-
    paste0("(", "^", str_replace_all(course_code[i], "\\s", " "), ")")
  index <- grep(pattern, courses, perl = TRUE)
  course_title <-
    html_nodes(course_page,
               "#fssearchresults > div.searchresult.search-courseresult > h2") %>%
    .[[index]] %>% html_text() %>% str_squish()
  course_name[i] <-
    gsub(
      "(.*[A-Z]?[0-9]{1,3}[A-Z]{0,2})(\\s.*)((?:[0-9]\\s\\-\\s)?[0-9]\\sUnits)",
      "\\2",
      course_title
    ) %>%
    str_squish()
  credit_amount[i] <-
    gsub("(.*[A-Z]?[0-9]{1,3}[A-Z]{0,2})(\\s.*)([0-9])(\\sUnits)",
         "\\3",
         course_title) %>%
    as.numeric()
  course_description[i] <-
    html_nodes(
      course_page,
      "#fssearchresults > div.searchresult.search-courseresult > div > div > p.courseblockdesc"
    ) %>%
    .[[index]] %>% html_text() %>% str_squish() %>% str_remove(paste0(course_name[i], ":\\sRead\\sMore.*"))
  other_info <-
    html_nodes(course_page,
               "#fssearchresults > div.searchresult.search-courseresult") %>%
    .[[index]] %>% html_nodes("div > div > div > div > p") %>% html_text() %>% str_squish()
  prereq[i] <- grep("Prerequisites:", other_info, value = TRUE) %>%
    str_remove("Prerequisites:\\s") %>% paste0("")
  hours[i] <-
    grep("Fall\\sand\\/or\\sspring:", other_info, value = TRUE) %>%
    str_remove("Fall\\sand\\/or\\sspring:") %>% paste0("")
  lecture[i] <- grepl("lecture", hours[i])
  lab[i] <- grepl("laboratory ", hours[i])
  test <- paste(other_info, collapse = " ")
  i <- i + 1
}

required_courses <-
  data.frame(
    course_code,
    course_name,
    course_description,
    credit_amount,
    prereq,
    hours,
    lecture,
    lab
  )


test_page <- read_html(curl(
  paste0("http://guide.berkeley.edu", course_links[1]),
  handle = curl::new_handle("useragent" = "Mozilla/5.0")
))

test_info1 <-
  html_nodes(test_page,
             "#fssearchresults > div.searchresult.search-courseresult") %>%
  .[[index]] %>% html_nodes("div > div > div > div > p") %>% html_text() %>% str_squish()

# Program Requirements ####

program_page <- read_html(
  curl(
    "http://guide.berkeley.edu/undergraduate/degree-programs/data-science/#majorrequirementstext",
    handle = curl::new_handle("useragent" = "Mozilla/5.0")
  )
)

requirement_tables <-
  html_nodes(program_page, "div#majorrequirementstextcontainer > table")

table_names <-
  html_nodes(program_page, "div#majorrequirementstextcontainer > h5") %>%
  html_text()

all_tables <-
  html_nodes(program_page, "div#majorrequirementstextcontainer") %>% .[[1]] %>% html_table()

courses <- select(all_tables, Code) %>% unlist() %>% unname()
course_credit_amount <-
  select(all_tables, Units) %>% unlist() %>% unname()
num_courses <- length(courses)
categories <- "Category L1"
category_description <- "All of"
min_credits <- vector(mode = "character")
max_credits <- vector(mode = "character")
letter <- "L"
j <- 2
k <- 1
alph_count <- 1
table_num <- 1
alphabet <-
  c(
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z"
  )

i <- 2
while (i <= num_courses) {
  item <- courses[i]
  if (grepl("Code", item)) {
    letter <- "U"
    k <- k + 1
    next_item <- courses[i + 1]
    categories[j] <- paste0("Category ", letter, k)
    if (grepl("(one)|(two)|(three)|(four)|(all)", next_item)) {
      category_description[j] <- next_item
      i <- i + 1
    } else{
      category_description[j] <- "Choose one of the following: "
    }
    alph_count <- 1
    j <- j + 1
  } else if (grepl("or", item)) {
    new_description <-
      paste(category_description[j - 1], item, sep = " ")
    category_description[j - 1] <- new_description
  } else {
    category_description[j] <- item
    categories[j] <-
      paste0("Category ", letter, k, alphabet[alph_count])
    alph_count <- alph_count + 1
    j <- j + 1
  }
  i <- i + 1
}

program_requirements <- data.frame(categories, category_description)
