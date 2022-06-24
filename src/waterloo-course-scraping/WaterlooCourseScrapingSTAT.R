# Course Scraping Waterloo Data Science Requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Source for Utility functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

# TODO List ####
# TODO: Clean code
# TODO: Get ride of non generic functions
# > match_item <- c("MATH 212 LEC,TST, TUT", "MATH 213, TST, TUT")
# > grepl("LEC", match_item)
# [1]  TRUE FALSE
# > grepl("TST", match_item)
# [1] TRUE TRUE
# TODO: Spell checking library(stringdist)

# Functions ####
seperate_information <- function(match_item, column_name, web_link, node, course_dataframe, vector_type, break_if_match){
  information <- read_html(web_link) %>% html_nodes(node) %>% html_text() %>% str_squish() %>% stri_remove_empty()
  columns <- list()
  
  i = 1
  for(item in match_item){
    columns[[i]] <- vector(mode = vector_type, length = nrow(course_dataframe))
    i = i + 1
  }
  
  i <- 1
  for(item in match_item){
    if(vector_type == "logical"){
      columns[[i]] <- grepl(item, information)
    }else{
      columns[[i]] <- grep(item, information, value = TRUE)
    } 
    i = i + 1
  }
  
  seperate <- data.frame(columns)
  colnames(seperate) <- column_name
  course_dataframe <- cbind.data.frame(course_dataframe, seperate)
  return(course_dataframe)
}

seperate_information_increment <- function(match_item, column_name, web_link, node, course_dataframe, vector_type, break_if_match, increment_match, match_offset){
  information <- read_html(web_link) %>% html_nodes(node) %>% html_text() %>% str_squish() %>% stri_remove_empty()
  columns <- list()
  n <- nrow(course_dataframe)
  
  i = 1
  for(item in match_item){
    columns[[i]] <- vector(mode = vector_type, length = n)
    i = i + 1
  }
  
  j = 1
  for(item in information){
    i = 0
    if (j < n & item == course_dataframe[j + match_offset, increment_match]) {
      j = j + 1
      next
    }
    for(column in match_item){
      i = i + 1
      if(grepl(column, item)){
        if(vector_type == "logical")
          columns[[i]][j] <- TRUE
        else
          columns[[i]][j] <- item
        if(break_if_match){
          break
        }
      }
    }
  }
  
  seperate <- data.frame(columns)
  colnames(seperate) <- column_name
  course_dataframe <- cbind.data.frame(course_dataframe, seperate)
  return(course_dataframe)
}

seperate_information_single <- function(match_item, column_name, web_link, node, course_dataframe, vector_type, break_if_match){
  information <- read_html(web_link) %>% html_nodes(node) %>% html_text() %>% str_squish() %>% stri_remove_empty()
  columns <- vector(mode = vector_type, length = nrow(course_dataframe))
  j = 0
  for(item in information){
    j = j + 1
    for(column in match_item){
      if(grepl(column, item)){
        if(vector_type == "logical")
          columns[j] <- TRUE
        else
          columns[j] <- column
        if(break_if_match)
          break
      }
    }
  }
  seperate <- data.frame(columns)
  colnames(seperate) <- column_name
  course_dataframe <- cbind.data.frame(course_dataframe, seperate)
  return(course_dataframe)
}

# Get Course Requirements ####
web_link <-
  "http://ugradcalendar.uwaterloo.ca/page/MATH-Statistics1"

waterloo_course_requirements <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/page/MATH-Statistics1",
    "#ctl00_contentMain_lblContent ul ul a"
  )

number_of_courses <- nrow(waterloo_course_requirements)

colnames(waterloo_course_requirements) <- c("Course Code")

# Get Categories ####

# Pull strings for categories (like requirement v on degree navigator)

# Html Nodes for the categories
category_html_nodes <-
  read_html(web_link) %>% html_elements(xpath = "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li")

number_of_categories <- length(category_html_nodes)

# Descriptions of Categories
category_description <-
  vector(mode = "character", length = number_of_categories)

# Gets descriptions of categories base on if they have sublists or not
i <- 1
for (item in category_html_nodes) {
  if (grepl("ul", item))
    category_description[i] <-
      read_html(web_link) %>% html_elements(xpath =  paste0(
        "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li[",
        i,
        "]/text()"
      )) %>% html_text() %>% str_squish()
  else
    category_description[i] <-
      read_html(web_link) %>% html_elements(xpath = paste0("//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li[", i, "]")) %>% html_text() %>% str_squish()
  i = i + 1
}

category_names <- get_item_vector("Category", number_of_categories)

course_category <- vector(mode = "character", length = number_of_courses)

course_category_description <- vector(mode = "character", length = number_of_courses)

category_is_used <- vector(mode = "logical", length = number_of_categories)

course_code <- vector(mode = "character", length = number_of_courses)

general_requirement_description <- vector(mode = "character")
general_requirement_index <- vector(mode = "numeric")

# Loop using html tags
i <- 1
j <- 1
k <- 0

while (i <= number_of_categories) {
  courses_in_category <-
    read_html(web_link) %>% html_nodes(paste0(
      "#ctl00_contentMain_lblContent > ul > li:nth-child(",
      i,
      ") > ul > li > a"
    )) %>% html_text() %>% str_squish()
  for (course in courses_in_category) {
    k <- k + 1
    if (!grepl("[0-9]", course)) {
      general_requirement_description[length(general_requirement_description) + 1] <-
        read_html(web_link) %>% html_node(
          paste0(
            "#ctl00_contentMain_lblContent > ul > li:nth-child(" ,
            i,
            ") > ul > li:nth-child(",
            k,
            ")"
          )
        ) %>% html_text() %>% str_squish()
      general_requirement_index <- j - 1
    }
    course_code[j] <- course
    course_category[j] <- category_names[i]
    course_category_description[j] <- category_description[i]
    category_is_used[i] <- TRUE
    j = j + 1
  }
  k <- 0
  i = i + 1
}

course_categories <-
  data.frame(course_code,
             course_category,
             course_category_description)
colnames(course_categories) <-
  c("Course Code", "Category", "Category Requirement")

waterloo_course_requirements <-
  merge(waterloo_course_requirements, course_categories, by = "Course Code")

# Get Course Information #### 

# TODO: Change to use get_text_xpath(cs_link, "/html/body/main/center[8]/div/div/em")
compsci_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(compsci_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

math_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(math_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

stat_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(stat_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

engl_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/ENGL",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(engl_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

amath_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/AMATH",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(amath_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

mthel_courses_waterloo <-
  get_text_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/MTHEL",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(mthel_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

# Clean Data
clean_from_data <-
  "LEC|LAB|\\,|TST|TUT|WSP|ESS|DIS|0\\.50|PRJ|RDG|STU|0\\.25|0\\.00|2\\.50|SEM"
compsci_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", compsci_courses_waterloo$`Course Code`) %>% str_squish()
math_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", math_courses_waterloo$`Course Code`) %>% str_squish()
stat_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", stat_courses_waterloo$`Course Code`) %>% str_squish()
engl_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", engl_courses_waterloo$`Course Code`) %>% str_squish()
amath_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", amath_courses_waterloo$`Course Code`) %>% str_squish()
mthel_courses_waterloo["Course Code"] <-
  gsub(clean_from_data, "", mthel_courses_waterloo$`Course Code`) %>% str_squish()

matches <- c("Prereq:", "Antireq:", "Coreq:", "Note", ".")
columns <-
  c("Prerequisite",
    "Antirequisite",
    "Corequisite",
    "Note",
    "Other Information")

amath_courses_waterloo <-
  seperate_information_increment(
    matches,
    columns,
    "http://ugradcalendar.uwaterloo.ca/courses/AMATH",
    ".colspan-2 :nth-child(1)",
    amath_courses_waterloo,
    "character",
    TRUE,
    2,
    1
  )

engl_courses_waterloo <-
  seperate_information_increment(
    matches,
    columns,
    "http://ugradcalendar.uwaterloo.ca/courses/ENGL",
    ".colspan-2 :nth-child(1)",
    engl_courses_waterloo,
    "character",
    TRUE,
    2,
    1
  )

math_courses_waterloo <-
  seperate_information_increment(
    matches,
    columns,
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".colspan-2 :nth-child(1)",
    math_courses_waterloo,
    "character",
    TRUE,
    2,
    1
  )

compsci_courses_waterloo <-
  seperate_information_increment(
    matches,
    columns,
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".colspan-2 :nth-child(1)",
    compsci_courses_waterloo,
    "character",
    TRUE,
    2,
    1
  )

stat_courses_waterloo <-
  seperate_information_increment(
    matches,
    columns,
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".colspan-2 :nth-child(1)",
    stat_courses_waterloo,
    "character",
    TRUE,
    2,
    1
  )

mthel_courses_waterloo <-
  seperate_information_increment(
    matches,
    columns,
    "http://ugradcalendar.uwaterloo.ca/courses/MTHEL",
    ".colspan-2 :nth-child(1)",
    mthel_courses_waterloo,
    "character",
    TRUE,
    2,
    1
  )

names <- c("LEC", "LAB", "TST", "TUT", "PRJ", "RDG", "STU")

course_component_name <-
  c("Lecture",
    "Lab",
    "Test Slot",
    "Tutorial",
    "Project",
    "Reading",
    "Studio")

amath_courses_waterloo <-
  seperate_information(
    names,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/AMATH",
    ".divTableCell:nth-child(1) strong",
    amath_courses_waterloo,
    "logical",
    FALSE
  )

compsci_courses_waterloo <-
  seperate_information(
    names,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".divTableCell:nth-child(1) strong",
    compsci_courses_waterloo,
    "logical",
    FALSE
  )

stat_courses_waterloo <-
  seperate_information(
    names,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".divTableCell:nth-child(1) strong",
    stat_courses_waterloo,
    "logical",
    FALSE
  )

math_courses_waterloo <-
  seperate_information(
    names,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".divTableCell:nth-child(1) strong",
    math_courses_waterloo,
    "logical",
    FALSE
  )

engl_courses_waterloo <-
  seperate_information(
    names,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/ENGL",
    ".divTableCell:nth-child(1) strong",
    engl_courses_waterloo,
    "logical",
    FALSE
  )

mthel_courses_waterloo <-
  seperate_information(
    names,
    course_component_name,
    "http://ugradcalendar.uwaterloo.ca/courses/MTHEL",
    ".divTableCell:nth-child(1) strong",
    mthel_courses_waterloo,
    "logical",
    FALSE
  )

amath <-
  merge(waterloo_course_requirements, amath_courses_waterloo, by = "Course Code")
math <-
  merge(waterloo_course_requirements, math_courses_waterloo, by = "Course Code")
cs <-
  merge(waterloo_course_requirements, compsci_courses_waterloo, by = "Course Code")
stat <-
  merge(waterloo_course_requirements, stat_courses_waterloo, by = "Course Code")
engl <-
  merge(waterloo_course_requirements, engl_courses_waterloo, by = "Course Code")
mthel <-
  merge(waterloo_course_requirements, mthel_courses_waterloo, by = "Course Code")

requirements <- rbind(amath, math, cs, stat, engl, mthel)
requirements <-
  requirements[, c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Category",
    "Category Requirement",
    "Prerequisite",
    "Antirequisite",
    "Corequisite",
    "Note",
    "Other Information",
    "Lecture",
    "Lab",
    "Test Slot",
    "Tutorial",
    "Project",
    "Reading",
    "Studio"
  )]

# TODO: Add courses of any level categories to data frame
i <- 1
other_frame <- NULL
for (item in general_requirement_index) {
  course_code <- waterloo_course_requirements$`Course Code`[item]
  course_name <- "Other"
  course_description <- general_requirement_description[i]
  category <- waterloo_course_requirements$Category[item]
  category_requirement <-
    waterloo_course_requirements$`Category Requirement`[item]
  prerequisite <- NA
  antirequisite <- NA
  corequisite <- NA
  note <- NA
  other_information <- NA
  lecture <- NA
  lab <- NA
  test_slot <- NA
  tutorial <- NA
  project <- NA
  reading <- NA
  studio <- NA
  other_frame <-
    data.frame(
      course_code,
      course_name,
      course_description,
      category,
      category_requirement,
      prerequisite,
      antirequisite,
      corequisite,
      note,
      other_information,
      lecture
      ,
      lab,
      test_slot,
      tutorial,
      project,
      reading,
      studio
    )
  colnames(other_frame) <-  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Category",
    "Category Requirement",
    "Prerequisite",
    "Antirequisite",
    "Corequisite",
    "Note",
    "Other Information",
    "Lecture",
    "Lab",
    "Test Slot",
    "Tutorial",
    "Project",
    "Reading",
    "Studio"
  )
  requirements <- rbind(requirements, other_frame)
  i <- i + 1
}
# TODO: Find a way to generalize categories of any level
i <- 1
for (item in category_is_used) {
  # TODO: rep NA to get a repeating empty vector of length rep(NA, ncol(df))
  # https://www.geeksforgeeks.org/how-to-insert-blank-row-into-dataframe-in-r/
  if (!item) {
    course_code <- "Category Requirement"
    course_name <- "Other Requirement"
    course_description <- category_description[i]
    category <- category_names[i]
    category_requirement <- category_description[i]
    prerequisite <- NA
    antirequisite <- NA
    corequisite <- NA
    note <- NA
    other_information <- NA
    lecture <- NA
    lab <- NA
    test_slot <- NA
    tutorial <- NA
    project <- NA
    reading <- NA
    studio <- NA
    other_frame <-
      data.frame(
        course_code,
        course_name,
        course_description,
        category,
        category_requirement,
        prerequisite,
        antirequisite,
        corequisite,
        note,
        other_information,
        lecture
        ,
        lab,
        test_slot,
        tutorial,
        project,
        reading,
        studio
      )
    colnames(other_frame) <-  c(
      "Course Code",
      "Course Name",
      "Course Description",
      "Category",
      "Category Requirement",
      "Prerequisite",
      "Antirequisite",
      "Corequisite",
      "Note",
      "Other Information",
      "Lecture",
      "Lab",
      "Test Slot",
      "Tutorial",
      "Project",
      "Reading",
      "Studio"
    )
    requirements <- rbind(requirements, other_frame)
  }
  i <- i + 1
}
# TODO: Put other requirements into main courses document using generalized function
# For example One additional 300- or 400-level STAT course -> STAT 3** One of One additional 300- level STAT Course

# TODO: Combine the code from the CS and STAT Waterloo course scraping Files
# TODO: Generalize this combined code and functions to apply to other universities
