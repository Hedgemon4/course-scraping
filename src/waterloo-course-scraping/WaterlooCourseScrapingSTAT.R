# Course Scraping Waterloo Data Science Requirements

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# Source for functions
source("~/R/Projects/course-scraping/src/util/CourseScrapingUtil.R")

waterloo_course_requirements <-
  get_course_dataframe(
    "http://ugradcalendar.uwaterloo.ca/page/MATH-Statistics1",
    "#ctl00_contentMain_lblContent ul ul a"
  )

colnames(waterloo_course_requirements) <- c("Course Code")

# TODO: Try putting into a list of list

# Get Categories

web_link <-
  "http://ugradcalendar.uwaterloo.ca/page/MATH-Statistics1"

# Pull strings for categories (like requirement v on degree navigator)
category_nodes <- read_html(web_link) %>% html_elements(xpath = "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li") 
category_text <- vector(mode = "character", length = length(category_nodes))
test1 <- read_html(web_link) %>% html_elements(xpath = "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li[1]/text()") %>% html_text() %>% str_squish()
i <- 1
for(item in category_nodes){
  if(grepl("ul", item))
    category_text[i] <- read_html(web_link) %>% html_elements(xpath =  paste0("//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li[", i, "]/text()")) %>% html_text() %>% str_squish()
  else
    category_text[i] <- read_html(web_link) %>% html_elements(xpath = paste0("//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li[", i, "]")) %>% html_text() %>% str_squish()
  i = i + 1
}

category_names <-
  c(
    "Category 1",
    "Category 2",
    "Category 3",
    "Category 4",
    "Category 5",
    "Category 6",
    "Category 7",
    "Category 8"
  )

requirement_categories_courses <-
  vector(mode = "character",
         length = nrow(waterloo_course_requirements))

category_description_courses <-
  vector(mode = "character",
         length = nrow(waterloo_course_requirements))

category_is_used <- vector(mode = "logical", length = length(category_text))

courses <- vector(mode = "character")

course_codes <- "STAT(![0-9])|MATH(![0-9])|AMATH(![0-9])|ENGL(![0-9])|CS(![0-9])|MTHEL(![0-9])"

decsription_for_general_requirement <- vector(mode = "character")
decsription_for_general_requirement_element <- vector(mode = "numeric")

# Loop using html tags
n <- as.numeric(length(category_text))
i <- 1
j <- 1
k <- 0

# test1 <- read_html(web_link) %>% html_elements(xpath = "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li/ul/li") %>% html_text() %>% str_squish()
# Filter categories by html layout instead of string matching
while (i <= n) {
  courses_in_category <-
    read_html(web_link) %>% html_nodes(paste0(
      "#ctl00_contentMain_lblContent > ul > li:nth-child(",
      i,
      ") > ul > li > a"
    )) %>% html_text() %>% str_squish()
  for (course in courses_in_category) {
    k <- k + 1
    if(!grepl("[0-9]", course)){
      decsription_for_general_requirement[length(decsription_for_general_requirement) + 1] <- read_html(web_link) %>% html_node(paste0("#ctl00_contentMain_lblContent > ul > li:nth-child(" , i, ") > ul > li:nth-child(", k, ")")) %>% html_text() %>% str_squish()
      decsription_for_general_requirement_element <- j
    }
    courses[j] <- course
    requirement_categories_courses[j] <- category_names[i]
    category_description_courses[j] <- category_text[i]
    category_is_used[i] <- TRUE
    j = j + 1
  }
  k <- 0
  i = i + 1
}

course_categories <-
  data.frame(courses, requirement_categories_courses,
             category_description_courses)
colnames(course_categories) <- c("Course Code","Category", "Category Requirement")

# Function to scan for category, and append category header if item is not found

waterloo_course_requirements <-
  merge(waterloo_course_requirements, course_categories, by = "Course Code")

# Get courses
compsci_courses_waterloo <-
  get_course_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/CS",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(compsci_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

math_courses_waterloo <-
  get_course_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/MATH",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(math_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

stat_courses_waterloo <-
  get_course_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/STAT",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(stat_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

engl_courses_waterloo <-
  get_course_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/ENGL",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(engl_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

amath_courses_waterloo <-
  get_course_dataframe(
    "http://ugradcalendar.uwaterloo.ca/courses/AMATH",
    ".divTableCell:nth-child(1) strong",
    ".colspan-2 strong",
    ".colspan-2:nth-child(4)"
  )

colnames(amath_courses_waterloo) <-
  c('Course Code', 'Course Name', 'Course Description')

mthel_courses_waterloo <-
  get_course_dataframe(
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

# TODO: Add courses of any level categories to data frame
# TODO: Find a way to generalize categories of any level


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

# TODO: Put other requirements into main courses document using generalized function
# For example One additional 300- or 400-level STAT course -> STAT 3** One of One additional 300- level STAT Course

# TODO: Combine the code from the CS and STAT waterloo course scraping Files
# TODO: Generalize this combined code and functions to apply to other universities
