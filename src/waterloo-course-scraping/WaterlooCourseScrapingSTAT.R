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
categories_web <-
  read_html(web_link) %>% html_elements(xpath = "//*[@id=\"ctl00_contentMain_lblContent\"]/ul/li/text()") %>% html_text() %>% str_squish()

categories_web[4] <-
  paste(categories_web[4], "STAT", categories_web[5], sep = " ")
categories_web <- categories_web[-5]

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

# Loop using html tags
n <- as.numeric(length(categories_web))
i <- 1
j <- 1

# Filter categories by html layout instead of string matching
while (i <= n) {
  courses_in_category <-
    read_html(web_link) %>% html_nodes(paste0(
      "#ctl00_contentMain_lblContent > ul > li:nth-child(",
      i,
      ") > ul > li > a"
    )) %>% html_text() %>% str_squish()
  for (course in courses_in_category) {
    requirement_categories_courses[j] <- category_names[i]
    category_description_courses[j] <- categories_web[i]
    j = j + 1
  }
  i = i + 1
}

course_categories <-
  data.frame(requirement_categories_courses,
             category_description_courses)
colnames(course_categories) <- c("Category", "Category Requirement")

# Function to scane for category, and append category header if item is not found

waterloo_course_requirements <-
  cbind(waterloo_course_requirements, course_categories)

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

other <- data.frame(c("STAT", "MATH", "CS", "AMATH", "ENGL"))
colnames(other) <- "Course Code"
other <- merge(waterloo_course_requirements, other, by = "Course Code")
other[1] <- "STAT 4**"


courses <- rbind(amath, math, cs, stat, engl, mthel)
courses <-
  courses[, c(
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
# TODO: Generlize this combined code and functions to apply to other universities
