# Library Imports
library(rvest)
library(dplyr)
library(stringr)

get_courses <- function(course_description_link, ...){
  nodes <- list(...)
  course_information <- list()
  i = 1
  for (item in nodes){
    course_information[i] <- read_html(course_description_link) %>% html_nodes(item) %>%
      html_text() %>% str_squish() 
    i = i + 1
  }
  return(course_information)
}
