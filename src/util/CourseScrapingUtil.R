# Library Imports
library(rvest)
library(dplyr)
library(stringr)

get_course_dataframe <- function(course_description_link, ...){
  nodes <- list(...)
  course_information <- list()
  i = 1
  for (item in nodes){
    information <- read_html(course_description_link) %>% html_nodes(item) %>%
      html_text() %>% str_squish() 
    course_information <- cbind(course_information, information)
    i = i + 1
  }
  return(data.frame(course_information))
}
