# Library Imports
library(rvest)
library(dplyr)
library(stringr)

get_course_dataframe <- function(course_description_link, ...){
  nodes <- list(...)
  course_information <- NULL
  for (item in nodes){
    information <- read_html(course_description_link) %>% html_nodes(item) %>%
      html_text() %>% str_squish() 
    course_information <- cbind(course_information, information)
  }
  return(data.frame(course_information))
}

get_other_course_information <- function(web_link, node, course_dataframe) {
  course_information <- read_html(web_link) %>% html_nodes(node) %>% html_text() %>% str_squish() %>% stri_remove_empty()
  
  n = nrow(course_dataframe)
  
  antireq <- vector(mode = 'character', length = n)
  prereq <- vector(mode = 'character', length = n)
  other <- vector(mode = 'character', length = n)
  coreq <- vector(mode = 'character', length = n)
  note <- vector(mode = 'character', length = n)
  
  i = 1
  
  for (item in course_information) {
    if(i >= n)
      break
    if (item == course_dataframe[i + 1, 2]) {
      i = i + 1
      next
    } else{
      if (grepl("Note:", item)) {
        note[i] <- item
      } else if (grepl("Prereq:", item)) {
        prereq[i] <- item
      } else if (grepl("Antireq:", item)) {
        antireq[i] <- item
      } else if (grepl("Coreq:", item)) {
        coreq[i] <- item
      } else
        other[i] <- item
    }
  }
  
  other_course_info <- data.frame(prereq, antireq, coreq, note, other)
  colnames(other_course_info) <-c("Prerequisite","Antirequisite","Corequisite","Note","Other Information")
  return(other_course_info)
}