# Library Imports
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

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

seperate_information <- function(match_item, column_name, web_link, node, course_dataframe, vector_type, break_if_match){
  information <- read_html(web_link) %>% html_nodes(node) %>% html_text() %>% str_squish() %>% stri_remove_empty()
  
  columns <- list()
  i = 1
  for(item in match_item){
    columns[[i]] <- vector(mode = vector_type, length = nrow(course_dataframe))
    i = i + 1
  }
  
  j = 0
  for(item in information){
    i = 0
    j = j + 1
    for(column in match_item){
      i = i + 1
      if(grepl(column, item)){
        if(vector_type == "logical")
          columns[[i]][j] <- TRUE
        else
          columns[[i]][j] <- item
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

# TODO: Change this to use html tags instead of regex matching
get_requirement_categories <- function(web_link, node, category_type){
  info <- read_html(web_link) %>% html_nodes(node) %>% html_text() %>% str_squish()
  category <- NULL
  number_from_category <- NULL
  i = 0
  j = 1
  num <- NULL
  b <- FALSE
  for(item in info){
    for(type in category_type){
      if(grepl(type, item)){
        num <- type
        b <- TRUE
        break
      }
    }
    if(b){
      b <- FALSE
      i = i + 1
    }else{
      category[j] <- paste("Category", i, sep = " ")
      number_from_category[j] <- num
      j = j + 1
    }
  }
  return(data.frame(category, number_from_category))
}

# TODO: identify by type of bullet point instead of string