# Library Imports
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

get_text_dataframe <- function(course_description_link, ...){
  # Takes an html link and a list of nodes, and returns the text from the list of nodes as a dataframe
  nodes <- list(...)
  course_information <- NULL
  for (item in nodes){
    information <- read_html(course_description_link) %>% html_nodes(item) %>%
      html_text() %>% str_squish() 
    course_information <- cbind(course_information, information)
  }
  return(data.frame(course_information))
}

get_text_css <- function(html_link, node){
  # Returns the text from the specified css node in a html link
  return(read_html(html_link) %>% html_nodes(node) %>% html_text() %>% str_squish())
}

get_text_xpath <- function(html_link, path){
  # Returns the text from the specified xpath node in a html link
  return(read_html(html_link) %>% html_elements(xpath = path) %>% html_text() %>% str_squish())
}

get_nodes_css <- function(html_link, node){
  # Returns the specified css nodes from the given link
  return(read_html(html_link) %>% html_nodes(node))
}

get_nodes_xpath <- function(html_link, path){
  return(read_html(html_link) %>% html_elements(xpath = path))
}

get_item_vector <- function(item_name, n){
  # Returns a vector of the form item_name i (ie Category 1) where n is the length of the vector
  item_vector <- vector(mode = "character", length = n)
  i <- 1
  while(i <= n){
    item_vector[i] <- paste(item_name, i, " ")
    i + i + 1
  }
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

# TODO: identify by type of bullet point instead of string