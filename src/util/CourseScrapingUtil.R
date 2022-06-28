# Library Imports
library(rvest)
library(dplyr)
library(stringr)
library(stringi)

get_text_dataframe <- function(course_description_link, ...) {
  # Takes an html link and a list of nodes, and returns the text from the list 
  # of nodes as a dataframe with every node becoming a column
  nodes <- list(...)
  course_information <- NULL
  web_page <- read_html(course_description_link)
  for (item in nodes) {
    information <- html_nodes(web_page, item) %>%
      html_text() %>% str_squish()
    course_information <- cbind(course_information, information)
  }
  return(data.frame(course_information))
}

get_text_css <- function(html_link, node) {
  # Returns the text from the specified css node in a html link
  return(read_html(html_link) %>% html_nodes(node) %>% html_text() %>% str_squish())
}

get_text_xpath <- function(html_link, path) {
  # Returns the text from the specified xpath node in a html link
  return(
    read_html(html_link) %>% html_elements(xpath = path) %>% html_text() %>% str_squish()
  )
}

get_nodes_css <- function(html_link, node) {
  # Returns the specified css nodes from the given link
  return(read_html(html_link) %>% html_nodes(node))
}

get_nodes_xpath <- function(html_link, path) {
  return(read_html(html_link) %>% html_elements(xpath = path))
}

get_item_vector <- function(item_name, n) {
  # Returns a vector of the form item_name i (ie Category 1) where n is the length of the vector
  item_vector <- vector(mode = "character", length = n)
  i <- 1
  while (i <= n) {
    item_vector[i] <- paste(item_name, i, " ")
    i = i + 1
  }
  return(item_vector)
}
