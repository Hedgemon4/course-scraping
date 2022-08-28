# Program Requirement Scraping for Wilfrid Laurier University ####

# Scrapes program and concentration requirements for the data science program

# The concentrations in the program are not required, but can be used to
# specialize the degree if desired

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)
library(curl)

# Extends the timeout timer due to program sometimes taking too long to run
options(timeout = max(1000000, getOption("timeout")))

# Course Information ####

# Reads Data Science Program Requirements web page
calendar_page <-
  read_html(
    curl(
      "https://academic-calendar.wlu.ca/program.php?cal=1&d=2589&p=5763&s=1034&y=85",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

# Program Concentrations

# Reads Data Science Program Concentration requirements
concentration_page <-
  read_html(
    curl(
      "https://academic-calendar.wlu.ca/program.php?cal=1&d=2589&p=5764&s=1034&y=85",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

link_text <-
  c((html_nodes(calendar_page, "a") %>% html_text()), (html_nodes(concentration_page, "a"))
    %>% html_text())

links <-
  c((html_nodes(calendar_page, "a") %>% html_attr("href")), (html_nodes(concentration_page, "a")
                                                             %>% html_attr("href")))

course_indices <-
  grep("(CP|ST|MA|DATA|EC|BU|ENTR)([0-9]{3})", link_text)

course_codes <- link_text[course_indices] %>% unique()

course_links <- links[course_indices] %>% unique()

closeAllConnections()

num_courses <- length(course_codes)

# Vectors to store information about courses required by the program
course_name <- vector("character", num_courses)
course_description <- vector("character", num_courses)
credit_amount <- vector("numeric", num_courses)
antireq <- vector("character", num_courses)
coreq <- vector("character", num_courses)
prereq <- vector("character", num_courses)
lecture <- vector("logical", num_courses)
lab <- vector("logical", num_courses)
tutorial <- vector("logical", num_courses)
note <- vector("character", num_courses)
hours <- vector("character", num_courses)

# The following loop visits the page of each course required by the program,
# scrapes the information, and then sorts it into the correct vector using
# regular expressions.

# Tracks which vector index the loop is currently on
i <- 1

for (item in course_links) {
  # Reads webpage for the current course link
  course_page <-
    read_html(curl(
      paste0("https://academic-calendar.wlu.ca/", item),
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    ))
  course_name[i] <-
    html_nodes(course_page, "span:nth-child(2)") %>% html_text()
  credit_amount[i] <-
    html_nodes(course_page, "span~ span") %>% html_text() %>% .[[1]] %>%
    str_extract("(([0-9]{1})(\\.)([0-9]{1}))") %>% as.numeric()
  course_description[i] <-
    html_nodes(course_page, "p") %>% html_text() %>% paste(collapse = "")  %>%
    str_squish()
  # Some pages were formatted differently, so if no course description was found,
  # it searches the other potential location
  if (course_description[i] == "")
    course_description[i] <-
    html_nodes(course_page, ".content div span") %>% html_text() %>% paste(collapse = "")
  hours[i] <-
    html_nodes(course_page, ".hours") %>% html_text() %>% paste(collapse = "")
  lecture[i] <- grepl("Lecture", hours[i])
  lab[i] <- grepl("Lab", hours[i])
  tutorial[i] <- grepl("Tutorial", hours[i])
  # Other course information (prereq, coreq, etc.)
  other_info <-
    html_nodes(
      course_page,
      "#main > div.content > div.reqs > dl > dt, #main > div.content > div.reqs > dl > dd.required"
    ) %>%
    html_text() %>% str_squish()
  prereq[i] <-
    grep("Prerequisites", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  antireq[i] <-
    grep("Exclusions", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  coreq[i] <-
    grep("Co\\-requisites", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  note[i] <-
    grep("Note(s|)", other_info) %>% +1 %>% other_info[.] %>% stri_remove_na() %>% paste0(collapse = "")
  i <- i + 1
}

# Standardizes credits to the same scale used by UBC
credit_amount <- credit_amount * 6

# Creates data frame for courses required by the program
course_information <-
  data.frame(
    course_codes,
    course_name,
    course_description,
    credit_amount,
    antireq,
    coreq,
    prereq,
    hours,
    lecture,
    lab,
    tutorial,
    note
  )

colnames(course_information) <-
  c(
    "Course Code",
    "Course Name",
    "Course Description",
    "Credit Amount",
    "Antirequisite",
    "Corequisite",
    "Prerequisite",
    "Hours",
    "Lecture",
    "Lab",
    "Tutorial",
    "Note"
  )

closeAllConnections()

# Requirement Information ####

# Scrapes the data science program requirements

# Reads the webpage containing the data science program requirements
requirement_page <-
  read_html(
    curl(
      "https://students.wlu.ca/programs/science/data-science/program-requirements.html",
      handle = curl::new_handle("useragent" = "Mozilla/5.0")
    )
  )

# Gets and cleans the description requirements
category_description <-
  html_nodes(requirement_page, "ul:nth-child(4) li") %>% html_text()

# Removes excess information (course names, credit amounts, etc.)
category_description <-
  str_extract_all(
    category_description,
    "(^(?:BU|CP|ENTR|DATA|MA|ST)[0-9]{3})|(\\sor\\s(?:BU|CP|ENTR|DATA|MA|ST)[0-9]{3})|([0-9].*)"
  ) %>%
  unlist()

# Searches for items which should be merged into a single category (contains or)
indices <-
  grep("(\\sor\\s(?:BU|CP|ENTR|DATA|MA|ST)[0-9]{3})",
       category_description)

# Merges these subcategories into the same vector index
category_description[(indices - 1)] <-
  paste0(category_description[(indices - 1)], category_description[indices])

category_description <- category_description[-indices]

num_categories <- length(category_description)

# Vectors for program requirement information
requirement_category <-
  vector(mode = "character", length = num_categories)
category_min <- vector(mode = "numeric", length = num_categories)
category_max <- vector(mode = "numeric", length = num_categories)
isCore <- vector(mode = "logical", length = num_categories)

credit_values <-
  c("0\\.5", "1\\.0", "1\\.5", "2\\.0", "2\\.5", "3\\.0")
new_credit_values <- c("3", "6", "9", "12", "15", "18")

i <- 1

# Changes credit values to the same scale used at UBC
for (item in credit_values) {
  category_description <-
    str_replace_all(category_description, item, new_credit_values[i])
  i <- i + 1
}

# Tracks the current index for the program requirement vectors
i <- 1

# The loop goes through each category description, and calculates the amount of
# credits required and if the category is a core requirement (must be taken)

for (item in category_description) {
  requirement_category[i] <- paste0("Category G", i)
  courses <-
    str_extract_all(item, "(CP|ST|MA|DATA|EC|BU|ENTR)([0-9]{3})", item) %>% unlist()
  item <-
    str_remove_all(item, "(CP|ST|MA|DATA|EC|BU|ENTR)([0-9]{3})")
  credit <- str_extract_all(item, "([0-9]+)") %>% unlist()
  if (length(credit) == 0) {
    credit <-
      filter(course_information, `Course Code` %in% courses) %>%
      select(`Credit Amount`)
    if (grepl("or", item)) {
      category_min[i] <- min(credit)
      category_max[i] <- max(credit)
    } else{
      category_min[i] <- sum(credit)
      category_max[i] <- sum(credit)
    }
    if (category_min[i] == sum(credit) &
        category_max[i] == sum(credit)) {
      isCore[i] = TRUE
    }
  } else{
    isCore[i] <- FALSE
    category_min[i] <- credit[1]
    category_max[i] <- credit[1]
  }
  i <- i + 1
}

# Creates data frame for program requirements
program_requirements <-
  data.frame(requirement_category,
             category_description,
             category_min,
             category_max,
             isCore)

colnames(program_requirements) <- c(
  "Requirement Category",
  "Category Description",
  "Category Minimum Credit Amount",
  "Category Maximum Credit Amount",
  "Core Requirement"
)

# Concentration Requirements ####

# Scrapes potential concentration (specialization) requirements for the program

# Vectors for concentration information
requirement_category <- vector(mode = "character")
category_description <- vector(mode = "character")
category_min <- vector(mode = "numeric")
category_max <- vector(mode = "numeric")
isCore <- vector(mode = "logical")

# Letters for subcategory naming
alphabets <- c("A", "B", "C", "D", "E", "F", "G")

i <- 1

# Tracks which node on the concentration page is being scraped
# (concentrations start from node 11)
j <- 11

# Tracks the category number
k <- 0

# Tracks which alphabet character to use for subcategories
l <- 1

# The loop scrapes each category description, and then determines if it is
# a concentration name, or a requirement for the concentration, and the credit
# amount. If it is a course, it determines if it is a core requirement for the
# concentration.

last_category <- -1
while (j < 18) {
  if (j %% 2 == 1) {
    # Calculates the credit amount for the concentration
    if (last_category != -1) {
      category_min[last_category] <-
        sum(category_min[rep((last_category + 1):(i - 1))])
      category_max[last_category] <-
        sum(category_max[rep((last_category + 1):(i - 1))])
    }
    if (j == 17)
      break
    # Get new category information (new concentration)
    category_description[i] <-
      html_nodes(requirement_page, paste0("p:nth-child(", j, ") strong")) %>%
      html_text()
    k <- k + 1
    l <- 1
    requirement_category[i] <- paste0("Category S", k)
    # Row is a header, so isCore is NA
    isCore[i] <- NA
    last_category <- i
    i <- i + 1
  } else {
    # Gets information about the requirements for the concentration
    requirements_in_subcategory <-
      html_nodes(requirement_page,
                 paste0("#text-1 > ul:nth-child(", j, ") > li")) %>%
      html_text()
    for (item in requirements_in_subcategory) {
      requirement_category[i] <- paste0("Category S", k, alphabets[l])
      z <- 1
      # Changes credit values to UBC equivalent
      for (credit in credit_values) {
        item <-
          str_replace_all(item, credit, new_credit_values[z])
        z <- i + 1
      }
      category_description[i] <- item
      # Determins the amount of credits required by the subcategory, and if it
      # is a core requirement for the concentration
      if (grepl("((O|o)ne\\s)|(3.*credit)", item)) {
        category_max[i] <- 3
        category_min[i] <- 3
        isCore[i] <- FALSE
      } else if (grepl("((T|t)wo\\s)|(6.*credit)", item)) {
        category_max[i] <- 6
        category_min[i] <- 6
        isCore[i] = FALSE
      } else{
        courses_in_subcategory <-
          str_extract_all(item, "(CP|ST|MA|DATA|EC|BU|ENTR)([0-9]{3})") %>% unlist()
        category_description[i] <-
          paste(courses_in_subcategory, collapse = " ")
        credits <-
          filter(course_information,
                 `Course Code` %in% courses_in_subcategory) %>% select(`Credit Amount`) %>% sum()
        category_max[i] <- credits
        category_min[i] <- credits
        # Course is a core requirement if it must be taken
        # (only option in subcategory)
        isCore[i] = ((category_max[i] == category_min[i]) &
                       (category_max[i] == sum(credits)))
      }
      l <- l + 1
      i <- i + 1
    }
  }
  j <- j + 1
}

# Create data frame for concentration requirement information
concentration_requirements <-
  data.frame(requirement_category,
             category_description,
             category_min,
             category_max,
             isCore)

colnames(concentration_requirements) <- c(
  "Requirement Category",
  "Category Description",
  "Category Minimum Credit Amount",
  "Category Maximum Credit Amount",
  "Core Requirement"
)

# Write CSV Files ####

# write.csv(
#   program_requirements,
#   "Wilfrid Laurier University Data Science Program Requirements.csv"
# )
# write.csv(
#   concentration_requirements,
#   "Wilfrid Laurier University Data Science Program Specialization Requirements.csv"
# )

closeAllConnections()
