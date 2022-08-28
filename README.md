# ALT2040 Data Science Program Requirement Scraping

This repository contains R scripts which are used for scraping the program requirements and related subject course calendars for the Data Science major or honours programs at various universities throughout Canada and the United States. 

# Methodology
- The R scripts use an R package called RVest to harvest the program requiredments and course information from the academic calendars of various academic institutions
- This information is then processed using other R packages (stringr, stringi, etc.) and regular expressions into data frames
- The data frames contain vectors which are standardized across universities to follow a similar format for easier compairisons (which is explained below)
- These data frames where then saved as CSV files and uploaded to this repository
- The goal was to put all the information into a common location and make it more acessible for future studies

# Repository Structure
- The repository contains two main folders: `csv-files` and `universities`
- `universities` contains the R scripts for scraping the prorgam requirements and the related course calendars
- `csv-files` contains the csv files generated from the R Scripts (course calendars and program requirements) 

## Course Calendar and Required Course Files
- These files contain the course calendars for either a specific subject (such as computer science), or for any courses required by the major (one CSV for all required courses)
- The files are all layed out similarliy, with the same column names. 
- Depending on the information available on the university, some will have more or less columns. 
- Any columns with a `*` are found on every course CSV file

### Column Names
- Course Code (*)
- Course Name (*)
- Course Description (*)
- Credit Amount (*): In all calendars, this has been standardized to the same scale that UBC uses
- Antirequisite
- Corequisite
- Prerequisite
- Equivalency: Courses which are equivalent to each other
- Recommended/Preperation: Contains any courses which were recomended to take before the course, but not a mandatory requirement
- Hours: Components of course (lecture, lab, tutorial, seminar, etc.)
- Lab, Lecture, Tutorial, Seminar, etc: Logical vectors for each course component which are true if the course contains that component
- Breadth Requirement, Distribution Requirement, Quantitative Requirement, etc: Vectors which indicate if the course fufills various other requirements imposed by the institution for graduation (not used by all institutions and names vary)
- Delivery Format
- Note/ Other Information: any other notes or information about the course not part of the other above vectors

## Program Requirement Files
- These files contain the requirements for the specified program at the university
- Depending on how the program requirements are displayed, some have the requirements for the whole degree, and others for the major or program only
- All the columns listed below are found in all the files, but are organized slightly differently depending on how the institutions academic calendar was formatted

### Column Names
Requirement Category:
- Contains the name for the category or subcategory (Example: Category G1A)
- Starts with an alphabet, which is different depending on if the instituition lists the year with the requirements: G = general/no year listed, F = first year, S = second year, U = upper year, C = Co-Op course
- Followed by a number, which indiactes the requirement order
- If followed by a letter, that means it it a subcategory of the overall category, so the category might be F4, and the subcategory F4A. Note that if a catagory required more subcategories than there are letters, a three digit system is used (001 to 999) instead to indicate the subcategory number (Example: Category G1037)

Category Description:
- Lists the requirements for that category or subcategory
- Depending on how the academic calendar was formatted, and if it is a category or subcategory, it will either have a category requirement description (one of, two of, all of, 7+ credits, etc.) and be potentially followed by a list of courses, or it will just have a list of courses
- Examples: (All of)/(MATH 54 or STAT 89A or EECS 16A& EECS 16B or PHYSICS 89)/(ASTRON 128)/(Students complete all of: BUS 343, BUS 360W, BUS 439, BUS 445, BUS 345, BUS 336)

Category Minimum Credit Amount:
- Lists the minimum number of credits from the category or subcategory to fulfill this requirement, and is standardized to the same scale that UBC uses

Category Maximum Credit Amount:
- Lists the maximum number of credits from the category or subcategory to fulfill this requirement, and is standardized to the same scale that UBC uses

Core Requirement:
- Indicates if the course or requirement is needed to fulfill the program requirements (no alternate options)
