
remove_from_string <- function(course, courseCode){
  courseCode <- c(courseCode, " ")
  output <- str_remove_all(course, courseCode)
  return(output)
}
