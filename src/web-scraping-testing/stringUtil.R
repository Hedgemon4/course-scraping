
remove_from_string <- function(a, b){
  a = courseCodes
  i = 1
  for(val in a){
    a[i] <- str_remove_all(val, b[i])
    i <- i + 1
  }
  return(a)
}
