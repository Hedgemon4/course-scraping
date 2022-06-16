
remove_from_string <- function(a, b){
  i = 1
  for(val in a){
    remove <- paste(b[i], " ", sep = "")
    a[i] <- str_remove(val, remove)
    i <- i + 1
  }
  return(a)
}
