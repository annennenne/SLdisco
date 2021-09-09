
shortnum <- function(n) {
  if (n < 1000) {
    return(n)
  } 
  if (n < 10^6) {
    return(paste(n %/% 1000, "K", sep = ""))
  } else {
    return(paste(n %/% 10^6, "M", sep = ""))
  }
}