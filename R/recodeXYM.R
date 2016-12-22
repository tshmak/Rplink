recodeXYM <- function(vec) {
  vec[vec=="X"] <- 23
  vec[vec=="Y"] <- 24
  vec[vec=="M"] <- 25
  return(as.numeric(vec))
}