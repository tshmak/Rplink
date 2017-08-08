find.starting.line <- function(file, startwith="") {

  r <- readLines(file, n=1000)
  s <- which(grepl(paste0("^", startwith), r))
  if(length(s) > 1) {
    warning(paste("More than 1 starting line found for", file))
    s <- s[length(s)]
  } else if(length(s) == 0) {
    stop(paste("No starting line found for", file))
  }
  return(s)

}
