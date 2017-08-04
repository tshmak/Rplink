parse.plink.options <- function(options) {
  l <- length(options)
  names <- names(options)
  Names <- gsub("\\.", "-", names)
  cmd <- ""
  for(i in 1:l) {
    if(is.null(l[[i]])) l[[i]] <- ""
    cmd <- paste(cmd, paste0("--", Names[i]), l[[i]])
  }
  return(cmd)
}
