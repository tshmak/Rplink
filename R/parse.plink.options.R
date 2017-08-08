parse.plink.options <- function(options) {
  l <- length(options)
  names <- names(options)
  Names <- gsub("\\.", "-", names)
  cmd <- ""
  for(i in 1:l) {
    if(is.null(options[[i]])) options[[i]] <- ""
    cmd <- paste(cmd, paste0("--", Names[i]), options[[i]])
  }
  return(cmd)
}
