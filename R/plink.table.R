plink.table <- function(...) {

  ### Run plink and then attempt to find table (text file)
  ### produced by plink and load automatically

  file <- plink(...)
  log <- attr(file, "log")
  outfilestub <- attr(file, "out")
  log <- gsub(paste0(outfilestub, ".log"), "", log, fixed=T)
  log <- gsub(paste0(outfilestub, ".nosex"), "", log, fixed=T)

  outfilestub.search <- gsub("\\\\", "\\\\\\\\", outfilestub)

### Testing ###
# print(outfilestub.search)
###############

  m <- gregexpr(paste0(outfilestub.search, "\\.[[:alnum:]_\\.-]*[[:alnum:]_-]"), log)
  matches <- unique(regmatches(log, m)[[1]])
  if(length(matches) == 1) {
    table <- read.table2(matches)

    attr(table, "notes") <- attributes(file)
    return(table)
  }
  else if(length(matches) > 1) {
    warning("Multiple output files returned. Returning the plink() output...")
  }
  else {
    warning("No output files found. Returning the plink() output...")
  }
  return(file)
}

