read.table.plink <- function(filestub, ext, ...) {
  result <- NULL
  
  readfile <- paste0(filestub, ext)
  if(file.exists(readfile)) {
    result <- read.table2(readfile, ...)
    attr(result, "table.exists") <- T
  }
  else attributes(result) <- list(table.exists= F)
  
  return(result)
  
}