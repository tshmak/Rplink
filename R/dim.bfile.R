dim.bfile <- function(bfile, dim=c(1:2)) {

  #' S3 method for
  n <- integer(0)
  for(i in dim) {
    if(i == 1) {
      file <- paste0(bfile, ".fam")
    } else if(i == 2) {
      file <- paste0(bfile, ".bim")
    }
    if(Sys.info()["sysname"] == "Windows") {
      wc.output <- shell(paste("wc -l", file), intern=T)
    } else {
      wc.output <- system(paste("wc -l", file), intern=T)
    }
    n <- c(n, as.numeric(strsplit(wc.output, split = "\\s+")[[1]][1]))

  }
  return(n)
}
