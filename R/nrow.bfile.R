nrow.bfile <- function(bfile) {
  famfile <- paste0(bfile, ".fam")
  if(Sys.info()["sysname"] == "Windows") {
    wc.output <- shell(paste("wc -l", famfile), intern=T)
  } else {
    wc.output <- system(paste("wc -l", famfile), intern=T)
  }
  return(as.numeric(strsplit(wc.output, split = "\\s+")[[1]][1]))
}
