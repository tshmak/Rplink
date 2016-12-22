ncol.bfile <- function(bfile) {
  bimfile <- paste0(bfile, ".bim")
  if(Sys.info()["sysname"] == "Windows") {
    wc.output <- shell(paste("wc -l", bimfile), intern=T)
  } else {
    wc.output <- system(paste("wc -l", bimfile), intern=T)
  }
  return(as.numeric(strsplit(wc.output, split = "\\s+")[[1]][1]))
}
