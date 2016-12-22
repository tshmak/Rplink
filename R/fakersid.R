fakersid <- function(bfile) {
  #### Replace rsid with fake ones in .bim file 
  bimfilename <- paste0(bfile, ".bim")
  bimfile <- read.table2(bimfilename)
  bimfile$V2 <- paste0("fakers", 1:nrow(bimfile))
  write.table2(bimfile, file=bimfilename)
  invisible()
}