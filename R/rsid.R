rsid <- function(bfile) {
  #' Extract rsid from a bfile
  return(read.table2(paste0(bfile, ".bim"))[,2])
}
