rsid <- function(bfile) {
  #' Extract rsid from a bfile
  if(inherits(bfile, "pfile")) stop("Not sure what to do with pfiles yet.")
  return(read.bim(bfile)[,2])
}
