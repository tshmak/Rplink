FIDIID <- function(bfile) {
  #' Extract FID and IID from .fam file
  if(inherits(bfile, "pfile")) stop("Not sure what to do with pfiles yet.")

  ids <- read.fam(bfile)[,1:2]
  colnames(ids) <- c("FID", "IID")
  return(ids)

}
