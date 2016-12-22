FIDIID <- function(bfile) {
  #' Extract FID and IID from .fam file
  ids <- read.table2(paste0(bfile, ".fam"))[,1:2]
  colnames(ids) <- c("FID", "IID")
  return(ids)

}
