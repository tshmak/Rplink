check.duplicated.ID <- function(bimfile, snps) {
  if(length(bimfile$ID[bimfile$ID %in% snps]) != length(snps)) {
    stop(paste("Some of the snps selected are not unique by ID.",
               "Perhaps use fakersid() to fill in the SNPs and retry with replace.bim"))
  }
  return(TRUE)
}
