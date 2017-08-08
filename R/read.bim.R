read.bim <- function(bfile, pfile=F, add.ext=TRUE) {

  if(inherits(bfile, "pfile") || pfile) {
    ext <- if(add.ext) ".pvar" else ""
    pvarfile <- paste0(bfile, ext)
    startline <- find.starting.line(pvarfile, "#CHROM")
    bim <- read.table.plink(bfile, ext,
                            skip=startline-1,
                            header=TRUE)
    colnames(bim)[1] <- "CHROM"
    return(bim)
  }

  ext <- if(add.ext) ".bim" else ""
  bim <- read.table.plink(bfile, ext)
  colnames(bim)[1:6] <- c("CHROM", "ID", "CM", "POS", "ALT", "REF")
  return(bim)

}
