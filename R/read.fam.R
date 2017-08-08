read.fam <- function(bfile, pfile=F, add.ext=TRUE) {

  if(inherits(bfile, "pfile") || pfile) {
    ext <- if(add.ext) ".psam" else ""
    psamfile <- paste0(bfile, ext)
    startline <- find.starting.line(psamfile, "#[FI]ID")
    fam <- read.table.plink(bfile, ext,
                            skip=startline-1,
                            header=TRUE)
    colnames(fam)[1] <- sub("^X\\.", "", colnames(fam)[1])
    return(fam)
  }

  ext <- if(add.ext) ".fam" else ""
  fam <- read.table.plink(bfile, ext)
  colnames(fam)[1:5] <- c("FID", "IID", "PAT", "MAT", "SEX")
  return(fam)

}
