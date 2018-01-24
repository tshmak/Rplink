fakersid <- function(bfile, duplicated.only=FALSE,
                     overwrite=F, new.bim="") {
  #### Replace rsid with fake ones in .bim file
  if(inherits(bfile, "pfile")) stop("Not sure what to do with pfiles yet.")
  if(new.bim=="" & !overwrite) {
    stop(paste("Specify overwrite=T to overwrite existing .bim file.",
               "or specify new.bim"))
  }
  bimfilename <- paste0(bfile, ".bim")
  if(new.bim == "") new.bim <- bimfilename
  bimfile <- read.bim(bfile)
  fakeids <- paste(bimfile$CHROM, bimfile$POS,
                   paste0("fakers", 1:nrow(bimfile)),
                   sep="_")
  if(duplicated.only) {
    s <- sort(bimfile$ID)
    dup <- unique(s[duplicated(s)])
    Dup <- bimfile$ID %in% dup
    if(any(Dup)) bimfile$ID[Dup] <- fakeids[Dup]
  } else {
    bimfile$ID <- fakeids
  }
  write.table2(bimfile, file=new.bim)
  return(new.bim)
}
