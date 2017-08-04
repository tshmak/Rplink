pfile <- function(filestub, clear=F) {
  #' bfile for PLINK2 pfiles!
  #'
  if(clear) {
    rm(.bfile, .fam, .bim, .bed, envir=.GlobalEnv)
    return(invisible())
  }

  pfile <- filestub
  class(pfile) <- c("pfile", "bfile")

  assign(".bed", paste0(filestub, ".pgen"), envir=.GlobalEnv)
  assign(".fam", paste0(filestub, ".psam"), envir=.GlobalEnv)
  assign(".bim", paste0(filestub, ".pvar"), envir=.GlobalEnv)
  assign(".bfile", pfile, envir=.GlobalEnv)

}
