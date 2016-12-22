bfile <- function(filestub, clear=F) {
  ### Convenience function to create the variables .bed, .bim, .fam in GlobalEnv
  if(clear) {
    rm(.bed, .bim, .fam, .bfile, envir=.GlobalEnv)
    return(invisible(NULL))
  }
  
  bfile <- filestub
  class(bfile) <- "bfile"
  
  assign(".bed", paste0(filestub, ".bed"), envir=.GlobalEnv)
  assign(".bim", paste0(filestub, ".bim"), envir=.GlobalEnv)
  assign(".fam", paste0(filestub, ".fam"), envir=.GlobalEnv)
  assign(".bfile", bfile, envir=.GlobalEnv)
  
}