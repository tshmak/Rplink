dim.bfile <- function(bfile) {
  #### Determine number of rows in bfile
  if(exists(".bfile", envir=.GlobalEnv) ) {
    .bfile <- get(".bfile", envir=.GlobalEnv)
    if(bfile == .bfile ) {
      if(is.null(attr(.bfile, "Dim"))) {
        bim <- read.table2(paste0(bfile, ".bim"))
        fam <- read.table2(paste0(bfile, ".fam"))
        attr(.bfile, "Dim")  <- c(nrow(fam), nrow(bim))
        assign(".bfile", .bfile, envir=.GlobalEnv)
        
      } 
      return(attr(.bfile, "Dim"))
    }
  }

  bim <- read.table2(paste0(bfile, ".bim"))
  fam <- read.table2(paste0(bfile, ".fam"))
  return(c(nrow(fam), nrow(bim)))
 
}