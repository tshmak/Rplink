run.plink <- function(cmd, plink2=FALSE) {

  # args <- list(...)
  if(plink2) {
    plink.exec <- getOption("Rplink.plink2.exec")
    if(is.null(plink.exec)) stop("Rplink.plink2.exec not defined in options.")
  } else {
    plink.exec <- getOption("Rplink.plink.exec")
    if(is.null(plink.exec)) stop("Rplink.plink.exec not defined in options.")
  }

  if(Sys.info()["sysname"] == "Windows") {
    Command <- "shell"
  } else {
    Command <- "system"
  }
  args <- list(paste(plink.exec, cmd))

  do.call(Command, args)

}
