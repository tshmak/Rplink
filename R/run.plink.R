run.plink <- function(cmd, plink2=FALSE) {

  # args <- list(...)
  if(plink2) {
    plink.exec <- getOption("Rplink.plink2.exec")
  } else {
    plink.exec <- getOption("Rplink.plink.exec")
  }

  if(Sys.info()["sysname"] == "Windows") {
    Command <- "shell"
  } else {
    Command <- "system"
  }
  args <- list(paste(plink.exec, cmd))

  do.call(Command, args)

}
