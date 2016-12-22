run.plink <- function(cmd, ...) {
  
  args <- list(...)
  if(Sys.info()["sysname"] == "Windows") {
    Command <- "shell"
    args$cmd <- paste(getOption("Rplink.plink.exec"), cmd)
  } else {
    Command <- "system"
    args$command <- paste(getOption("Rplink.plink.exec"), cmd)
  }
  
  
  do.call(Command, args)
  
}
