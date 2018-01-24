plink.merge <- function(bfile.pattern, dir=".", out=NULL,
                        confirm=TRUE,
                        ...) {
  if(length(bfile.pattern) == 1) {
    bed.pattern <- paste0(bfile.pattern, "\\.bed$")
    files <- dir(path=dir, pattern=bed.pattern)
    if(length(files) == 0) stop("No files were found that matched the pattern")
    if(length(files) == 1) stop("Only 1 file was found that matched the pattern.")
    files <- sub("\\.bed", "", files)
  } else {
    stopifnot(is.vector(bfile.pattern))
    files <- bfile.pattern
  }
  message("Merging the following bfiles:")
  # cat(paste(files, collapse="\n"))
  print(files)
  if(confirm) {
    yesno <- readline("Ok?")
    if(!(yesno %in% c("Y", "y"))) return(invisible())
  }

  mergefile <- tempfile(pattern="merge")
  write.table2(files, file=mergefile)
  cmd <- paste("--merge-list", mergefile)
  return(plink(bfile="", cmd=cmd, out=out, ...))
}
