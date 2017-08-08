nrow.bfile <- function(bfile) {

  if(inherits(bfile, "pfile")) {
    ext <- ".psam"
    file <- paste0(bfile, ext)
    if(Sys.info()["sysname"] == "Windows") {
      nl <- shell(paste("gawk '$0 ~ /#[FI]ID/{nl=0; next} {nl++} END {print nl}'",
                               file), intern=T)
    } else {
      nl <- system(paste("awk '$0 ~ /#[FI]ID/{nl=0; next} {nl++} END {print nl}'",
                                file), intern=T)
    }
    return(as.numeric(nl))
  }

  ext <- ".fam"
  file <- paste0(bfile, ext)
  if(Sys.info()["sysname"] == "Windows") {
    wc.output <- shell(paste("wc -l", file), intern=T)
  } else {
    wc.output <- system(paste("wc -l", file), intern=T)
  }
  return(as.numeric(strsplit(wc.output, split = "\\s+")[[1]][1]))
}
