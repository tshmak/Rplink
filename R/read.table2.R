read.table2 <- function(..., fread=getOption("Rplink.use.fread"))  {

  check.header <- function(file, ...) {
    ### Function to check if there is a header in a  text file
    test1 <- read.table(file, header=F, nrows=1, stringsAsFactors=F, ...)
    classes1 <- unlist(lapply(test1, class))  %in% c("character", "logical")
    test2 <- read.table(file, header=F, nrows=1, stringsAsFactors=F, skip=1, ...)
    classes2 <- unlist(lapply(test2, class))  %in% c("character", "logical")
    if(all(classes2 == classes1)) return(F)
    else return(T)
  }

  options <- list(...)
  if(is.null(options$header)) {
    options2 <- options
    options2$stringsAsFactors <- NULL
    options2$nrows <- NULL
    options2$skip <- NULL
    options$header <- do.call("check.header", options2)
  }
  if(is.null(options$stringsAsFactors)) options$stringsAsFactors <- F

  if(is.null(fread)) fread <- F
  if(!fread) {
    return(do.call("read.table",options)) 
  } else {
    if(is.null(options$data.table)) options$data.table <- F
    df <- do.call(data.table::fread,options)
    colnames(df) <- make.names(colnames(df), unique=TRUE)
#     colnames(df) <- sub("^([0-9])", "X\\1", colnames(df))
#     colnames(df) <- gsub("[() ]", ".", colnames(df))
    return(df)
  }


}
