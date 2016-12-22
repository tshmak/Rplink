showlog <- function(obj) {
  test <- attr(obj, "log")
  if(is.null(test)) {
    test <- attr(obj, "notes")$log
  }
  if(is.null(test)) stop("Can''t find log")
  else {
    cat(test)
    return(invisible(test))
  }
}