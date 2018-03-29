check.duplicated.ID <- function(bim, snps=NULL, action=c("stop", "return", "false")) {
  message <- paste("Some of the snps selected are not unique by ID.",
        "Perhaps use fakersid() to fill in the SNPs and retry with replace.bim")
  action <- match.arg(action)
  passed <- TRUE
  if(!is.null(snps)) {
    ids <- bim$ID[bim$ID %in% snps]
  } else {
    ids <- bim$ID
  }

  if(length(ids) != length(snps)) {
    if(action == "stop") {
      stop(message)
    } else if(action == "return") {
      return(unique(ids[duplicated(ids)]))
    } else if(action == "false") {
      passed <- FALSE
    }
  }

  return(TRUE)
}
