LDprune <- function(..., 
                    type=c("indep", "indep-pairwise", "indep-pairphase")[2], 
                    window=200, 
                    step=50,
                    threshold=ifelse(type=="indep", 2,0.9),
                    load=F,
                    recode=NULL,
                    fam=NULL) {
  
  options <- list(...)
  if(!is.null(options$cmd)) stop("Don''t specify cmd")

  save.out <- options$out
  options.out <- NULL
  options$cmd <- paste0("--", type)
  options$cmd <- paste(options$cmd, window, step, threshold)
  out <- do.call("plink", options)

  ### We should be able to over-write the extract arguments, since only those
  ### SNPs selected are in .prune.in
  options$extract <- paste0(out, ".prune.in")
  options$cmd <- "--make-bed"
  
  if(!load) {
    options$out <- save.out
    return(do.call("plink", options))
  }
  else {
    out <- do.call("plink", options)
    options$out <- save.out
    options$cmd <- NULL
    options$recode <- recode
    options$fam <- fam
    options$bfile <- out
    options$keep <- 
      options$remove <- 
      options$extract <- 
      options$exclude <- 
      options$chr <- NULL
    return(do.call("load.to.R", options))
  }
}
