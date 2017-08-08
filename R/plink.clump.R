plink.clump <- function(pvals, r2, p1=1, p2=1, kb=250, allow.overlap=F,
                        prune=F, load=F, ...) {
  #' FUnction to call plink --clump using user-supplied p-values
  #' A number of options not implemented here. Need to call plink() directly for those.
  #' @param pvals can be given as a vector (must equal number of SNPs), or
  #' a data.frame with two columns, the first column being SNP id and the second being the p-values.
  #' @param prune Actually prune dataset (rather than just output .clumped file)
  #' @param load load pruned dataset
  #' @param ... options passed to plink()

  options1 <- list(...)
  if(is.data.frame(pvals)) {
    stopifnot(ncol(pvals) == 2)
    table <- pvals
  } else {
    stopifnot(is.vector(pvals) & is.numeric(pvals))
    if(is.null(options1$bfile))
      bfile <- get(".bfile", envir=.GlobalEnv, inherits=F)
    else bfile <- options1$bfile

    bim <- read.bim(bfile)
    stopifnot(nrow(bim) == length(pvals))
    table <- cbind(bim$ID, pvals)
  }
  colnames(table) <- c("SNP", "P")
  write.table2(table, file=pvalsfile <- tempfile(pattern="pvals"), col.names=T)
  options <- paste("--clump", pvalsfile,
                   "--clump-p1", p1,
                   "--clump-p2", p2,
                   "--clump-r2", r2,
                   "--clump-kb", kb)
  if(allow.overlap) options <- paste(options, "--allow-overlap")
  options1$cmd <- paste(options1$cmd, options)

  #### run plink ####
  if(!prune) {
    return(do.call("plink", options1))
  }

  outfile <- do.call(plink, options1)
  options2 <- list()
  options2$extract  <- read.table.plink(outfile,ext = ".clumped")$SNP
  options2$cmd <- "--make-bed"
  options2$bfile <- outfile

  if(!load) {
    options2$out <- outfile
    d <- do.call("plink", options2)
    file.remove(paste0(outfile, ".bed~"))
    file.remove(paste0(outfile, ".bim~"))
    file.remove(paste0(outfile, ".fam~"))
    return(d)
  }
  out <- do.call("plink", options2)
  options2$out <- outfile
  options2$cmd <- NULL
  options2$recode <- recode
  options2$fam <- fam
  options2$bfile <- out
  options2$keep <-
    options2$remove <-
    options2$extract <-
    options2$exclude <-
    options2$chr <- NULL
  return(do.call("load.to.R", options2))

}
