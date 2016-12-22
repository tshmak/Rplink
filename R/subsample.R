subsample <- function(..., n=NULL,p=NULL, fill.missing.a2=T, QC=T, chr=NULL,
                        geno= 0.01, hwe= 0.00001, maf=0.01, out=NULL, 
                        P=ifelse(QC,p*2,p), within.chromosome=F) {

  #' Function to obtain a random subset of a bfile
  #' @param P Over-sampling to account for deletion from QC
  #' @within.chromosome sample within chromosome

  #### filter on chromosome first ####
  if(within.chromosome || !is.null(chr)) {
    if(is.null(chr)) {
      CHR <- read.table2(paste0(bfile, ".bim"))[,1]
      chr <- sample(CHR,1)
      cat("Chromosome ", chr, "chosen\n")
    }
    bfile <- plink(cmd="--make-bed", chr=chr,  ...)
  } else if(sum(names(list(...)) != "bfile") > 0) {
    bfile <- plink(cmd="--make-bed", ...)
  } else if(is.null(list(...)$bfile)) {
    if(exists(".bfile", envir = .GlobalEnv)) {
      bfile <- get(".bfile", envir=.GlobalEnv)
    } else {
      stop("bfile not specified and .bfile not found in .GlobalEnv")
    }
  } else bfile <- list(...)$bfile

#   dim.bfile <- dim.bfile(bfile)
  nrow <- nrow.bfile(bfile)
  ncol <- ncol.bfile(bfile)

  #### sample ####
  if(!is.null(n) && !is.null(p)) {
    stopifnot(n <= nrow)
    stopifnot(p <= ncol)

    P <- min(ncol, P)

    start <- sample(ncol - P + 1, 1)
    end <- start + P - 1
    tokeep <- sample(nrow, n)
    tokeep <- logical.vector(tokeep, nrow)
    toextract <- logical.vector(start:end, ncol)
    bfile <- plink(bfile=bfile, extract=toextract,
                   keep=tokeep, 
                   cmd="--make-bed")
  } else if(!is.null(n)) {
    stopifnot(n <= nrow)

    tokeep <- sample(nrow, n)
    tokeep <- logical.vector(tokeep, nrow)
    bfile <- plink(bfile=bfile, keep=tokeep, 
                   cmd="--make-bed")

  } else if(!is.null(p)) {
    stopifnot(p <= ncol)

    P <- min(ncol, P)

    start <- sample(ncol - P + 1, 1)
    end <- start + P - 1
    toextract <- logical.vector(start:end, ncol)
    bfile <- plink(bfile=bfile, extract=toextract, 
                   cmd="--make-bed")
  } 

  #### QC ####
  if(QC) {
    cmd <- paste("--geno", geno, "--hwe", hwe, "--maf", maf, "--make-bed")
    bfile <- plink(bfile=bfile, cmd=cmd)
    
    remaining.ncol <- ncol.bfile(bfile)
    
    if(!is.null(p)) {
      if(remaining.ncol < p)
        warning("Number of cols less than p after QC. Perhaps increase P.")
      
      start <- sample(remaining.ncol - p + 1, 1)
      end <- start + p - 1
      bfile <- plink(bfile=bfile, extract=logical.vector(start:end, remaining.ncol),
                     cmd="--make-bed")
    }
  } 
  
  #### Impute and out ####
  cmd <- "--make-bed"
  if(fill.missing.a2) cmd <- paste(cmd, "--fill-missing-a2")
  if(!is.null(out)) {
    bfile <- plink(bfile=bfile,cmd=cmd, out=out)
  } else if(fill.missing.a2) {
    bfile <- plink(bfile=bfile,cmd=cmd)
  }
  return(bfile)
}
