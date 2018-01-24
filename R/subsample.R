subsample <- function(..., n=NULL,p=NULL, fill.missing.a2=T,
                      QC=T, chr=NULL,
                      geno= 0.01, hwe= 0.00001, maf=0.01, mind=0.01,
                      out=NULL, contiguous=TRUE,
                      P=ifelse(QC,p*2,p), within.chromosome=F,
                      silent=TRUE) {

  #' Function to obtain a random subset of a bfile
  #' @param P Over-sampling to account for deletion from QC
  #' @within.chromosome sample within chromosome

  options <- attributes(plink(cmd="test", test=T, ...))

  bim <- read.bim(options[["bfile", exact=T]])
  #### filter on chr first ####
  if(is.null(chr)) chr <- unique(bim$CHROM)
  if(within.chromosome) {
    cat("Chromosome ", chr, "chosen\n")
    chr <- sample(chr, 1)
  }
  toextract <- bim$ID[bim$CHROM %in% chr]

  #### extract/exclude ####
  if(!is.null(options[["extract", exact=T]])) {
    extract.snps <- read.table2(options[["extract", exact=T]], sep="\t",
                                header=FALSE) # "\t to ensure if there are semicolons in the SNP name it doesn't upset the read
    toextract <- intersect(toextract, extract.snps$V1)
  }
  if(!is.null(options[["exclude", exact=T]])) {
    exclude.snps <- read.table2(options[["exclude", exact=T]], sep="\t",
                                header=FALSE)
    toextract <- toextract[!(toextract %in% exclude.snps$V1)]
  }

  #### keep/remove ####
  fam <- read.fam(options[["bfile", exact=T]])
  tokeep <- fam[,1:2]
  if(!is.null(options[["keep", exact=T]])) {
    keep2 <- read.table2(options[["keep", exact=T]], header=FALSE)
    colnames(keep2)[1:2] <- c("FID", "IID")
    tokeep <- merge(tokeep, keep2[,1:2])
  }
  if(!is.null(options[["remove", exact=T]])) {
    remove2 <- read.table2(options[["remove", exact=T]], header=FALSE)
    colnames(remove2)[1:2] <- c("FID", "IID")
    remove2 <- cbind(remove2[,1:2], toexclude=TRUE)
    tokeep <- merge(tokeep, remove2, all.x=TRUE)
    tokeep$toexclude[is.na(tokeep$toexclude)] <- FALSE
    tokeep <- subset(tokeep, !toexclude, select=1:2)
  }

  ncol <- length(toextract)
  nrow <- nrow(tokeep)

  #### sample ####
  if(!is.null(n)) {
    stopifnot(n <= nrow)

    tokeep.sample <- sample(nrow, n)
    tokeep <- tokeep[tokeep.sample, ]

  }
  if(!is.null(p)) {
    stopifnot(p <= ncol)

    P <- min(ncol, P)

    if(contiguous) {
      start <- sample(ncol - P + 1, 1)
      end <- start + P - 1
      toextract <- toextract[start:end]
    } else {
      toextract <- sample(toextract, P)
    }
  }

  #### QC ####
  if(QC) {
    cmd <- paste("--geno", geno, "--hwe", hwe, "--maf", maf, "--mind", mind,
                 "--make-just-bim")
    qc <- plink(bfile=options$bfile,
                cmd=cmd, extract=toextract, keep=tokeep,
                silent=silent)
    qc.bim <- read.bim(qc)

    remaining.ncol <- nrow(qc.bim)

    if(!is.null(p)) {
      if(remaining.ncol < p) {
        cat("Only", remaining.ncol, "variants left after QC.\n",
            ifelse(P < ncol, "Perhaps increase P.\n", ""))
      }

      if(contiguous) {
        start <- sample(remaining.ncol - p + 1, 1)
        end <- start + p - 1
        toextract <- qc.bim$ID[start:end]
      } else {
        toextract <- sample(qc.bim$ID, p)
      }
    }
  }

  #### Impute and out ####
  bfile <- plink(bfile=options$bfile, silent=silent, keep=tokeep,
                 extract=toextract, out=out,
                 fill.missing.a2=fill.missing.a2,
                 cmd="--make-bed")
  return(bfile)
}
