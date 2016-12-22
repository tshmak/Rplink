plink <- function(bfile=NULL, out=tempfile(pattern="out"),
                  keep=NULL, remove=NULL,
                  extract=NULL, exclude=NULL,
                  chr=NULL, pheno=NULL,
                  covar=NULL,
                  seed=NULL,
                  silent=T,
                  allow.no.sex=T,
                  keep.allele.order=T,
                  cmd="",
                  test=F,
                  savelog=T,
                  threads=NULL,
                  memory=NULL, 
                  replace.bim=NULL,
                  replace.fam=NULL,
                  ...) {

#   Wrapper function to call plink with various defaults. Function will return
#   the argument of 'out', which is just a filename stub. But you can access the
#   various files produced by plink following the stub.
#
#   bfile: The --bfile option. If this is not specified, we'll look for a
#   variable called .bfile in the Global environment. Use bfile() to define
#   these to save you having to define these every time.
#
#   out: The --out option. By default, we'll use a temporary file. You can of
#   course specify your own.
#
#   keep, remove: Sample filtering. Can be given as a filename, or a logical
#   vector (same length as .fam file), or a data.frame/list with two variables:
#   FID, IID.
#
#   extract, exclude: Variant filtering. Can be given as a filename, or a
#   logical vector (same length as .bim file), or a character vector of rs IDs.
#
#   chr: Can be given as a string or a numeric vector.
#
#   pheno: Specify an alternative phenotype. Phenotype can be given as a numeric
#   vector (same length as .fam file) or a filename. The --no-pheno option will
#   be automatically added, so that the original phenotype in the .fam file is
#   ignored. Multiple phenotype is not supported at the moment.
#
#   seed: Integer from 0 to 2^32 - 1
#
#   silent: The --silent option
#
#   cmd: All other arguments to pass to plink, e.g. --assoc --1
#
#   test: If test==T, the plink command is not run, but all the arguments are
#   returned as a string for you to check.
#
#   savelog: By default, the log file''s content will be saved as an attribute
#   to the returned value.
  

  pars <- list(class="plinkout")
  if(cmd == "") stop("cmd must be specified. Perhaps it should be --make-bed?")

  ## bfile
  if(is.null(bfile)) {
    if(exists(".bfile", envir=.GlobalEnv, inherits=F)) {
      pars$bfile <- get(".bfile", envir=.GlobalEnv, inherits=F)
      pars$fam <- paste0(pars$bfile, ".fam")
      pars$bim <- paste0(pars$bfile, ".bim")
      bfile <- paste("--bfile", pars$bfile)
    }
    else bfile <- ""
  }
  else if(bfile=="") {
    bfile <- ""
  }
  else {
    pars$bfile <- bfile
    bfile <- paste("--bfile", bfile)
    pars$fam <- paste0(pars$bfile, ".fam")
    pars$bim <- paste0(pars$bfile, ".bim")
  }
  
  ## replace.bim/replace.fam
  if(!is.null(replace.bim) || !is.null(replace.fam)) {
    if(bfile == "") stop("bfile must be specified!")
    pars$bed <- paste0(pars$bfile, ".bed")
    if(!is.null(replace.bim)) pars$bim <- replace.bim
    if(!is.null(replace.fam)) pars$fam <- replace.fam
    bfile <- paste("--bed", pars$bed, 
                   "--bim", pars$bim, 
                   "--fam", pars$fam) 
  }
  
  ## out
  pars$out <- out
  out <- paste("--out", out)

  ## keep
  if(is.null(keep)) keep <- ""
  else if(is.character(keep) && length(keep) == 1) {
    ## A file is given
    pars$keep <- keep
    keep <- paste("--keep", keep)
  }
  else if(is.logical(keep)) {
    ## A vector of logicals is given
    famfile <- read.table2(pars$fam)
    stopifnot(length(keep) == nrow(famfile))
    kept.famfile <- famfile[keep, ]
    write.table2(kept.famfile, file=keepfile <- tempfile(pattern="keep"))
    pars$keep <- keepfile
    keep <- paste("--keep", keepfile)
  }
  else if(is.list(keep)) {
    stopifnot(!is.null(keep$FID) && !is.null(keep$IID))
    write.table2(data.frame(FID=keep$FID, IID=keep$IID),
                file=keepfile <- tempfile(pattern="keep"))
    pars$keep <- keepfile
    keep <- paste("--keep", keepfile)
  }
  else {
    stop("Don't know what to do yet...")
  }

  ## remove
  if(is.null(remove)) remove <- ""
  else if(is.character(remove) && length(remove) == 1) {
    ## A file is given
    pars$remove <- remove
    remove <- paste("--remove", remove)
  }
  else if(is.logical(remove)) {
    ## A vector of logicals is given
    if(!exists("famfile", inherits=F))
      famfile <- read.table2(pars$fam)
    stopifnot(length(remove) == nrow(famfile))
    kept.famfile <- famfile[remove, ]
    write.table2(kept.famfile, file=removefile <- tempfile(pattern="remove"))
    pars$remove <- removefile
    remove <- paste("--remove", removefile)
  }
  else if(is.list(remove)) {
    stopifnot(!is.null(remove$FID) && !is.null(remove$IID))
    write.table2(data.frame(FID=remove$FID, IID=remove$IID),
                file=removefile <- tempfile(pattern="remove"))
    pars$remove <- removefile
    remove <- paste("--remove", removefile)
  }
  else {
    stop("Don't know what to do yet...")
  }

  ## extract
  if(is.null(extract)) extract <- ""
  else if(is.logical(extract)) {
    ## A vector of logicals is given
    bimfile <- read.table2(pars$bim)
    stopifnot(length(extract) == nrow(bimfile))
    # stopifnot(length(unique(bimfile[,2])) == nrow(bimfile))
    kept.bimfile <- bimfile[extract, 2, drop=F]
    write.table2(kept.bimfile, file=extractfile <- tempfile(pattern="extract"))
    pars$extract <- extractfile
    extract <- paste("--extract", extractfile)
  }
  else if(is.vector(extract)) {
    if(is.numeric(extract))
      stop("We need a list of SNP IDs not numeric vector.
           Try convert to a logical vector")
    else if(is.character(extract)) {
      if(length(extract) == 1)  {
        if(file.exists(extract)) {
          ## A file is given
          pars$extract <- extract
          extract <- paste("--extract", extract)
        }
      }
      if(is.null(pars$extract)) {
        write.table2(extract, file=extractfile <- tempfile(pattern="extract"))
        pars$extract <- extractfile
        extract <- paste("--extract", extractfile)
      }
    } else stop("I don't know what your 'extract' input is")
  }
  else {
    stop("Don't know what to do yet...")
  }

  ## exclude
  if(is.null(exclude)) exclude <- ""
  else if(is.logical(exclude)) {
    ## A vector of logicals is given
    bimfile <- read.table2(pars$bim)
    stopifnot(length(exclude) == nrow(bimfile))
    stopifnot(length(unique(bimfile[,2])) == nrow(bimfile))
    kept.bimfile <- bimfile[exclude, 2, drop=F]
    write.table2(kept.bimfile, file=excludefile <- tempfile(pattern="exclude"))
    pars$exclude <- excludefile
    exclude <- paste("--exclude", excludefile)
  }
  else if(is.vector(exclude)) {
    if(is.numeric(exclude))
      stop("We need a list of SNP IDs not numeric vector.
           Try convert to a logical vector")
    else if(is.character(exclude)) {
      if(length(exclude) == 1)  {
        if(file.exists(exclude)) {
          ## A file is given
          pars$exclude <- exclude
          exclude <- paste("--exclude", exclude)
        }
      }
      if(is.null(pars$exclude)) {
        write.table2(exclude, file=excludefile <- tempfile(pattern="exclude"))
        pars$exclude <- excludefile
        exclude <- paste("--exclude", excludefile)
      }
    } else stop("I don't know what your 'exclude' input is")
  }
  else {
    stop("Don't know what to do yet...")
  }
  
  ## chr
  if(is.null(chr)) chr <- ""
  else {
    pars$chr <- paste(chr, collapse=",")
    chr <- paste("--chr", pars$chr)
  }


  ## pheno
  if(is.null(pheno)) pheno <- ""
  else if(is.character(pheno) && length(pheno) == 1) {
    ## A file is given
    pars$pheno <- pheno
    pheno <- paste("--no-pheno --pheno", pheno)
  }
  else if(is.numeric(pheno) || is.data.frame(pheno)) {
    if(is.data.frame(pheno)) {
      stopifnot(c("FID", "IID" %in% colnames(pheno)))
      stopifnot(ncol(pheno) == 3)
    } else {
      if(!exists("famfile", inherits=F))
        famfile <- read.table2(pars$fam)
      pheno.data.frame <- famfile[,1:2]
      stopifnot(length(pheno) == nrow(famfile))
      pheno <- cbind(pheno.data.frame, pheno)
    }
    write.table2(pheno,
                 file=phenofile <- tempfile(pattern="pheno"))
    pars$pheno <- phenofile
    pheno <- paste("--no-pheno --pheno", phenofile)
  }
  else {
    stop("Don't know what to do yet...")
  }
  
  ## covar
  if(is.null(covar)) covar <- ""
  else if(is.character(covar) && length(covar) == 1) {
    ## A file is given
    pars$covar <- covar
    covar <- paste("--covar", covar)
  }
  else if(is.numeric(covar) || is.data.frame(covar)) {
    if(is.vector(covar)) covar <- matrix(covar, ncol=1)
    if(is.matrix(covar)) covar <- as.data.frame(covar)
    if(!all(c("FID", "IID") %in% colnames(covar))) {
      if(!exists("famfile", inherits=F))
        famfile <- read.table2(pars$fam)
      covar.data.frame <- famfile[,1:2]
      stopifnot(nrow(covar) == nrow(famfile))
      covar <- cbind(covar.data.frame, covar)
    }
    write.table2(covar,file=covarfile <- tempfile(pattern="covar"))
    pars$covar <- covarfile
    covar <- paste("--covar", covarfile)
  }
  else {
    stop("Don't know what to do yet...")
  }
  
  ## seed
  if(!is.null(seed)) {
    pars$seed <- seed
    seed <- paste("--seed", seed)
  }
  else seed <- ""

  ## threads
  if(!is.null(threads)) {
    pars$threads <- threads
    threads <- paste("--threads", threads)
  }
  else threads <- ""
  
  ## memory
  if(!is.null(memory)) {
    pars$memory <- memory
    memory <- paste("--memory", memory)
  }
  else memory <- ""
  
  ## silent
  if(silent) silent <- "--silent" else
    silent <- ""

  ## allow.no.sex
  if(allow.no.sex) allow.no.sex <- "--allow-no-sex" else
    allow.no.sex <- ""
  
  ## keep.allele.order
  if(keep.allele.order) keep.allele.order <- "--keep-allele-order" else
    keep.allele.order <- ""
  
  #################################################################
  plink.command <- paste(bfile, out, keep, remove, extract, exclude,
                         chr, pheno, covar, seed, threads, memory, silent, 
                         allow.no.sex, keep.allele.order, cmd)
  outfile <- gsub(pattern="^--out ", "", out)

  if(test) {
    attributes(plink.command) <- pars
    return(plink.command)
  }
  else {
    run.plink(plink.command, ...)
    attributes(outfile) <- pars
    if(savelog) {
      logfile <- paste0(outfile, ".log")
      if(file.exists(logfile))
        attr(outfile, "log") <- readChar(logfile, file.info(logfile)$size)
    }
    return(outfile)
  }

}
