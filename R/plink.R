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
                  pfile=NULL,
                  plink2=FALSE,
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

  if(cmd == "") stop("cmd must be specified. Perhaps it should be --make-bed?")
  pars <- list()

  ## pfile
  if(plink2) {
    if(is.null(pfile)) {
      if(exists(".bfile", envir=.GlobalEnv, inherits=F)) {
        .bfile <- get(".bfile", envir=.GlobalEnv, inherits=F)
        pars$pfile <- .bfile
      }
    } else if(pfile=="") {
      # stop("Why do you need to run plink2 if you're not using pfile?")
    } else if(!is.null(bfile)) {
      if(bfile != pfile) stop("If you specify both pfile and bfile, they should have the same stub.")
      pars$bpfile <- pfile
    } else {
      pars$pfile <- pfile
    }

    if(!is.null(pars$pfile)) {
      .bed <- paste0(pars$pfile, ".pgen")
      .bim <- paste0(pars$pfile, ".pvar")
      .fam <- paste0(pars$pfile, ".psam")
    }
    class(pfile) <- unique(c("pfile", class(pfile)))

  } else {
    if(!is.null(pfile)) stop("pfile can only be specified with the plink2 option.")
  }

  ## bfile
  if(is.null(bfile)) {
    if(exists(".bfile", envir=.GlobalEnv, inherits=F)) {
      .bfile <- get(".bfile", envir=.GlobalEnv, inherits=F)
      pars$bfile <- .bfile
    }
  } else if(bfile=="") {
    # Do nothing
  } else {
    pars$bfile <- bfile
  }

  if(!is.null(pars$bfile)) {
    .bed <- paste0(pars$bfile, ".bed")
    .bim <- paste0(pars$bfile, ".bim")
    .fam <- paste0(pars$bfile, ".fam")
  }

  ## replace.bim/replace.fam
  if(!is.null(replace.bim) || !is.null(replace.fam)) {
    if(bfile == "") stop("bfile must be specified!")
    if(!is.null(pars$pfile)) stop("You cannot specify pfile with replace.bim/replace.fam yet.")
    pars$bed <- .bed
    pars$bim <- .bim
    pars$fam <- .fam
    pars$bfile <- NULL

    ## replace.bim
    if(!is.null(replace.bim)) {

      if(is.character(replace.bim) && length(replace.bim) == 1) {
        ## A file is given
        pars$bim <- replace.bim
      }
      else if(is.data.frame(replace.bim)) {
        # stopifnot(nrow(replace.bim) == ncol.bfile(bfile))
        write.table2(replace.bim,
                     file=replace.bimfile <- tempfile(pattern="replace.bim"))
        pars$bim <- replace.bimfile
      }
      else {
        stop("Don't know what to do yet...")
      }

    }

    ## replace.fam
    if(!is.null(replace.fam)) {

      if(is.character(replace.fam) && length(replace.fam) == 1) {
        ## A file is given
        pars$fam <- replace.fam
      }
      else if(is.data.frame(replace.fam)) {
        # stopifnot(nrow(replace.fam) == nrow.bfile(bfile))
        write.table2(replace.fam,
                     file=replace.famfile <- tempfile(pattern="replace.fam"))
        pars$fam <- replace.famfile
      }
      else {
        stop("Don't know what to do yet...")
      }

    }

  }

  ## out
  pars$out <- out

  ## keep
  if(is.null(keep)) {
    keep <- ""
  } else if(is.character(keep) && length(keep) == 1) {
    ## A file is given
    pars$keep <- keep
  } else if(is.logical(keep)) {
    ## A vector of logicals is given
    famfile <- read.fam(.fam, pfile=!is.null(pars$pfile), add.ext=F)
    stopifnot(length(keep) == nrow(famfile))
    kept.famfile <- famfile[keep, ]
    write.table2(kept.famfile, file=keepfile <- tempfile(pattern="keep"))
    pars$keep <- keepfile
  } else if(is.list(keep)) {
    stopifnot(!is.null(keep$FID) && !is.null(keep$IID))
    write.table2(data.frame(FID=keep$FID, IID=keep$IID),
                 file=keepfile <- tempfile(pattern="keep"))
    pars$keep <- keepfile
  } else {
    stop("Don't know what to do yet...")
  }

  ## remove
  if(is.null(remove)) {
    remove <- ""
  } else if(is.character(remove) && length(remove) == 1) {
    ## A file is given
    pars$remove <- remove
  } else if(is.logical(remove)) {
    ## A vector of logicals is given
    if(!exists("famfile", inherits=F))
      famfile <- read.table2(.fam)
    stopifnot(length(remove) == nrow(famfile))
    kept.famfile <- famfile[remove, ]
    write.table2(kept.famfile, file=removefile <- tempfile(pattern="remove"))
    pars$remove <- removefile
  } else if(is.list(remove)) {
    stopifnot(!is.null(remove$FID) && !is.null(remove$IID))
    write.table2(data.frame(FID=remove$FID, IID=remove$IID),
                 file=removefile <- tempfile(pattern="remove"))
    pars$remove <- removefile
  } else {
    stop("Don't know what to do yet...")
  }

  ## extract
  if(is.null(extract)) {
    extract <- ""
  } else if(is.logical(extract)) {
    ## A vector of logicals is given
    bimfile <- read.bim(.bim, pfile=!is.null(pars$pfile), add.ext=F)
    stopifnot(length(extract) == nrow(bimfile))
    kept.bimfile <- bimfile$ID[extract]
    write.table2(kept.bimfile, file=extractfile <- tempfile(pattern="extract"))
    pars$extract <- extractfile
  } else if(is.vector(extract)) {
    if(is.numeric(extract))
      stop("We need a list of SNP IDs not numeric vector.
           Try convert to a logical vector")
    else if(is.character(extract)) {
      if(length(extract) == 1)  {
        if(file.exists(extract)) {
          ## A file is given
          pars$extract <- extract
        }
      }
      if(is.null(pars$extract)) {
        write.table2(extract, file=extractfile <- tempfile(pattern="extract"))
        pars$extract <- extractfile
      }
    } else stop("I don't know what your 'extract' input is")
  } else {
    stop("Don't know what to do yet...")
  }

  ## exclude
  if(is.null(exclude)) {
    exclude <- ""
  } else if(is.logical(exclude)) {
    ## A vector of logicals is given
    bimfile <- read.bim(.bim, pfile=!is.null(pars$pfile), add.ext=F)
    stopifnot(length(exclude) == nrow(bimfile))
    kept.bimfile <- bimfile$ID[exclude]
    write.table2(kept.bimfile, file=excludefile <- tempfile(pattern="exclude"))
    pars$exclude <- excludefile
  } else if(is.vector(exclude)) {
    if(is.numeric(exclude)) {
      stop("We need a list of SNP IDs not numeric vector.
           Try convert to a logical vector")

    } else if(is.character(exclude)) {
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
  } else {
    stop("Don't know what to do yet...")
  }

  ## chr
  if(!is.null(chr)) {
    pars$chr <- paste(chr, collapse=",")
  }

  ## pheno
  if(!is.null(pheno)) {
    if(is.character(pheno) && length(pheno) == 1) {
      ## A file is given
      pars$pheno <- pheno
      pars$no.pheno <- ""
    } else if(is.numeric(pheno) || is.data.frame(pheno)) {
      if(is.data.frame(pheno)) {
        stopifnot(c("FID", "IID" %in% colnames(pheno)))
        stopifnot(ncol(pheno) == 3)
      } else {
        if(!exists("famfile", inherits=F))
          famfile <- read.fam(.fam, pfile=!is.null(pars$pfile), add.ext=F)
        pheno.data.frame <- famfile[,c("FID", "IID")]
        stopifnot(length(pheno) == nrow(famfile))
        pheno <- cbind(pheno.data.frame, pheno)
      }
      write.table2(pheno,
                   file=phenofile <- tempfile(pattern="pheno"))
      pars$pheno <- phenofile
      pars$no.pheno <- ""
    } else {
      stop("Don't know what to do yet...")
    }
  }
  ## covar
  if(!is.null(covar)) {
    if(is.character(covar) && length(covar) == 1) {
      ## A file is given
      pars$covar <- covar
    } else if(is.numeric(covar) || is.data.frame(covar)) {
      if(is.vector(covar)) covar <- matrix(covar, ncol=1)
      if(is.matrix(covar)) covar <- as.data.frame(covar)
      if(!all(c("FID", "IID") %in% colnames(covar))) {
        if(!exists("famfile", inherits=F))
          famfile <- read.fam(.fam, pfile=!is.null(pars$pfile), add.ext=F)
        covar.data.frame <- famfile[,c("FID", "IID")]
        stopifnot(nrow(covar) == nrow(famfile))
        covar <- cbind(covar.data.frame, covar)
      }
      write.table2(covar,file=covarfile <- tempfile(pattern="covar"))
      pars$covar <- covarfile
    } else {
      stop("Don't know what to do yet...")
    }
  }

  ## seed
  if(!is.null(seed)) {
    pars$seed <- seed
  }

  ## threads
  if(!is.null(threads)) {
    pars$threads <- threads
  }

  ## memory
  if(!is.null(memory)) {
    pars$memory <- memory
  }

  ## silent
  if(silent) pars$silent <- ""

  ## allow.no.sex
  if(allow.no.sex) pars$allow.no.sex <- ""

  ## keep.allele.order
  if(keep.allele.order) pars$keep.allele.order <- ""

  #################################################################
  options <- list(...)
  pars <- c(pars, options)
  plink.command <- parse.plink.options(pars)
  plink.command <- paste(plink.command, cmd)


  if(test) {
    attributes(plink.command) <- pars
    return(plink.command)
  } else {
    pars$class <- "plinkout"
    outfile <- pars$out
    run.plink(plink.command, plink2=plink2)
    attributes(outfile) <- pars

    logfile <- paste0(outfile, ".log")
    if(file.exists(logfile)) {
      log <- readChar(logfile, file.info(logfile)$size)
      if(savelog) {
        attr(outfile, "log") <- log
      }
      has.pgen <- grepl(paste0(outfile, ".pgen"), log)
      has.bed <- grepl(paste0(outfile, ".pgen"), log)
      if(has.pgen) pars$class <- c("pfile", "bfile", pars$class) else
        if(has.bed) pars$class <- c("bfile", pars$class)
    }
    return(outfile)
  }

}
