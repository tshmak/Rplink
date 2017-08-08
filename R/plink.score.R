plink.score <- function(coefs,
                        score.options="",
                        q.score.range=NULL,
                        q.score.range.data=NULL,
                        q.score.range.options="",
                        p.vals=T,
                        noscale=F,
                        ...) {
  ### FUnction to call plink --score with coefs as weights
  ### Optionally can apply p-values cut-offs using Plink's
  ### --q-score-range option
  ### Specify q.score.range as a range of cut-offs,
  ### e.g.: c(0, 0.00001, 0.0001, 0.01, 0.1, 1)
  ### p.vals: Indicate that the cut-offs are p-values
  ###         If they're not, then specify q.score.range in
  ###         in two or three columns. (Only last two columns will
  ###         be used.) See plink manual.
  ### noscale: By default, plink automatically scales the score so that
  ###         it is proportional to the number of variants included. Use this
  ###         option to disable this.
  ### ...: options passed to plink()

  if(noscale) stop("noscale not supported any more. It's too dangerous, as I can't detect the actual number of SNPs used in plink.score.")
  other.options <- list(...)
  if(is.data.frame(coefs)) {
    bim <- coefs
    write.table2(bim, file=coefsfile <- tempfile(pattern="coefs"))
  } else if(is.vector(coefs) && is.numeric(coefs)) {
    if(is.null(other.options$bfile))
      bfile <- get(".bfile", envir=.GlobalEnv, inherits=F) else
        bfile <- other.options$bfile

    bim <- read.bim(bfile)
    stopifnot(nrow(bim) == length(coefs))
    bim <- cbind(bim, coefs)
    write.table2(bim, file=coefsfile <- tempfile(pattern="coefs"))
  } else if(is.character(coefs) && length(coefs) == 1) {
    coefsfile <- coefs
  }
  columns <- ifelse(grepl("^[0-9]+\\s[0-9]*\\s*[0-9]*\\s*",score.options),
                    "", "2 5 7")
  options <- paste("--score", coefsfile, columns, score.options)
  other.options$cmd <- paste(other.options$cmd, options)

  ## q-score-range
  if(!is.null(q.score.range)) {
    if(p.vals) {
      cutoffs <- q.score.range
      stopifnot(all(cutoffs >= 0 & cutoffs <= 1))
      cutoffs <- cutoffs[cutoffs > 0]
      n.cutoffs <- length(cutoffs)
      matrix <- matrix(0, nrow=n.cutoffs, ncol=3)
      matrix[,1] <- 1:n.cutoffs
      matrix[,2] <- 0
      matrix[,3] <- cutoffs

    }
    else {
      stopifnot(length(dim(q.score.range)) == 2)
      if(ncol(q.score.range) == 2) {
        matrix <- cbind(1:nrow(q.score.range), q.score.range)
      }
      else {
        matrix <- q.score.range
        matrix[,1] <- 1:nrow(q.score.range)
      }
    }
    write.table2(matrix, file=qfile <- tempfile(pattern="q"))

    ## Data file
    if(is.character(q.score.range.data) && length(q.score.range.data) == 1) {
      ## A file is given
      qdata <- q.score.range.data
    }
    else if(is.vector(q.score.range.data)) {
      if(is.character(coefs)) stop("Cannot combine with a file input for coefs")
      stopifnot(nrow(bim) == length(q.score.range.data))
      write.table2(cbind(bim$ID, q.score.range.data),
                  file=qdata <- tempfile(pattern="qdata"))
    }
    else {
      stop("I don''t know what to do with this q.score.range.data input.")
    }

    options <- paste("--q-score-range", qfile, qdata, q.score.range.options)
    other.options$cmd <- paste(other.options$cmd, options)

  }

  #### run plink ####
  if(!is.null(other.options$test)) return(do.call(plink, other.options))

  outfile <- do.call(plink, other.options)

  ### Read in table ###
  if(is.null(q.score.range)) {
    table <- read.table.plink(outfile, ".profile")
    if(noscale) table$SCORE <- table$SCORE * 2 * nrow(bim)
    if(attributes(table)$table.exists) {
      attr(table, "notes") <- attributes(outfile)
      return(table)
    }
    else {
      warning(attributes(outfile)$log)
      return(outfile)
    }
  }
  else {
    new.table <- T
    toreturn <- NULL
    for(i in 1:length(cutoffs)) {
      table <- read.table.plink(outfile, paste0(".", i, ".profile"))
      if(noscale) table$SCORE <- table$SCORE * 2 * nrow(bim)
      if(attributes(table)$table.exists) {
        if(new.table) {
          toreturn <- table
          new.table <- F
        }
        else {
          toreturn <- cbind(toreturn, table[,ncol(table)])
        }
        colnames(toreturn)[ncol(toreturn)] <- paste0("SCORE", i)
      }
      else warning(attributes(outfile)$log)

    }
    attr(toreturn, "notes") <- attributes(outfile)
    return(toreturn)
  }

}
