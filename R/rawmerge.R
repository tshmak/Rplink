rawmerge <- function(..., out="", force=F) {
  #' Function to merge bfiles by directly concatenating them, avoiding all the checks and
  #' hassle in plink. Also much more memory efficient.
  #' Only available on Linux and depends on the utilities "cmp" and "tail"

  bfiles <- unlist(list(...))

  if(out == "") stop("Must specify out (output bfile)")

  fam <- paste0(bfiles, ".fam")
  bim <- paste0(bfiles, ".bim")
  bed <- paste0(bfiles, ".bed")

  stopifnot(sapply(fam, file.exists))
  stopifnot(sapply(bim, file.exists))
  stopifnot(sapply(bed, file.exists))
  ext <- c(".bed", ".bim", ".fam")

  if(!force) {
    message("Asserting all .fam are the same")
    for(i in 1:length(bfiles)) {
      if(i == 1) next
      test <- system(paste("cmp", fam[1], fam[i]), intern=T)
      if(length(test) > 0) stop(paste0(fam[i], " is not the same as ", fam[1]))
    }
  }

  message("Asserting all .bed are legitimate")
  for(i in 1:length(bfiles)) {
    if(rawToChar(readBin(bed[i], what="raw", n=3)) != "l\033\001") {
      stop(paste(bed[i], "does not have the right signature."))
    }
  }

  ## Probably not worth checking cos it's almost always too large
  # message("Asserting the .bim files are the right size")
  # famsize <- nrow.bfile(bfiles[1])
  # for(i in 1:length(bfiles)) {
  #   bimsize <- ncol.bfile(bfiles[i])
  #   bimsize2 <- (bimsize %/% 4 + 1) * 4
  #   expected.size <- bimsize2 * famsize + 3
  #   if(expected.size > .Machine$integer.max) {
  #     warning("Cannot compute expected .bed size accurately for testing because it's too big.")
  #   } else {
  #     if(as.integer(expected.size) != file.size(bed[i])) {
  #       stop(paste("The file sizes are not quite what's expected for", bfile[i]))
  #     }
  #   }
  # }
  #

  message("Copying ", bfiles[1], " to ", out)
  for(e in ext) {
    system(paste("cat", paste0(bfiles[1], e), ">", paste0(out, e)))
    #' I'm not using file.copy because I want the behaviour w.r.t. copying permission
    #' to be more consistent with other Linux commands.
  }

  for(i in 2:length(bfiles)) {
    bed.size <- file.size(bed[i])
    cmd1 <- paste("tail -c +4", bed[i], ">>", paste0(out, ".bed"))
    # print(cmd1)
    cmd2 <- paste("cat", bim[i], ">>", paste0(out, ".bim"))
    # print(cmd2)
    message("Merging ", bfiles[i], " with ", out)
    system(cmd1)
    system(cmd2)

  }
  return(out)

}
