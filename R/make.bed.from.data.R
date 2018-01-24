make.bed.from.data <- function(mat, fam=NULL, bim=NULL,
                               chr=1,
                               rsid=colnames(mat),
                               pos=1:ncol(mat),
                               cm=0,
                               A1=NULL,
                               A2=NULL,
                               FID=IID,
                               IID=paste0("id", 1:nrow(mat)),
                               father=0,
                               mother=0,
                               sex=0,
                               pheno=0,
                               with.fam=F,
                               keep.ped.map=F,
                               silent=T,
                               out=tempfile(pattern="out")) {
  #' Function to convert a genotype matrix to PLINK .bed file

  # clear(); setwd(attachroot("/WORK/myRpackages/Rplink/tests")); input <- read.table.tim(attachroot("/WORK/2017-11/DavidSiegel/AnotherTest/xinput_train.csv"), sep=","); mat <- as.matrix(input); fam=NULL; bim=NULL; chr=0; rsid=colnames(mat); pos=0;cm=0;A1=NULL;A2=NULL;IID=paste0("id", 1:nrow(mat));FID=IID; father=0;mother=0;sex=0;pheno=0; with.fam=F;out="test1"; keep.ped.map=F

  #### with.fam ####
  if(with.fam) {
    mat <- mat[,-(1:6)]
    if(!is.null(fam)) stop("fam should not be specified together with with.fam")
    fam <- mat[,1:6]
  }

  #### check mat ####
  uniqs <- unique(as.matrix(mat))
  if(!all(uniqs %in% c(0,1,2,NA))) {
    stop("mat contains values that are not either 0, 1, 2, or NA")
  }
  dim <- dim(mat)

  #### has.alleles ####
  has.alleles <- FALSE
  if(!is.null(A1) || !is.null(A2)) {
    has.alleles <- TRUE
    if(is.null(A1) || is.null(A2)) {
      error("Must specify both A1 and A2 if you specify either.")
    }
    stopifnot(length(A1) == ncol(mat))
    stopifnot(length(A2) == ncol(mat))
  }

  #### check fam ####
  if(is.null(fam)) {
    fam <- data.frame(FID=FID, IID=IID, father=father,
                mother=mother, sex=sex, pheno=pheno)
  } else {
    if(is.character(fam) && length(fam) == 1) {
      ## A file is given
      fam <- read.table2(fam)
    }
    else if(is.data.frame(fam)) {
      stopifnot(ncol(fam) == 6)
    }
    else {
      stop("Don't know what to do with this fam object")
    }
  }
  colnames(fam) <- c("FID", "IID", "father", "mother", "sex", "pheno")
  stopifnot(nrow(fam) == nrow(mat))
  stopifnot(!any(duplicated(fam[,1:2])))

  #### genotype matrix ####
  tmat <- t(mat)
  hap1 <- as.integer(tmat > 0)
  hap2 <- as.integer(tmat > 1)

  Mat <- matrix(rbind(hap1, hap2), nrow=nrow(mat), byrow = TRUE)
  Mat <- Mat + 1
  Mat[is.na(Mat)] <- 0
  alleles <- as.data.frame(matrix(c(2,1), ncol=2, nrow=ncol(mat),
                                  byrow = TRUE))
  if(is.null(rsid)) {
    rsid <- paste0("var", 1:ncol(mat))
  }
  alleles$id <- rsid
  alleles.file <- paste0(out, ".alleles")
  write.table2(alleles, file=alleles.file)
  # Mat[Mat == 1] <- "A"
  # Mat[Mat == 2] <- "G"
  # tempfile <- tempfile(pattern="make.bed", fileext=c(".ped", ".map"))
  ped <- cbind(fam, as.data.frame(Mat))
  pedfile <- paste0(out, ".ped")
  write.table2(ped, file=pedfile)

  #### map file ####
  map <- data.frame(chr=chr, rsid=rsid, cm=cm, pos=pos)
  # if(pos == 0) map$pos <- 1:nrow(map)
  # if(chr == 0) map$chr <- 1
  mapfile <- paste0(out, ".map")
  write.table2(map, file=mapfile)

  #### Call plink ####
  out <- plink(bfile="", ped=pedfile, map=mapfile, cmd="--make-bed", out=out,
               a1.allele=paste(alleles.file, 1, 3), silent=silent)
  if(!keep.ped.map) dummy <- file.remove(pedfile, mapfile, alleles.file)

  #### recode alleles ####
  bim <- read.bim(bfile = out)
  if(has.alleles) {
    bim$ALT <- A1
    bim$REF <- A2
    write.table2(bim, file=paste0(out, ".bim"))
  } else {
    bim$ALT <- 1
    bim$REF <- 0
    write.table2(bim, file=paste0(out, ".bim"))
  }

  return(out)

}
