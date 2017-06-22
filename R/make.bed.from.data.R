make.bed.from.data <- function(mat, fam=NULL, bim=NULL, 
                               chr=0, 
                               rsid=colnames(mat), 
                               pos=0,
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
                               out="") {
  #' Function to convert a genotype matrix to PLINK .bed file
  stopifnot(out != "")
  if(with.fam) {
    mat <- mat[,-(1:6)]
    if(!is.null(fam)) stop("fam should not be specified together with with.fam")
    fam <- mat[,1:6]
  }
  uniqs <- unique(as.matrix(mat))
  if(!all(uniqs %in% c(0,1,2,NA))) {
    stop("mat contains values that are not either 0, 1, 2, or NA")
  }
  dim <- dim(mat)
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
  stopifnot(!any(duplicate(fam[,1:2])))
  
  #### genotype matrix ####
  hap1 <- as.integer(mat > 0)
  hap2 <- as.integer(mat > 1)
  
  mat <- t(matrix(rbind(hap1, hap2), ncol=nrow(mat)))
  tempfile <- tempfile(pattern="make.bed", fileext=c(".ped", ".map"))
  
  #### Not finished ####

}