corr.matrix <- function(..., lag=10, R2=F, window.kb=1e5, cov=F, 
                        window.r2=0, matrix=T, giveCsparse=T) {
  ### obtain correlation/R2 matrix in Matrix::spareMatrix's ijx form
  ### using Plink's --r or --r2
  ### cov: Report covariance rather than correlation

  if(cov && !matrix) stop("Must request matrix form for cov")
  
  cmd <- ifelse(R2, "--r2", "--r")
  cmd <- paste(cmd, "--ld-window", lag+1)
  cmd <- paste(cmd, "--ld-window-kb", format(window.kb, scientific=F))
  cmd <- paste(cmd, "--ld-window-r2", window.r2)
  cor <- plink.table(cmd=cmd, ...)
  
  A <- cor[,c("CHR_A", "BP_A", "SNP_A")]
  names(A) <- c("CHR", "BP", "SNP")
  B <- cor[,c("CHR_B", "BP_B", "SNP_B")]
  names(B) <- c("CHR", "BP", "SNP")
  stacked <- rbind(A,B) 
  order <- order(stacked$CHR, stacked$BP)
  stacked <- stacked[order,]
  levels <- unique(stacked$SNP)
  dim <- length(levels)
  
  list <- list()
  list$i <- factor(cor$SNP_A, levels=levels)
  list$j <- factor(cor$SNP_B, levels=levels)
  list$i <- unclass(list$i)
  list$j <- unclass(list$j)
#   names(list$i) <- names
#   names(list$j) <- names
  list$x <- cor[, ifelse(R2, "R2", "R")]

  #### Adding the diagonals ####
  list$i <- c(list$i, 1:dim)
  list$j <- c(list$j, 1:dim)
  list$x <- c(list$x, rep(1, dim))
  list <- as.data.frame(list)[order(list$i, list$j), ]

  #### Adding other elements ####
  list <- as.list(list)
  list$dims <- c(dim, dim)
  list$giveCsparse <- giveCsparse
  list$symmetric <- T
  list$dimnames <- list(levels, levels)
  if(matrix) {
    toreturn <- do.call(sparseMatrix, list)
    if(cov) {
      sd <- mean.sd(...)$sd
      D <- Diagonal(x=sd)
      toreturn <- crossprod(D, tcrossprod(toreturn, D))
    }
    return(toreturn)
  }
  else {
    return(list)
  }
  
}