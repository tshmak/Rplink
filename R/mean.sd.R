mean.sd <- function(...) {
  #### Get mean and SD of the SNP genotypes in (0/1/2) coding ####
  freqx <- plink(cmd="--freqx", ...)
  freqx2 <- read.table.plink(freqx, ".frqx", sep="\t")
  data <- as.matrix(freqx2[, c("C.HOM.A1.", "C.HET.", "C.HOM.A2.")])
  n <- rowSums(data)
  sumx <- data %*% c(2,1,0)
  meanx <- as.vector(sumx / n)
  sumx2 <- data %*% c(4,1,0)
  var <- as.vector(sumx2)/n - meanx^2
  var <- var * (n-1)/n
  maf <- 0.5 - abs(meanx - 1) / 2
  
  return(cbind(freqx2, data.frame(mean=meanx, maf=maf, 
                                  var=var, sd=sqrt(var))))
  
}