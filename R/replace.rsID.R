replace.rsID <- function(bim, A1=T, A2=T) {
  #' Function to replace rsID with chromosome and positions
  bim[,2] <- paste0("chr", bim[,1], "_", bim[,4])
  if(A1) bim[,2] <- paste(bim[,2], bim[,5], sep="_")
  if(A2) bim[,2] <- paste(bim[,2], bim[,6], sep="_")
  return(bim)
}
