load.to.R <- function(..., recode="A", cmd="--recode", fam=F) {
  ### Function to load plink bed data into R
  ### warning, this could be slow
  ### This uses Plink's recode function. So give options as first argument. 
  ### fam: includes the first 6 columns of the files (often given as 
  ###      FID, IID, PAT, MAT, SEX, PHENOTYPE)

  if(cmd != "--recode") stop("If you''re going to change cmd, don''t use load.to.R")
  options <- list(...)
  options$cmd <- paste(cmd, recode)
  
  file <- do.call("plink.table", options)
  if(!fam) file <- as.matrix(file[,-(1:6)])
  return(file)
}