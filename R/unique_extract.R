unique_extract <- function(bim, logical.extract) {
  #' Function to return a bim file with unique extract elements 
  #' Now you can apply the logical vector with replace.bim=the returned data.frame
  ids.2extract <- bim$ID[logical.extract]
  ids.not2extract <- bim$ID[!logical.extract]
  conflicts <- !logical.extract
  conflicts[conflicts] <- ids.not2extract %in% ids.2extract
  bim$ID[conflicts] <- paste0(bim$ID[conflicts], "dont.use")
  return(bim)
}