print.plinkout <- function(plinkout) {
  ### Function to give the plink output file stub without the attributes
  attributes(plinkout) <- NULL
  print(plinkout)
}