bed2int <- function(con, readlen=floor(.Machine$integer.max /4),
                    endian = .Platform$endian) {
  #' Load a bed file (as a connection) and convert into integer
  #' readlen=Memory readlen

  int <- readBin(con, what="integer", n=readlen,endian=endian, size=1, signed=F)
  first <- int %/% 64
  remainder.first <- int %% 64
  second <- remainder.first %/% 16
  remainder.second <- remainder.first %% 16
  third <- remainder.second %/% 4
  fourth <- remainder.second %% 4

  res <- as.vector(rbind(fourth, third, second, first))
  res <- 3 - res
  res[res == 2] <- NA
  res[res == 3] <- 2

  return(as.integer(res))

}
