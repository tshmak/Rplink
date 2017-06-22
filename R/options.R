if(Sys.info()["sysname"] == "Windows") {
  options(Rplink.plink.exec="D:/PLINK/plink.exe")
} else {
  options(Rplink.plink.exec="/home/tshmak/software/plink/v1.90b3.44/plink")
}

if("data.table" %in% .packages(all.available=T)) {
  options(Rplink.use.fread=T)
} else {
  options(Rplink.use.fread=F)
}

