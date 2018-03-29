if(Sys.info()["sysname"] == "Windows") {
  options(Rplink.plink.exec="D:/PLINK/plink.exe")
} else {
  # if(is.null(getOption("Rplink.plink.exec"))) {
  #   tryget <- Sys.getenv("plink")
  #   if(tryget == "") {
  #     message(paste("Cannot find plink executable. Please specify in 'Rplink.plink.exec',",
  #                "or specify a 'plink' variable in .bashrc"))
  #   } else {
  #     options(Rplink.plink.exec=tryget)
  #   }
  # }
  #
  # if(is.null(getOption("Rplink.plink2.exec"))) {
  #   tryget <- Sys.getenv("plink2")
  #   if(tryget == "") {
  #     message(paste("Cannot find plink2 executable. Please specify in 'Rplink.plink2.exec',",
  #                "or specify a 'plink2' variable in .bashrc"))
  #   } else {
  #     options(Rplink.plink.exec=tryget)
  #   }
  # }
  if(Sys.info()["nodename"] == "GRC170") {
    options(Rplink.plink.exec="/home/tshmak/software/plink/v1.90b5.2/plink")
    # options(Rplink.plink2.exec="/home2/groups/pcsham/projects/UKB/plink2/plink2")
  } else {
    options(Rplink.plink.exec="/home/tshmak/software/plink/v1.90b4.4/plink")
    options(Rplink.plink2.exec="/home2/groups/pcsham/projects/UKB/plink2/plink2")
  }
}

if("data.table" %in% .packages(all.available=T)) {
  options(Rplink.use.fread=T)
} else {
  options(Rplink.use.fread=F)
}

