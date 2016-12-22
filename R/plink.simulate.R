plink.simulate <- function(n.SNPs=NULL, 
                           labels=paste0("SNP", 1:length(n.SNPs)), 
                           lower.bound=0.01,
                           upper.bound=1, 
                           OR.het=1, ## Note: over hom alt
                           OR.hom=1, ## Note: over hom alt
                           r2=0, 
                           dominance=0,
                           lower.bound.causal=NULL,
                           upper.bound.causal=NULL,
                           lower.bound.marker=NULL,
                           upper.bound.marker=NULL,
                           ld=0,
                           n=NULL,
                           ncases=NULL,
                           ncontrols=NULL,
                           prevalence=NULL,
                           label=NULL,
                           missing=NULL,
                           tags=F,
                           haps=F,
                           acgt="",   ### Either: "acgt", "1234", "12"
                           cmd="",
                           ...) {     ### to pass to plink()
  
  ### Call plink's simulate function (includes simulate-qt)
  ### The first 11 parameters are for constructing the simulation parameter file.
  ### Each should be given a single value or be a vector
  
  stopifnot(!is.null(n.SNPs))
  
  sim.file.params <- list(n.SNPs, 
                          labels, 
                          lower.bound, 
                          upper.bound, 
                          OR.het, 
                          OR.hom, 
                          r2, 
                          dominance,
                          lower.bound.causal,
                          upper.bound.causal,
                          lower.bound.marker,
                          upper.bound.marker,
                          ld) 
  lengths <- unlist(lapply(sim.file.params, length))
  if(max(lengths) > 1) {
    if(length(unique(lengths[lengths > 1])) > 1) 
      print(sim.file.params)
      stop("Lengths of parameters for simulation parameter file not consistent")
  }
  
  if(is.null(n)) case.controls <- T
  else case.controls <- F
  
  if(!case.controls) { ### qt scenario
    stopifnot(!is.null(n))
    if(tags || haps) {
      stopifnot(!is.null(lower.bound.causal) && 
                  !is.null(upper.bound.causal) && 
                  !is.null(lower.bound.marker) &&
                  !is.null(upper.bound.marker))
      
      if(tags && haps) stop("tags and haps cannot both be specified.")
      else if(tags) tagshaps <- "tags"
      else tagshaps <- "haps"
      
      simfile.data <- data.frame(n.SNPs, 
                                 labels, 
                                 lower.bound.causal,
                                 upper.bound.causal,
                                 lower.bound.marker,
                                 upper.bound.marker,
                                 ld,
                                 r2, 
                                 dominance)
    }
    else {
      tagshaps <- ""
      simfile.data <- data.frame(n.SNPs, 
                              labels, 
                              lower.bound, 
                              upper.bound, 
                              r2, 
                              dominance) 
    }
    write.table2(simfile.data, file=simfile <- tempfile(pattern="simfile"))
    cmd <- paste(cmd, "--simulate-qt", simfile, tagshaps, acgt)
    cmd <- paste(cmd, "--simulate-n", n)
  }
  else {
    stopifnot(!is.null(ncases) && !is.null(ncontrols))
    stopifnot(!is.null(prevalence))
    
    if(tags | haps) {
      stopifnot(!is.null(lower.bound.causal) && 
                  !is.null(upper.bound.causal) && 
                  !is.null(lower.bound.marker) &&
                  !is.null(upper.bound.marker))

      if(tags && haps) stop("tags and haps cannot both be specified.")
      else if(tags) tagshaps <- "tags"
      else tagshaps <- "haps"
      
      simfile.data <- data.frame(n.SNPs, 
                                 labels, 
                                 lower.bound.causal,
                                 upper.bound.causal,
                                 lower.bound.marker,
                                 upper.bound.marker,
                                 ld,
                                 OR.het, 
                                 OR.hom)
    }
    else {
      tagshaps <- ""
      simfile.data <- data.frame(n.SNPs, 
                                    labels, 
                                    lower.bound, 
                                    upper.bound, 
                                    OR.het, 
                                    OR.hom) 
    }
    write.table2(simfile.data, file=simfile <- tempfile(pattern="simfile"))
    cmd <- paste(cmd, "--simulate-prevalence", prevalence)
    cmd <- paste(cmd, "--simulate", simfile, tagshaps, acgt)
    cmd <- paste(cmd, "--simulate-ncases", ncases, "--simulate-ncontrols", ncontrols)  
    
  }
  
  ### Other options
  if(!is.null(label)) cmd <- paste(cmd, "--simulate-label", label)
  if(!is.null(missing)) cmd <- paste(cmd, "--simulate-missing", missing)
  
  return(plink(cmd=cmd, bfile="", ...))
  
}
