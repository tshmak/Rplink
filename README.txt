Rplink -- Some tools for calling plink from R 
Author: Timothy Mak
(Tools are continually being updated.)

The essential tools are the functions: 
bfile() 
plink()

To use these tools, you need to first specify the PLINK executable filename in options(Rplink.plink.exec=...) in the file "options.R". 

Generally, you'll also want to run bfile() before you use plink(). bfile() creates four variables: ".bfile", ".bim", ".fam", and ".bed" in the Global environment that the tools can subsequently use. ".bim", ".fam", and ".bed" are strings giving the filenames of the respective .bim, .fam, and .bed files, and ".bfile" gives the stub of these files. If you don't use bfile, e.g. if you use ped/map files, then these are irrelevant and it'll be more cumbersome to use plink() (at the moment). 

plink() then runs plink using the R functions shell() (in Windows) and system() (in Unix). The advantage of using plink() instead of using shell() or system() directly is that you can specify various input to plink as R objects rather than files. Basically plink() writes the various files for you. For further details, read the comments at the beginning of plink.R. 

The idea is that plink() forms the back bone of Rplink, such that if you want to extend Rplink, you can write wrapper that calls plink(). An example is given in plink.score(). This is a convenience function for running allelic score (--score) analysis from plink. plink.score() calls plink() such that it retains all the convenience that plink() has, e.g. you can specify variant/sample filtering criteria as R objects. It also returns the results of the analyses as an R data.frame, rather than the .profile files that plink creates. 