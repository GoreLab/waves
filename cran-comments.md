## Test environments
* local OS X install, R 3.5.2
* windows-latest (release; R CMD check workflow on Github Actions)
* macOS-latest (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (devel; R CMD check workflow on Github Actions)
* R-hub Windows Server 2022, R-devel, 64 bit
* R-hub Fedora Linux, R-devel, gfortran
* win-builder (devel) 

## R CMD check results
There were no ERRORs or WARNINGs. 

One NOTE was found in both the win-builder and R-hub builder Windows Server checks: 
* checking for detritus in the temp directory ... NOTE Found the following files/directories:
  'lastMiKTeXException'
  
This NOTE is not reproducible locally or with any of the other checks listed above.

## Downstream dependencies
There are currently no downstream dependencies for this package.
