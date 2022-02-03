## Test environments
* local Apple Silicon (M1), macOS 12.1 Monterey, R 4.1.2
* windows-latest (release; R CMD check workflow on Github Actions)
* macOS-latest (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (devel; R CMD check workflow on Github Actions)
* R-hub Fedora Linux, R-devel, clang, gfortran
* R-hub Windows Server 2022, R-devel, 64 bit
* win-builder (devel) 

## R CMD check results
There were no ERRORs or WARNINGs.

One NOTE was found with the R-hub Windows Server 2022 check.
* "checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'"
  
This NOTE is not reproducible locally or with any of the other checks listed above.

## Downstream dependencies
There are currently no downstream dependencies for this package.
