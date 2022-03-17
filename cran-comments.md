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

One additional NOTE was found with all R-hub and the win-builder checks.
*Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1002/ppj2.20012
    From: inst/CITATION
    Status: 503
    Message: Service Unavailable

Found the following (possibly) invalid DOIs:
  DOI: 10.1002/ppj2.20012
    From: DESCRIPTION
          inst/CITATION
    Status: Service Unavailable
    Message: 503
  DOI: 10.3835/plantgenome2016.12.0130
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503
    
These DOIs in the DESCRIPTION and CITATION files have not changed since the last package version (0.2.3; March 7, 2022). They have been manually checked and are correct. They do not produce NOTEs locally or with GitHub Actions checks. 

## Downstream dependencies
There are currently no downstream dependencies for this package.
