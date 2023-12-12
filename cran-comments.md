## Resubmission
This is a resubmission. In this version, I have added the extra top-level files to .Rbuildignore to address all of the notes generated through the last submission.

## Test environments
* local Apple Silicon (M1), macOS 13.5.2 Ventura, R 4.2.1
* windows-latest (release; R CMD check workflow on Github Actions)
* macOS-latest (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (devel; R CMD check workflow on Github Actions)
* R-hub Windows Server 2022, R-devel, 64 bit
* R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub Fedora Linux, R-devel, clang, gfortran
* win-builder (devel) 

## R CMD check results
There were no ERRORs or WARNINGs.

Three NOTEs were found with the R-hub Windows Server 2022 check:

* checking HTML version of manual ... NOTE
  Skipping checking math rendering: package 'V8' unavailable

* checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
These NOTEs are not reproducible locally or with the Github Actions R CMD check workflow, but a similar note to the first listed here also occurs with the other R-hub checks:

* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' available
  Skipping checking math rendering: package 'V8' unavailable
  
All of these R-hub-related NOTEs are documented R-hub bugs (e.g., https://github.com/r-hub/rhub/issues/548) that are unrelated to the R package being tested.


## Downstream dependencies
There are currently no downstream dependencies for this package.
