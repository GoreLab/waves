## Test environments
* local OS X install, R 3.5.2
* windows-latest (release; R CMD check workflow on Github Actions)
* macOS-latest (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (devel; R CMD check workflow on Github Actions)
* win-builder (release)
* R-hub builder Windows Server (devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

One NOTE was found in both the win-builder and R-hub builder Windows Server checks: 

* "Possibly mis-spelled words in the DESCRIPTION: Phenome" lists a word that is not actually misspelled. 

## Downstream dependencies
There are currently no downstream dependencies for this package.
