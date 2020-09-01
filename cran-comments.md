## Test environments
* local OS X install, R 3.5.2
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel and release), R 4.0.2
* R-hub builder Windows Server 2008 (release), R 4.0.2 

## R CMD check results
There were no ERRORs or WARNINGs. 

One NOTE was found in the travis-ci check: 

* "checking for future file timestamps ... NOTE unable to verify current time"

One NOTE was found in the win-builder check: 

* "Possibly mis-spelled words in the DESCRIPTION" lists words that are not actually misspelled. 

## Downstream dependencies
There are currently no downstream dependencies for this package.
