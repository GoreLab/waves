## Test environments
* local OS X install, R 3.5.2
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel and release), R 4.0.2
* R-hub builder Windows Server 2008 (release), R 4.0.2 

## R CMD check results
There were no ERRORs or WARNINGs. 

One note was found in the travis-ci check: 

* "checking for future file timestamps ... NOTE unable to verify current time"

Three NOTEs were found in the win-builder check: 

* The first, "Possibly mis-spelled words in the DESCRIPTION" lists words that are not actually mis-spelled. 

* The second and third both refer to "Examples with CPU (user + system) or elapsed time > 10s". After initial package submission, it was requested by CRAN that all examples be made executable if they could run in under five minutes. While the time to run is system-dependent, the two examples listed as taking over 10 seconds to run take only 15 seconds in win-builder and do not cause a note on any other system that has been tested (local OS X, ubuntu on travis-ci, and R-hub Windows). If this is not acceptable, the maintainer will revert these examples to be non-executable again.

## Downstream dependencies
There are currently no downstream dependencies for this package.
