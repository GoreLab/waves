## Test environments
* local Apple Silicon (M1), macOS 12.1 Monterey, R 4.1.2
* windows-latest (release; R CMD check workflow on Github Actions)
* macOS-latest (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (release; R CMD check workflow on Github Actions)
* ubuntu 20.04 (devel; R CMD check workflow on Github Actions)
* win-builder (devel) 

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE from win-builder (devel):
"Found the following (possibly) invalid file URI: URI: articles/waves.html"
This note does not appear in any other checks. 
The file mentioned in this note is used for the pkgdown site.


## Downstream dependencies
There are currently no downstream dependencies for this package.
