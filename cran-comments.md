## Resubmission

This is a resubmission. In this version I have:

* Added "()" behind all function names in DESCRIPTION.
* Changed the @exampleIf of the importPhyloseq(), importTreeSummarizedExperiment() and importMicrobiotaProcess() documentation to run only when their respective packages are installed since those packages are SUGGESTs.
* Substantially reworked the importPhyloseq(), importTreeSummarizedExperiment() and importMicrobiotaProcess() functions to speed them up.
* Changed the importMicrobiotaProcess() example to a smaller synthetic case to decrease runtime. 
* Wrapped the importMicrobiotaProcess example in \donttest as it still violates the CRAN time limit requirement due to large overhead of the MicrobiotaProcess and SummarizedExperiment packages.
* Changed the example in plotPARAFACmodel() to reliably meet the timing requirement.
* Changed the example in assessModelStability() to reliably meet the timing requirement.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* This is my first submission of this package.
