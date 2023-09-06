## Resubmission

This is a resubmission. See NEWS.md for changes. 

## Test environments

* macOS, R 4.3.1
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

# Previous cran-comments

## Resubmission

This is a resubmission. In this version is have added a feature to the shiny interface to create screenshots of application settings. See NEWS.md. 

## Test environments

* macOS, R 4.2.2
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. In this version is have added a feature to the shiny interface to create screenshots of application settings. See NEWS.md. 

## Test environments

* macOS, R 4.2.1
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## Resubmission

This is a resubmission. In this version is addressed a function clash with `rlang` (i.e., `is_empty`) and made adjustments to work with the latest version of `shiny` and `bootstrap4`

## Test environments

* local Ubuntu 20.04 install, R 4.1.0
* local Ubuntu 20.04 through WSL2, R 4.0.5
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## R CMD check results

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details). This version is dependent on the new versions of radiant.data and radiant.model that were recently accepted. 

## Test environments

* local OS X install, R 3.6.3
* local Windows install, R 3.6.3
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. In this version I have tried to address an issue with building a vignette in the radiant package, brought to my attention by an email from Prof Ripley (see below). I was able to trace the problem to a function in radiant.multivariate that was affected by the recent release of dplyr 0.8.1.

Once on CRAN, the fix in this package (radiant.multivariate) should address the issue seen building the vignette in the radiant package

## Test environments

* local OS X install, R 3.6.0
* local Windows install, R 3.6.0
* ubuntu "trusty" (on travis-ci), R old, release, and devel
* win-builder (release and devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


Date: Wed, May 15, 2019 at 12:43 AM
Subject: CRAN package radiant
To: Vincent Nijs <radiant@rady.ucsd.edu>
Cc: CRAN <CRAN@r-project.org>


See https://cran.r-project.org/web/checks/check_results_radiant.html .

This is using a ridiculous amount of RAM for its vignettes: on a larger 
machine I saw a peak allocation of 313Gb, 260 of which was in memory.

Please correct ASAP (using a maximum of 5GB) and before May 22 to safely 
retain the package on CRAN.  Meanwhile we will exclude checking the 
vignettes.

-- 
Brian D. Ripley,                  ripley@stats.ox.ac.uk
Emeritus Professor of Applied Statistics, University of Oxford


## R CMD check results

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local OS X install, R 3.5.2
* local Windows install, R 3.5.2
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

radiant.multivariate was recently archived and removed from CRAN. Uwe Ligges confirmed that multiple emails were sent to my radiant@rady.ucsd.edu email address but, unfortunately, I cannot find any such emails in my inbox. I hope you will accept radiant.multivariate as a resubmission.

## Test environments

* local OS X install, R 3.5.1
* local Windows install, R 3.5.1
* Ubuntu "trusty" (on travis-ci), R oldrel, release, and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

radiant.multivariate depends on the simultaneously submitted radiant.data and radiant.model packages. From the last time I submitted updates to CRAN I seem to recall Uwe Ligges suggested I submit all radiant packages to be updated at the same time. I hope my recollection is correct and that this is indeed the preferred approach that will minimize workload for CRAN.

## Test environments

* local OS X install, R 3.5.0
* local Windows install, R 3.5.0
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Previous cran-comments

## Resubmission

This is a resubmission (0.8.0). In this version I have fixed several bugs and added 
several new features (see NEWS.md for details).

Please note that this version addresses the reverse dependency check warnings from radiant.data for radiant.multivariate. Deprecating the `*_each` commands used in the 0.6.0 versions of the `radiant.*` packages is related to the deprecation of the `*_each` functions in dplyr. I will update the remaining `radiant` package asap after radiant.multivariate is available on CRAN.

## Test environments

* local OS X install, R 3.4
* local Windows install, R 3.4
* ubuntu 14.04 (on travis-ci), R 3.3.3 and R-dev
* win-builder (release)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE about a possibly mis-spelled word ("Analytics"). The spelling is correct however.

## Resubmission

This is a resubmission. In this version I have:

* Fixed an invalid URL README.md (i.e., https://www.r-project.org.org to  https://www.r-project.org

## Resubmission

This is a resubmission. In this version I have:

* Updated a link to https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0) to fix a libcurl error. I prefer to keep this link in the README.md file because it provides a convenient summary of the terms of the AGPL3 license.
* Added a link to  https://www.r-project.org/Licenses/AGPL-3 in the LICENSE file.

## Test environments
* local OS X install, R 3.3.1
* local Windows install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE: New submission
