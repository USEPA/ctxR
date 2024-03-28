## Resubmission
This is a resubmission. In this version I have:

* Addressed CRAN comments to fix Description file misuse of quotations, adjusted 
the title, adjusted spacing.
* Added missing documentation to ccdr_options.Rd, register_ccte.Rd
* Rewrote vignettes, functions, tests to not write to user's home filespace
* Reset user's options in examples, vignettes, and demos
* Reduced the size of the vignettes and associated saved httptest responses 
* Updated the unit tests and reduced the size of the associated saved httptest
responses

## Test environments

* local Windows 10 install, R 4.2.1
* R Under development (unstable) (2024-03-19 r86153 ucrt)
* Rhub Windows Server 2022, R-devel, 64 bit
* Rhub Fedora Linux, R-devel, clang, gfortran
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Mac mini, Apple M1, macOS 13.3.1 (22E261)

## mac release results

Passed all checks.

Status: OK
* using check arguments '--no-clean-on-error '

* elapsed time (check, wall clock): 0:37

## rhub CMD check results
- Most notes appear to be related to testing (rhub) environment

Possibly misspelled words in DESCRIPTION:
  APIs (3:50, 21:51, 23:70, 24:52)
  bioactivity (20:43)
  CompTox (25:21)

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

Found the following (possibly) invalid URLs:
  URL: https://comptox.epa.gov/dashboard/
    From: DESCRIPTION
          inst/doc/Introduction.html
    Status: Error
    Message: libcurl error code 35:
      	error:0A000152:SSL routines::unsafe legacy renegotiation disabled
  URL: https://comptox.epa.gov/dashboard/batch-search
    From: inst/doc/Introduction.html
    Status: Error
    Message: libcurl error code 35:
      	error:0A000152:SSL routines::unsafe legacy renegotiation disabled
  URL: https://comptox.epa.gov/dashboard/chemical-lists/CCL4
    From: inst/doc/Chemical.html
          inst/doc/Hazard.html
    Status: Error
    Message: libcurl error code 35:
      	error:0A000152:SSL routines::unsafe legacy renegotiation disabled
  URL: https://comptox.epa.gov/dashboard/chemical-lists/NATADB
    From: inst/doc/Chemical.html
          inst/doc/Hazard.html
    Status: Error
    Message: libcurl error code 35:
      	error:0A000152:SSL routines::unsafe legacy renegotiation disabled
  URL: https://comptox.epa.gov/dashboard/chemical/invitrodb/DTXSID7020182
    From: inst/doc/Introduction.html
    Status: Error
    Message: libcurl error code 35:
      	error:0A000152:SSL routines::unsafe legacy renegotiation disabled

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found



## win devel check results
1 NOTE - CRAN incoming feasibility
Indicated possibly misspelled words in DESCRIPTION (APIs, CompTox, bioactivity). These are all correctly spelled.


## local R CMD check results

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Paul Kruse <kruse.paul@epa.gov>'
  
  New submission
  

0 errors | 0 warnings | 1 note

* This is a new release.
