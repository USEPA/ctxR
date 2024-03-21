## Test environments

* local Windows 10 install, R 4.2.1
* R Under development (unstable) (2024-03-19 r86153 ucrt)
* Rhub Windows Server 2022, R-devel, 64 bit
* Rhub Fedora Linux, R-devel, clang, gfortran
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC


## rhub CMD check results
- Most notes appear to be related to testing (rhub) environment

Possibly misspelled words in DESCRIPTION:
  APIs (3:48, 21:55, 23:72, 24:56)
  CCD (25:54)
  CCTE (3:43, 21:48, 23:67)
  CompTox (25:25)
  bioactivity (20:43)
  ccdR (22:42)

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
    From: inst/doc/Bioactivity.html
          inst/doc/Chemical.html
          inst/doc/Hazard.html
    Status: Error
    Message: libcurl error code 35:
      	error:0A000152:SSL routines::unsafe legacy renegotiation disabled
  URL: https://comptox.epa.gov/dashboard/chemical-lists/NATADB
    From: inst/doc/Bioactivity.html
          inst/doc/Chemical.html
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
Indicated possibly misspelled words in DESCRIPTION (APIs, CCD, CCTE, CompTox, bioactivity, ccdR). These are all correctly spelled.


## local R CMD check results

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Paul Kruse <kruse.paul@epa.gov>'
  
  New submission
  
  Size of tarball: 25660998 bytes

0 errors | 0 warnings | 1 note

* This is a new release.
