## Test environments

* local Windows 10 install, R 4.4.0
* R Under development (unstable) (2024-06-27 r86847 ucrt), 
Windows Server 2022 x64 (build 20348)


* Rhub/actions Windows Server 2022 x64 (build 20348)
* Rhub/actions Ubuntu 22.04.4 LTS, clang-asan, R-devel (2024-06-27 r86847)
* Rhub/actions macos-13 on GitHub, Apple clang version 14.0.0 
(clang-1400.0.29.202), GNU Fortran (GCC) 12.2.0, macOS  Venture 13.6.7

## rhub CMD check results

Status: OK

* elapsed time (Windows Server 2022 x64) : 1:50
* elapsed time (Ubuntu 22.04.4 LTS) : 0:56
* elapsed time (macos-13 on GitHub) : 1:50


## win devel check results
1 NOTE - CRAN incoming feasibility
Indicated possibly misspelled words in DESCRIPTION (APIs, CompTox, bioactivity). 
These are all correctly spelled.


## local R CMD check results

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Paul Kruse <kruse.paul@epa.gov>'
  
  New submission
  

0 errors | 0 warnings | 1 note

* This is a new release. It is a renamed version of the ccdR 1.0.0 package. 
There are a few reasons for renaming this package. ccdR was developed when the
APIs it wraps were primarily pulling data from the CompTox Chemicals Dashboard 
(CCD) and named to reflect that. More data is now available from the APIs than 
is represented by the CCD. The APIs have been renamed to be the 
Computational Toxicology and Exposure (CTX) APIs, which is a stable name that 
more appropriately represents the data domain and area of research the tools 
and resources related to the data represent. ctxR is much more representative 
than ccdR of the current APIs and data. Additionally, the US EPA is 
coordinating several API clients written in different languages to use the 
consistent package name ctx_, where '_' is used to represent the language in 
which a client is developed (e.g. ctxR for R, ctxPy for Python). Renaming ccdR 
to ctxR reflects this harmonization of CTX API clients across languages.
