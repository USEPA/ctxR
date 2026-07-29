# ctxR: Hazard API

[![](Pictures/ctxR_hex.png)](https://CRAN.R-project.org/package=ctxR)

### Introduction

In this vignette, [CTX Hazard
API](https://comptox.epa.gov/ctx-api/docs/hazard.html) will be explored.

Data for the Hazard API come from the Toxicity Value Database
[(ToxValDB)](https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=344315&Lab=NCCT).
ToxValDB includes data on thousands of chemicals from tens of thousands
of records, with an emphasis on quantitative estimates of relevant
points-of-departure from in vivo toxicology studies, such as no- and
low-observable adverse effect levels, screening levels, reference doses,
tolerable daily intake, etc.

The Aggregated Computational Toxicology Resource
[(ACToR)](https://www.epa.gov/comptox-tools/downloadable-computational-toxicology-data#actor)
is currently being integrated into ToxValDB. ACToR, as described in
[Judson et al (2008)](https://doi.org/10.1016/j.taap.2007.12.037), was
designed to serve as a central location for information on chemical
structure ***in vitro*** bioassays, and ***in vivo*** toxicology assays
used in various [Computational
Toxicology](https://www.epa.gov/comptox-tools) efforts at US EPA.

More information on ToxValDB can be found at
<https://www.epa.gov/comptox-tools/downloadable-computational-toxicology-data#AT>.
Additional resources are available under the “ToxVal” subtopic: [New
Approach Methods
training](https://www.epa.gov/chemical-research/new-approach-methods-nams-training).

**NOTE:** Please see the introductory vignette for an overview of the
*ctxR* package and initial set up instruction with API key storage.

Several ctxR functions can be used to access the CTX Hazard API data, as
described in the following sections.Tables output in each example have
been filtered to only display the first few rows of data.

## Hazard Resource

### Get Hazard Data by DTXSID

[`get_hazard_by_dtxsid()`](https://usepa.github.io/ctxR/reference/get_hazard_by_dtxsid.md)
retrieves all hazard data, both human and EcoTox data.

``` r

hazard_by_dtxsid <- get_hazard_by_dtxsid(DTXSID = 'DTXSID7020182')
```

## Skin Eye Resource

[`get_skin_eye_hazard()`](https://usepa.github.io/ctxR/reference/get_skin_eye_hazard.md)
retrieves hazard data specific to skin and eye hazard.

``` r

skin_eye_hazard <- get_skin_eye_hazard(DTXSID = 'DTXSID7020182')
```

## Cancer Resource

[`get_cancer_hazard()`](https://usepa.github.io/ctxR/reference/get_cancer_hazard.md)
retrieves cancer hazard data.

``` r

cancer_hazard <- get_cancer_hazard(DTXSID = 'DTXSID7020182')
```

## Genetox Resource

[`get_genetox_summary()`](https://usepa.github.io/ctxR/reference/get_genetox_summary.md)
retrieves summary level data for genotoxicity data associated to a
chemical.

``` r

genetox_summary <- get_genetox_summary(DTXSID = 'DTXSID7020182')
```

`get_genetox_detail()` retrieves more detailed genetox data for a
chemical than is provided on the summary level.

``` r

genetox_details <- get_genetox_details(DTXSID = 'DTXSID7020182')
```

## Example Use Case: Comparing Hazard Data Across Chemical Lists

The fourth Drinking Water Contaminant Candidate List (CCL4) is a set of
chemicals that “…are not subject to any proposed or promulgated national
primary drinking water regulations, but are known or anticipated to
occur in public water systems….” Moreover, this list “…was announced on
November 17, 2016. The CCL 4 includes 97 chemicals or chemical groups
and 12 microbial contaminants….” The National-Scale Air Toxics
Assessments (NATA) is “… EPA’s ongoing comprehensive evaluation of air
toxics in the United States… a state-of-the-science screening tool for
State/Local/Tribal agencies to prioritize pollutants, emission sources
and locations of interest for further study in order to gain a better
understanding of risks… use general information about sources to develop
estimates of risks which are more likely to overestimate impacts than
underestimate them….”

These lists can be found in the CCD with additional information at [CCL4
information](https://www.epa.gov/ccl/contaminant-candidate-list-4-ccl-4-0)
and [NATA
information](https://www.epa.gov/national-air-toxics-assessment). The
quotes from the previous paragraph were excerpted from list detail
descriptions found on the CCD.

In this example use case, hazard data will be compared between a water
contaminant priority and an air toxics list.

### Obtain Lists of Chemicals

First, confirm the chemical list to query. We use functions that wrap
some of the Chemical domain endpoints to retrieve information about the
list of chemicals.

``` r

options(width = 100)
ccl4_information <- get_public_chemical_list_by_name('CCL4')
print(ccl4_information, trunc.cols = TRUE)
#>   visibility  id    type                                    label
#> 1     PUBLIC 443 federal WATER|EPA: Chemical Contaminants - CCL 4
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           longDescription
#> 1 The Contaminant Candidate List (CCL) is a list of contaminants that, at the time of publication, are not subject to any proposed or promulgated national primary drinking water regulations, but are known or anticipated to occur in public water systems. Contaminants listed on the CCL may require future regulation under the Safe Drinking Water Act (SDWA). EPA announced the <a href='https://www.epa.gov/ccl/contaminant-candidate-list-4-ccl-4-0' target='_blank'>fourth Drinking Water Contaminant Candidate List (CCL 4)</a> on November 17, 2016. The CCL 4 includes 97 chemicals or chemical groups and 12 microbial contaminants. The group of cyanotoxins on CCL 4 includes, but is not limited to: anatoxin-a, cylindrospermopsin, microcystins, and saxitoxin. The CCL Chemical Candidate Lists are versioned iteratively and this description navigates between the various versions of the lists. The list of substances displayed below represents only the chemical CCL 4 contaminants. For the versioned lists, please use the hyperlinked lists below.<br/><br/>\r\n\r\n<a href='https://comptox.epa.gov/dashboard/chemical_lists/CCL5' target='_blank'>CCL5 - November 2022</a> <br/><br/>\r\n<a href='https://comptox.epa.gov/dashboard/chemical_lists/CCL4' target='_blank'>CCL4 - November 2016</a>\r\n This list<br/><br/>\r\n<a href='https://comptox.epa.gov/dashboard/chemical_lists/CCL3' target='_blank'>CCL3 - October 2009</a> <br/><br/>\r\n<a href='https://comptox.epa.gov/dashboard/chemical_lists/CCL2' target='_blank'>CCL2 - February 2005</a><br/><br/>\r\n<a href='https://comptox.epa.gov/dashboard/chemical_lists/CCL1' target='_blank'>CCL1 - March 1998</a><br/><br/> 
#>   listName chemicalCount            createdAt            updatedAt
#> 1     CCL4           100 2017-12-28T17:58:36Z 2022-10-26T21:14:27Z
#>                                                                                                                                              shortDescription
#> 1 The Contaminant Candidate List (CCL) is a list of contaminants that are known or anticipated to occur in public water systems. Version 4 is known as CCL 4.

natadb_information <- get_public_chemical_list_by_name('NATADB')
print(natadb_information, trunc.cols = TRUE)
#>   visibility  id    type                                            label
#> 1     PUBLIC 454 federal EPA: National-Scale Air Toxics Assessment (NATA)
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              longDescription
#> 1 The National-Scale Air Toxics Assessment (NATA) is EPA's ongoing comprehensive evaluation of air toxics in the United States. EPA developed the NATA as a state-of-the-science screening tool for State/Local/Tribal Agencies to prioritize pollutants, emission sources and locations of interest for further study in order to gain a better understanding of risks.  NATA assessments do not incorporate refined information about emission sources but, rather, use general information about sources to develop estimates of risks which are more likely to overestimate impacts than underestimate them.\r\n\r\nNATA provides estimates of the risk of cancer and other serious health effects from breathing (inhaling) air toxics in order to inform both national and more localized efforts to identify and prioritize air toxics, emission source types and locations which are of greatest potential concern in terms of contributing to population risk.  This in turn helps air pollution experts focus limited analytical resources on areas and or populations where the potential for health risks are highest.  Assessments include estimates of cancer and non-cancer health effects based on chronic exposure from outdoor sources, including assessments of non-cancer health effects for Diesel Particulate Matter (PM). Assessments provide a snapshot of the outdoor air quality and the risks to human health that would result if air toxic emissions levels remained unchanged.
#>   listName chemicalCount            createdAt            updatedAt
#> 1   NATADB           163 2018-02-21T12:04:16Z 2018-11-16T21:42:01Z
#>                                                                                                                shortDescription
#> 1 The National-Scale Air Toxics Assessment (NATA) is EPA's ongoing comprehensive evaluation of air toxics in the United States.
```

Next, retrieve the list of chemicals associated with each list.

``` r

ccl4 <- get_chemicals_in_list('CCL4')
ccl4 <- data.table::as.data.table(ccl4)

natadb <- get_chemicals_in_list('NATADB')
natadb <- data.table::as.data.table(natadb)
```

### Review Genotoxicity Data for a Single Chemical

Using the standard CompTox Chemicals Dashboard approach to access
genotoxicity hazard data, one would navigate to the individual chemical
page for
[DTXSID7020182](https://comptox.epa.gov/dashboard/chemical/genotoxicity/DTXSID7020182)
as shown below.

![Figure 1: CCD Navigation to Hazard Data\>Genotoxicity
Plot](Pictures/BPA_Hazard_data_-_Genotoxicity.png)

*Figure 1: CCD Navigation to Hazard Data\>Genotoxicity Plot*

Figure 2 shows the genotoxicity section of the hazard tab for Bisphenol
A. This page provides a summary of available genotoxicity data as well
as individual reports and samples of such data.

![Figure 2: CCD Hazard Data\>Genotoxicity for Bisphenol
A](Pictures/CCD_BPA_genotoxicity.png)

*Figure 2: CCD Hazard Data\>Genotoxicity for Bisphenol A*

The CTX APIs streamline the process of retrieving this information in a
programmatic fashion. Figure 3 shows the particular set of genotoxicity
resources available in the `Hazard` endpoints of the CTX APIs. There are
both summary and detail resources, reflecting the information one can
find on the CompTox Chemicals Dashboard Genotoxicity page for a given
chemical.

![Figure 3: CTX Hazard APIs for Genotoxicity Endpoints
](Pictures/Hazard_-_genotoxicity_resource.png)

*Figure 3: CTX Hazard APIs for Genotoxicity Endpoints*

### Review Genotoxicity Data for Chemical Lists

The function
[`get_genetox_summary()`](https://usepa.github.io/ctxR/reference/get_genetox_summary.md)
is used to access summary genotoxicity information per chemical. To
query a list of chemicals, rather than searching individually for each
chemical, the batch search version of the function,
[`get_genetox_summary_batch()`](https://usepa.github.io/ctxR/reference/get_genetox_summary_batch.md),
can be used to access these details.

First, pull the data.

``` r

ccl4_genotox <- get_genetox_summary_batch(DTXSID = ccl4$dtxsid)
natadb_genetox <- get_genetox_summary_batch(DTXSID = natadb$dtxsid)
```

Next, it may be helpful to examine the dimensions and column names of
the output.

``` r

dim(ccl4_genotox)
#> [1] 71 10
dim(natadb_genetox)
#> [1] 153  10
colnames(ccl4_genotox)
#>  [1] "id"               "dtxsid"           "reportsPositive"  "reportsNegative"  "reportsOther"    
#>  [6] "ames"             "micronucleus"     "clowderDocId"     "genetoxCall"      "genetoxSummaryId"
head(ccl4_genotox)
#>       id        dtxsid reportsPositive reportsNegative reportsOther     ames micronucleus
#>    <int>        <char>           <int>           <int>        <int>   <char>       <char>
#> 1:    92 DTXSID0020153              20               5            1 positive     positive
#> 2:  4399 DTXSID0020446               0               8            0 negative     negative
#> 3:   930 DTXSID0020573               3               9            0 negative     negative
#> 4:    93 DTXSID0020600              20               0            1 positive     positive
#> 5:  2079 DTXSID0020814               1               0            0     <NA>         <NA>
#> 6:   320 DTXSID0021464               8               6            0 positive     positive
#> 3 variables not shown: [clowderDocId <char>, genetoxCall <char>, genetoxSummaryId <int>]
```

The information returned is of the first variety highlighted in the
Figure 2, that is, summary data on the available genotoxicity data for
each chemical. Observe genotoxicity data was returned for 71 chemicals
from the CCL4 chemical list and 153 from the NATA chemical list.
Chemicals missing genotoxicity data for each list are noted below.

``` r

ccl4[!(dtxsid %in% ccl4_genotox$dtxsid), 
     .(dtxsid, casrn, preferredName, molFormula)]
#>              dtxsid       casrn             preferredName    molFormula
#>              <char>      <char>                    <char>        <char>
#>  1: DTXSID001024118  77238-39-2               Microcystin          <NA>
#>  2:   DTXSID0024052  55290-64-7                Dimethipin     C6H10O4S2
#>  3:   DTXSID0032578  59669-26-0                Thiodicarb  C10H18N4O4S3
#>  4:   DTXSID1037484 194992-44-4             Acetochlor OA     C14H19NO4
#>  5:   DTXSID1037486 171262-17-2 2-[(2,6-Diethylphenyl)(me     C14H19NO4
#>  6:   DTXSID1037567 171118-09-5           Metolachlor ESA    C15H23NO5S
#>  7:   DTXSID2022333    135-98-8          sec-Butylbenzene        C10H14
#>  8:   DTXSID2031083 143545-90-8        Cylindrospermopsin   C15H21N5O7S
#>  9:   DTXSID2037506  16655-82-6       3-Hydroxycarbofuran     C12H15NO4
#> 10:   DTXSID2052156    517-09-9                 Equilenin      C18H18O2
#> 11:   DTXSID3021857  25154-52-3               Nonylphenol       C15H24O
#> 12:   DTXSID3034458  99129-21-2                 Clethodim  C17H26ClNO3S
#> 13:   DTXSID3042219    103-65-1             Propylbenzene         C9H12
#> 14:   DTXSID3073137  14866-68-3                  Chlorate          ClO3
#> 15:   DTXSID3074313  35523-89-8                 Saxitoxin    C10H17N7O4
#> 16:   DTXSID4022448  51218-45-2               Metolachlor   C15H22ClNO2
#> 17:   DTXSID4032611  13194-48-4                  Ethoprop    C8H19O2PS2
#> 18:   DTXSID4034948 112410-23-8              Tebufenozide    C22H28N2O2
#> 19:  DTXSID50867064  64285-06-9                Anatoxin a      C10H15NO
#> 20:   DTXSID6024177  10265-92-6             Methamidophos     C2H8NO2PS
#> 21:   DTXSID6037483 187022-11-3            Acetochlor ESA    C14H21NO5S
#> 22:   DTXSID6037485 142363-53-9              Alachlor ESA    C14H21NO5S
#> 23:   DTXSID6037568 152019-73-3            Metolachlor OA     C15H21NO4
#> 24:   DTXSID7024241  42874-03-3               Oxyfluorfen C15H11ClF3NO4
#> 25:   DTXSID7047433    474-86-2                   Equilin      C18H20O2
#> 26:   DTXSID8022377     57-91-0         17alpha-Estradiol      C18H24O2
#> 27:   DTXSID8052483   7440-56-4                 Germanium            Ge
#> 28:   DTXSID9032113 107534-96-3              Tebuconazole   C16H22ClN3O
#> 29:   DTXSID9032329    741-58-2                 Bensulide  C14H24NO4PS3
#>              dtxsid       casrn             preferredName    molFormula
#>              <char>      <char>                    <char>        <char>
natadb[!(dtxsid %in% natadb_genetox$dtxsid), 
       .(dtxsid, casrn, preferredName, molFormula)]
#>             dtxsid        casrn             preferredName molFormula
#>             <char>       <char>                    <char>     <char>
#>  1: DTXSID00872421 NOCAS_872421     Lead & Lead Compounds       <NA>
#>  2:  DTXSID1020273    7782-50-5                  Chlorine        Cl2
#>  3: DTXSID10872417 NOCAS_872417 Cadmium & Cadmium Compoun       <NA>
#>  4: DTXSID30872414 NOCAS_872414 Antimony & Antimony Compo       <NA>
#>  5: DTXSID30872419 NOCAS_872419 Cobalt & Cobalt Compounds       <NA>
#>  6: DTXSID40872425 NOCAS_872425 Nickel & Nickel Compounds       <NA>
#>  7:  DTXSID5024267    1336-36-3 Polychlorinated biphenyls        C12
#>  8:  DTXSID7020687     608-73-1 1,2,3,4,5,6-Hexachlorocyc    C6H6Cl6
#>  9:  DTXSID7023984  NOCAS_23984       Coke oven emissions       <NA>
#> 10: DTXSID90872415 NOCAS_872415 Arsenic & Arsenic Compoun       <NA>
```

Now, genotoxicity details of the chemicals in each chemical list are
returned using the function
[`get_genetox_details_batch()`](https://usepa.github.io/ctxR/reference/get_genetox_details_batch.md).

``` r

ccl4_genetox_details <- get_genetox_details_batch(DTXSID = ccl4$dtxsid)
natadb_genetox_details <- get_genetox_details_batch(DTXSID = natadb$dtxsid)
```

If inspecting the first chemical in each set of results, DTXSID0020153,
notice that the information is identical in each case as this
information is chemical specific and not chemical list specific, though
ordered slightly differently.

``` r

all.equal(ccl4_genetox_details[dtxsid %in% 'DTXSID0020153', ], 
          natadb_genetox_details[dtxsid %in% 'DTXSID0020153', ])
#> [1] "Column 'source': 18 string mismatches"
```

Assays present for chemicals in each chemical list can be explored.
First, determine the unique values of the `assayCategory` column and
then group by these values and determine the number of unique assays for
each `assayCategory` value.

``` r

ccl4_genetox_details[, unique(assayCategory)]
#> [1] "in vivo"  "in vitro" "ND"
natadb_genetox_details[, unique(assayCategory)]
#> [1] "in vivo"  "in vitro" "ND"
ccl4_genetox_details[, unique(assayType)]
#>  [1] "Unscheduled DNA synthesis (UDS) in vitro, DNA effects"                                                                                 
#>  [2] "Mitotic recombination or gene conversion"                                                                                              
#>  [3] "In vivo carcinogenicity studies"                                                                                                       
#>  [4] "Histidine reverse gene mutation, Ames assay"                                                                                           
#>  [5] "micronucleus assay"                                                                                                                    
#>  [6] "InVitroCA"                                                                                                                             
#>  [7] "InVitroMN"                                                                                                                             
#>  [8] "InVitroMLA"                                                                                                                            
#>  [9] "Ames"                                                                                                                                  
#> [10] "InVivoMN"                                                                                                                              
#> [11] "Overall"                                                                                                                               
#> [12] "Sister-chromatid exchange (SCE) in vitro"                                                                                              
#> [13] "Micronucleus test, chromosome aberrations"                                                                                             
#> [14] "Cell transformation, clonal assay"                                                                                                     
#> [15] "Forward and reverse gene mutation, host-mediated assay"                                                                                
#> [16] "Rec-assay, spot test, DNA effects (bacterial DNA repair)"                                                                              
#> [17] "bacterial reverse mutation assay"                                                                                                      
#> [18] "Rec-assay, DNA effects (bacterial DNA repair)"                                                                                         
#> [19] "mammalian cell gene mutation assay"                                                                                                    
#> [20] "in vitro mammalian chromosome aberration test"                                                                                         
#> [21] "in vivo micronucleus (mouse)"                                                                                                          
#> [22] "DNA damage and repair assay, unscheduled DNA synthesis in mammalian cells in vitro"                                                    
#> [23] "Sperm morphology"                                                                                                                      
#> [24] "in vivo micronucleus (rat)"                                                                                                            
#> [25] "Unscheduled DNA synthesis (UDS) in vivo; DNA effects"                                                                                  
#> [26] "Sex-linked recessive lethal gene mutation"                                                                                             
#> [27] "Dominant lethal test"                                                                                                                  
#> [28] "Sister-chromatid exchange (SCE) in vivo"                                                                                               
#> [29] "Forward gene mutation at the HPRT locus"                                                                                               
#> [30] "Chromosome aberrations"                                                                                                                
#> [31] "Reverse gene mutation"                                                                                                                 
#> [32] "InVivoCA"                                                                                                                              
#> [33] "InVivoUDS"                                                                                                                             
#> [34] "transgenic"                                                                                                                            
#> [35] "Heritable translocation test, chromosome aberrations"                                                                                  
#> [36] "mammalian germ cell cytogenetic assay"                                                                                                 
#> [37] "chromosome aberration assay"                                                                                                           
#> [38] "unscheduled DNA synthesis"                                                                                                             
#> [39] "DNA Binding"                                                                                                                           
#> [40] "sister chromatid exchange assay in mammalian cells"                                                                                    
#> [41] "rodent dominant lethal assay"                                                                                                          
#> [42] "bacterial forward mutation assay"                                                                                                      
#> [43] "Bacterial Mutagenesis"                                                                                                                 
#> [44] "In Vitro Micronucleus"                                                                                                                 
#> [45] "Cytogenetics Other"                                                                                                                    
#> [46] "bacterial gene mutation assay"                                                                                                         
#> [47] "in vitro mammalian cell micronucleus test"                                                                                             
#> [48] "Aneuploidy, chromosome aberrations"                                                                                                    
#> [49] "Chromosome aberrations in vivo"                                                                                                        
#> [50] "Cell transformation, viral enhanced"                                                                                                   
#> [51] "sister chromatid exchange assay"                                                                                                       
#> [52] "InVivoDNADamage"                                                                                                                       
#> [53] "Forward gene mutation"                                                                                                                 
#> [54] "Chromosome aberrations in vitro"                                                                                                       
#> [55] "Forward gene mutation at the HPRT or ouabain locus"                                                                                    
#> [56] "Specific locus test, gene mutation"                                                                                                    
#> [57] "Forward gene mutation at the thymidine kinase (TK) locus; chromosome aberrations"                                                      
#> [58] "Spot test, gene mutation"                                                                                                              
#> [59] "In Vivo Non-mammalian Mutagenesis"                                                                                                     
#> [60] "In Vivo Micronucleus"                                                                                                                  
#> [61] "mouse spot test"                                                                                                                       
#> [62] "Micronucleus and sister chromatid exchange"                                                                                            
#> [63] "transgenic rodent mutagenicity assay"                                                                                                  
#> [64] "yeast cytogenetic assay"                                                                                                               
#> [65] "in vivo comet (rat)"                                                                                                                   
#> [66] "in vivo comet (mouse)"                                                                                                                 
#> [67] "in vitro mammalian cell transformation assay"                                                                                          
#> [68] "Cell transformation"                                                                                                                   
#> [69] "Tryptophan reverse gene mutation"                                                                                                      
#> [70] "Gene mutation"                                                                                                                         
#> [71] "Cell Transformation"                                                                                                                   
#> [72] "DNA Damage/Repair"                                                                                                                     
#> [73] "In Vivo Mammalian Mutagenesis"                                                                                                         
#> [74] "In Vitro Chromosome Aberration"                                                                                                        
#> [75] "DNA Covalent Binding"                                                                                                                  
#> [76] "Mutation"                                                                                                                              
#> [77] "in vitro chromosomal aberration study in mammalian cells"                                                                              
#> [78] "Mutation Other"                                                                                                                        
#> [79] "In vitro mammalian chromosomal aberration test"                                                                                        
#> [80] "In Vivo Chromosome Aberration"                                                                                                         
#> [81] "Forward and reverse gene mutation, body fluid assay"                                                                                   
#> [82] "Forward and reverse gene mutation, chromosome aberrations, mitotic recombination and gene conversion, DNA effects, host-mediated assay"
#> [83] "Chromosomal aberration assay"                                                                                                          
#> [84] "Mitotic recombination"                                                                                                                 
#> [85] "Aneuploidy, sex chromosome gain, chromosome aberrations"                                                                               
#> [86] "Aneuploidy, whole sex chromosome loss, chromosome aberrations"                                                                         
#> [87] "fluctuation test"
natadb_genetox_details[, unique(assayType)]
#>   [1] "In vivo carcinogenicity studies"                                                                                                            
#>   [2] "Micronucleus test, chromosome aberrations"                                                                                                  
#>   [3] "Rec-assay, spot test, DNA effects (bacterial DNA repair)"                                                                                   
#>   [4] "Rec-assay, DNA effects (bacterial DNA repair)"                                                                                              
#>   [5] "bacterial reverse mutation assay"                                                                                                           
#>   [6] "Unscheduled DNA synthesis (UDS) in vitro, DNA effects"                                                                                      
#>   [7] "Mitotic recombination or gene conversion"                                                                                                   
#>   [8] "micronucleus assay"                                                                                                                         
#>   [9] "Histidine reverse gene mutation, Ames assay"                                                                                                
#>  [10] "Sister-chromatid exchange (SCE) in vitro"                                                                                                   
#>  [11] "Ames"                                                                                                                                       
#>  [12] "InVitroMLA"                                                                                                                                 
#>  [13] "InVitroMN"                                                                                                                                  
#>  [14] "InVitroCA"                                                                                                                                  
#>  [15] "InVivoMN"                                                                                                                                   
#>  [16] "Overall"                                                                                                                                    
#>  [17] "Cell transformation, clonal assay"                                                                                                          
#>  [18] "Forward and reverse gene mutation, host-mediated assay"                                                                                     
#>  [19] "rodent dominant lethal assay"                                                                                                               
#>  [20] "DNA damage and repair assay, unscheduled DNA synthesis in mammalian cells in vitro"                                                         
#>  [21] "InVivoUDS"                                                                                                                                  
#>  [22] "Chromosome aberrations"                                                                                                                     
#>  [23] "Sister-chromatid exchange (SCE) in vivo"                                                                                                    
#>  [24] "transgenic"                                                                                                                                 
#>  [25] "InVivoCA"                                                                                                                                   
#>  [26] "Unscheduled DNA synthesis (UDS) in vivo; DNA effects"                                                                                       
#>  [27] "Heritable translocation test, chromosome aberrations"                                                                                       
#>  [28] "Sex-linked recessive lethal gene mutation"                                                                                                  
#>  [29] "Reverse gene mutation"                                                                                                                      
#>  [30] "Dominant lethal test"                                                                                                                       
#>  [31] "Forward gene mutation at the HPRT locus"                                                                                                    
#>  [32] "Cell transformation, viral enhanced"                                                                                                        
#>  [33] "DNA Damage/Repair"                                                                                                                          
#>  [34] "Bacterial Mutagenesis"                                                                                                                      
#>  [35] "InVivoDNADamage"                                                                                                                            
#>  [36] "in vitro mammalian chromosome aberration test"                                                                                              
#>  [37] "Cytogenetics Other"                                                                                                                         
#>  [38] "Mutation"                                                                                                                                   
#>  [39] "In Vivo Non-mammalian Mutagenesis"                                                                                                          
#>  [40] "In Vivo Micronucleus"                                                                                                                       
#>  [41] "In Vivo Mammalian Mutagenesis"                                                                                                              
#>  [42] "In Vivo Chromosome Aberration"                                                                                                              
#>  [43] "In Vitro Micronucleus"                                                                                                                      
#>  [44] "In Vitro Chromosome Aberration"                                                                                                             
#>  [45] "mammalian cell gene mutation assay"                                                                                                         
#>  [46] "in vivo micronucleus (mouse)"                                                                                                               
#>  [47] "Sperm morphology"                                                                                                                           
#>  [48] "Gene mutation"                                                                                                                              
#>  [49] "Spot test, gene mutation"                                                                                                                   
#>  [50] "Forward and reverse gene mutation, mitotic recombination and gene conversion, host-mediated assay"                                          
#>  [51] "DNA Binding"                                                                                                                                
#>  [52] "sister chromatid exchange assay in mammalian cells"                                                                                         
#>  [53] "bacterial forward mutation assay"                                                                                                           
#>  [54] "unscheduled DNA synthesis"                                                                                                                  
#>  [55] "bacteriophage induction in E. coli, gene mutation, UDS in mammalian cells, sex-linked recessive lethal mutations in Drosophila"             
#>  [56] "DNA damage, gene mutation, reverse mutation, gene conversion, DNA repair, chromosomal aberration, chromatid exchange, UDS"                  
#>  [57] "Forward gene mutation at the thymidine kinase (TK) locus; chromosome aberrations"                                                           
#>  [58] "chromosome aberration study in mammalian cells"                                                                                             
#>  [59] "in vitro mammalian cell transformation assay"                                                                                               
#>  [60] "Forward gene mutation at the HPRT or ouabain locus"                                                                                         
#>  [61] "Cell transformation"                                                                                                                        
#>  [62] "Forward and reverse gene mutation, body fluid assay"                                                                                        
#>  [63] "Drosophila SLRL assay"                                                                                                                      
#>  [64] "chromosome aberration assay"                                                                                                                
#>  [65] "Salmonella and Escherichia strains: bacterial reverse mutation assay (e.g. Ames test) ; Bacillus strains: recombination assay"              
#>  [66] "Cytogenetic assay in bone marrow cells"                                                                                                     
#>  [67] "Forward gene mutation"                                                                                                                      
#>  [68] "Chromosome aberrations in vivo"                                                                                                             
#>  [69] "Chromosome aberrations in vitro"                                                                                                            
#>  [70] "in vivo comet (mouse)"                                                                                                                      
#>  [71] "in vitro mammalian cell gene mutation tests using the thymidine kinase gene"                                                                
#>  [72] "in vivo micronucleus (rat)"                                                                                                                 
#>  [73] "in vivo comet (rat)"                                                                                                                        
#>  [74] "mouse spot test"                                                                                                                            
#>  [75] "Aneuploidy, whole sex chromosome loss, chromosome aberrations"                                                                              
#>  [76] "sister chromatid exchange assay"                                                                                                            
#>  [77] "Mouse Lymphoma Forward Mutation Assay"                                                                                                      
#>  [78] "mammalian erythrocyte micronucleus test"                                                                                                    
#>  [79] "Tryptophan reverse gene mutation"                                                                                                           
#>  [80] "bacterial gene mutation assay"                                                                                                              
#>  [81] "Micronucleus test in vitro, chromosome aberrations"                                                                                         
#>  [82] "yeast forward mutation and mitotic gene conversion assays in Schizosaccharomyces pombe (P1 strain) and Saccharomyces cerevisiae (D4 strain)"
#>  [83] "heritable translocation assay"                                                                                                              
#>  [84] "mitotic recombination assay with Saccharomyces cerevisiae"                                                                                  
#>  [85] "Aneuploidy, chromosome aberrations"                                                                                                         
#>  [86] "in vitro mammalian cell micronucleus test"                                                                                                  
#>  [87] "cell transformation"                                                                                                                        
#>  [88] "somatic mutation and recombination test in Drosophila"                                                                                      
#>  [89] "transgenic rodent mutagenicity assay"                                                                                                       
#>  [90] "Micronucleus and sister chromatid exchange"                                                                                                 
#>  [91] "yeast cytogenetic assay"                                                                                                                    
#>  [92] "in vitro mammalian cell gene mutation test using the Hprt and xprt genes"                                                                   
#>  [93] "bone marrow chromosome aberration assay and mammalian germ cell cytogenetic assay"                                                          
#>  [94] "bacterial mutation"                                                                                                                         
#>  [95] "bacterial reverse mutation assay (Salmonella typhimurium and Escherichia coli)"                                                             
#>  [96] "Aneuploidy, partial sex chromosome loss, chromosome aberrations"                                                                            
#>  [97] "Chromosome aberrations, in vivo"                                                                                                            
#>  [98] "in vitro chromosome aberration study"                                                                                                       
#>  [99] "Cell transformation, focus assay"                                                                                                           
#> [100] "Forward and reverse gene mutation, mitotic recombination and gene conversion, DNA effects, host-mediated assay"                             
#> [101] "DNA adduct formation"                                                                                                                       
#> [102] "gene mutation assay in fungi"                                                                                                               
#> [103] "DNA Covalent Binding"                                                                                                                       
#> [104] "Cell Transformation"                                                                                                                        
#> [105] "Aneuploidy, sex chromosome gain, chromosome aberrations"                                                                                    
#> [106] "mammalian comet assay"                                                                                                                      
#> [107] "mammalian germ cell cytogenetic assay"                                                                                                      
#> [108] "Forward and reverse gene mutation, chromosome aberrations, mitotic recombination and gene conversion, DNA effects, host-mediated assay"     
#> [109] "E. coli K-12 DNA repair host-mediated assay"                                                                                                
#> [110] "Chromosomal aberration assay"                                                                                                               
#> [111] "forward mutation"                                                                                                                           
#> [112] "mammalian cell gene mutation test"                                                                                                          
#> [113] "Mitotic recombination"
```

Next, determine the number of assays per unique `assayCategory` value,
count the number of assay results and grouping by `assayCategory`, and
`assayType`, and also examine the different numbers of `assayCategory`
and `assayTypes` values used for both chemical lists.

``` r

ccl4_genetox_details[, .(Assays = length(unique(assayType))), 
                     by = .(assayCategory)]
#>    assayCategory Assays
#>           <char>  <int>
#> 1:       in vivo     23
#> 2:      in vitro     62
#> 3:            ND      2
natadb_genetox_details[, .(Assays = length(unique(assayType))),
                       by = .(assayCategory)]
#>    assayCategory Assays
#>           <char>  <int>
#> 1:       in vivo     29
#> 2:      in vitro     82
#> 3:            ND      2

ccl4_genetox_details[, .N, by = .(assayCategory, assayType, assayResult)]
#>      assayCategory                 assayType assayResult     N
#>             <char>                    <char>      <char> <int>
#>   1:       in vivo Unscheduled DNA synthesis    positive     4
#>   2:      in vitro Mitotic recombination or     positive    11
#>   3:       in vivo In vivo carcinogenicity s    positive    22
#>   4:      in vitro Histidine reverse gene mu    positive    10
#>   5:       in vivo        micronucleus assay    negative    34
#>  ---                                                          
#> 144:      in vitro Forward and reverse gene     negative     1
#> 145:      in vitro in vitro mammalian cell t    positive     1
#> 146:      in vitro Aneuploidy, sex chromosom    negative     1
#> 147:      in vitro Aneuploidy, whole sex chr    positive     1
#> 148:      in vitro          fluctuation test    negative     1
ccl4_genetox_details[, .N, by = .(assayCategory)]
#>    assayCategory     N
#>           <char> <int>
#> 1:       in vivo   211
#> 2:      in vitro   728
#> 3:            ND    15
ccl4_genetox_details[assayCategory == 'in vitro', .N, by = .(assayType)]
#>                     assayType     N
#>                        <char> <int>
#>  1: Mitotic recombination or     20
#>  2: Histidine reverse gene mu    19
#>  3:                 InVitroCA    31
#>  4:                 InVitroMN     6
#>  5:                InVitroMLA    24
#>  6:                      Ames    82
#>  7:                  InVivoMN    28
#>  8: Sister-chromatid exchange    42
#>  9: Micronucleus test, chromo     8
#> 10: Cell transformation, clon     8
#> 11: Forward and reverse gene      7
#> 12: Rec-assay, spot test, DNA     2
#> 13: bacterial reverse mutatio   108
#> 14: Rec-assay, DNA effects (b    15
#> 15: mammalian cell gene mutat    36
#> 16: in vitro mammalian chromo    21
#> 17: DNA damage and repair ass    15
#> 18: Sex-linked recessive leth     9
#> 19: Sister-chromatid exchange    14
#> 20: Forward gene mutation at      6
#> 21:    Chromosome aberrations     2
#> 22:     Reverse gene mutation     9
#> 23: Heritable translocation t     5
#> 24: chromosome aberration ass    14
#> 25: sister chromatid exchange    10
#> 26: bacterial forward mutatio     1
#> 27:     Bacterial Mutagenesis    35
#> 28:     In Vitro Micronucleus     3
#> 29:        Cytogenetics Other    31
#> 30: bacterial gene mutation a     6
#> 31: in vitro mammalian cell m     5
#> 32: Aneuploidy, chromosome ab     5
#> 33: Cell transformation, vira    12
#> 34: sister chromatid exchange     3
#> 35:     Forward gene mutation     5
#> 36: Chromosome aberrations in     2
#> 37: Forward gene mutation at      6
#> 38: Specific locus test, gene     1
#> 39: Forward gene mutation at      2
#> 40:  Spot test, gene mutation     1
#> 41: In Vivo Non-mammalian Mut     7
#> 42:           mouse spot test     2
#> 43: transgenic rodent mutagen     1
#> 44:   yeast cytogenetic assay     1
#> 45: in vitro mammalian cell t     2
#> 46:       Cell transformation     5
#> 47: Tryptophan reverse gene m     8
#> 48:             Gene mutation     2
#> 49:       Cell Transformation     2
#> 50:         DNA Damage/Repair     9
#> 51: In Vitro Chromosome Aberr    10
#> 52:                  Mutation     4
#> 53: in vitro chromosomal aber     1
#> 54:            Mutation Other     4
#> 55: In vitro mammalian chromo     2
#> 56: Forward and reverse gene      2
#> 57: Forward and reverse gene      1
#> 58: Chromosomal aberration as     2
#> 59:     Mitotic recombination     1
#> 60: Aneuploidy, sex chromosom     1
#> 61: Aneuploidy, whole sex chr     1
#> 62:          fluctuation test     1
#>                     assayType     N
#>                        <char> <int>
ccl4_genetox_details[assayCategory == 'ND', .N, by = .(assayType)]
#>     assayType     N
#>        <char> <int>
#> 1:    Overall     5
#> 2: transgenic    10
ccl4_genetox_details[assayCategory == 'in vivo', .N, by = .(assayType)]
#>                     assayType     N
#>                        <char> <int>
#>  1: Unscheduled DNA synthesis     9
#>  2: In vivo carcinogenicity s    23
#>  3:        micronucleus assay    45
#>  4: in vivo micronucleus (mou    19
#>  5:          Sperm morphology     9
#>  6: in vivo micronucleus (rat     9
#>  7: Unscheduled DNA synthesis     3
#>  8:      Dominant lethal test     5
#>  9:                  InVivoCA    14
#> 10:                 InVivoUDS    11
#> 11: mammalian germ cell cytog     1
#> 12: unscheduled DNA synthesis     6
#> 13:               DNA Binding     1
#> 14: rodent dominant lethal as    16
#> 15: Chromosome aberrations in     2
#> 16:           InVivoDNADamage     7
#> 17:      In Vivo Micronucleus     1
#> 18: Micronucleus and sister c     2
#> 19:       in vivo comet (rat)     3
#> 20:     in vivo comet (mouse)     1
#> 21: In Vivo Mammalian Mutagen     7
#> 22:      DNA Covalent Binding    16
#> 23: In Vivo Chromosome Aberra     1
#>                     assayType     N
#>                        <char> <int>

natadb_genetox_details[, .N, by = .(assayCategory, assayType, assayResult)]
#>      assayCategory                 assayType assayResult     N
#>             <char>                    <char>      <char> <int>
#>   1:       in vivo In vivo carcinogenicity s    positive    62
#>   2:      in vitro Micronucleus test, chromo    negative     5
#>   3:      in vitro Rec-assay, spot test, DNA    positive     4
#>   4:      in vitro Rec-assay, DNA effects (b    positive    28
#>   5:      in vitro bacterial reverse mutatio    positive    77
#>  ---                                                          
#> 191:      in vitro Heritable translocation t    negative     2
#> 192:       in vivo     mammalian comet assay   equivocal     1
#> 193:      in vitro mammalian cell gene mutat    positive     1
#> 194:      in vitro     Mitotic recombination    positive     1
#> 195:      in vitro in vitro mammalian cell t    positive     1
natadb_genetox_details[, .N, by = .(assayCategory)]
#>    assayCategory     N
#>           <char> <int>
#> 1:       in vivo   516
#> 2:      in vitro  1995
#> 3:            ND    34
natadb_genetox_details[assayCategory == 'in vitro', .N, by = .(assayType)]
#>                     assayType     N
#>                        <char> <int>
#>  1: Micronucleus test, chromo    33
#>  2: Rec-assay, spot test, DNA     6
#>  3: Rec-assay, DNA effects (b    34
#>  4: bacterial reverse mutatio   303
#>  5: Mitotic recombination or     62
#>  6: Histidine reverse gene mu    55
#>  7: Sister-chromatid exchange    99
#>  8:                      Ames   238
#>  9:                InVitroMLA    85
#> 10:                 InVitroMN    20
#> 11:                 InVitroCA    98
#> 12:                  InVivoMN    89
#> 13: Cell transformation, clon    14
#> 14: Forward and reverse gene     17
#> 15: DNA damage and repair ass    40
#> 16:    Chromosome aberrations     3
#> 17: Sister-chromatid exchange    31
#> 18: Heritable translocation t    10
#> 19: Sex-linked recessive leth    26
#> 20:     Reverse gene mutation    30
#> 21: Forward gene mutation at     13
#> 22: Cell transformation, vira    46
#> 23:         DNA Damage/Repair    34
#> 24:     Bacterial Mutagenesis    49
#> 25: in vitro mammalian chromo    86
#> 26:        Cytogenetics Other    48
#> 27:                  Mutation     7
#> 28: In Vivo Non-mammalian Mut     7
#> 29:     In Vitro Micronucleus     7
#> 30: In Vitro Chromosome Aberr     7
#> 31: mammalian cell gene mutat   102
#> 32:             Gene mutation     8
#> 33:  Spot test, gene mutation     4
#> 34: Forward and reverse gene      4
#> 35: sister chromatid exchange    48
#> 36: bacterial forward mutatio     4
#> 37: bacteriophage induction i     1
#> 38: DNA damage, gene mutation     1
#> 39: Forward gene mutation at      6
#> 40: chromosome aberration stu     1
#> 41: in vitro mammalian cell t     2
#> 42: Forward gene mutation at     10
#> 43:       Cell transformation    11
#> 44: Forward and reverse gene      7
#> 45:     Drosophila SLRL assay    20
#> 46: chromosome aberration ass    27
#> 47: Salmonella and Escherichi     1
#> 48: Cytogenetic assay in bone     1
#> 49:     Forward gene mutation    18
#> 50: Chromosome aberrations in     7
#> 51: in vitro mammalian cell g     2
#> 52:           mouse spot test     8
#> 53: Aneuploidy, whole sex chr     4
#> 54: sister chromatid exchange     7
#> 55: Mouse Lymphoma Forward Mu     1
#> 56: Tryptophan reverse gene m    18
#> 57: bacterial gene mutation a    10
#> 58: Micronucleus test in vitr     2
#> 59: yeast forward mutation an     2
#> 60: mitotic recombination ass     6
#> 61: Aneuploidy, chromosome ab     8
#> 62: in vitro mammalian cell m    13
#> 63:       cell transformation     2
#> 64: somatic mutation and reco     3
#> 65: transgenic rodent mutagen     2
#> 66:   yeast cytogenetic assay     2
#> 67: in vitro mammalian cell g     2
#> 68:        bacterial mutation     1
#> 69: bacterial reverse mutatio     1
#> 70: Aneuploidy, partial sex c     2
#> 71: in vitro chromosome aberr     1
#> 72: Cell transformation, focu     2
#> 73: Forward and reverse gene      1
#> 74: gene mutation assay in fu     6
#> 75:       Cell Transformation     1
#> 76: Aneuploidy, sex chromosom     1
#> 77: Forward and reverse gene      1
#> 78: E. coli K-12 DNA repair h     1
#> 79: Chromosomal aberration as     2
#> 80:          forward mutation     1
#> 81: mammalian cell gene mutat     1
#> 82:     Mitotic recombination     1
#>                     assayType     N
#>                        <char> <int>
natadb_genetox_details[assayCategory == 'ND', .N, by = .(assayType)]
#>     assayType     N
#>        <char> <int>
#> 1:    Overall    16
#> 2: transgenic    18
natadb_genetox_details[assayCategory == 'in vivo', .N, by = .(assayType)]
#>                     assayType     N
#>                        <char> <int>
#>  1: In vivo carcinogenicity s    66
#>  2: Unscheduled DNA synthesis    27
#>  3:        micronucleus assay   109
#>  4: rodent dominant lethal as    33
#>  5:                 InVivoUDS    33
#>  6:                  InVivoCA    37
#>  7: Unscheduled DNA synthesis     5
#>  8:      Dominant lethal test    14
#>  9:           InVivoDNADamage    23
#> 10:      In Vivo Micronucleus    13
#> 11: In Vivo Mammalian Mutagen     8
#> 12: In Vivo Chromosome Aberra     6
#> 13: in vivo micronucleus (mou    51
#> 14:          Sperm morphology    25
#> 15:               DNA Binding     1
#> 16: unscheduled DNA synthesis    19
#> 17: Chromosome aberrations in     9
#> 18:     in vivo comet (mouse)     4
#> 19: in vivo micronucleus (rat     9
#> 20:       in vivo comet (rat)     3
#> 21: mammalian erythrocyte mic     2
#> 22: heritable translocation a     2
#> 23: Micronucleus and sister c     2
#> 24: bone marrow chromosome ab     2
#> 25: Chromosome aberrations, i     2
#> 26:      DNA adduct formation     1
#> 27:      DNA Covalent Binding     2
#> 28:     mammalian comet assay     7
#> 29: mammalian germ cell cytog     1
#>                     assayType     N
#>                        <char> <int>
```

Observe that there are 87 unique assays for CCl4 and 113 unique assays
for NATADB. The different assay categories are “in vitro”, “ND”, and “in
vivo”, with 62 unique “in vitro” assays for CCl4 and 82 for NATADB, 2
unique “ND” assays for CCL4 and 2 for NATADB, and 23 unique “in vivo”
assays for CCL4 and 29 for NATADB.

One may be interested in looking at the number of chemicals for which an
assay resulted in a positive or negative result. To assess this, group
by `assayResult` and determine the number of unique `dtxsid` values
associated with each `assayResult` value.

``` r

ccl4_genetox_details[, .(DTXSIDs = length(unique(dtxsid))), by = .(assayResult)]
#>    assayResult DTXSIDs
#>         <char>   <int>
#> 1:    positive      53
#> 2:    negative      63
#> 3:   equivocal      14
natadb_genetox_details[, .(DTXSIDs = length(unique(dtxsid))), 
                       by = .(assayResult)]
#>    assayResult DTXSIDs
#>         <char>   <int>
#> 1:    positive     129
#> 2:    negative     139
#> 3:   equivocal      47
```

For CCL4, there are 63 unique chemicals that have a negative assay
result, 53 that have a positive result, and 14 that have an equivocal
result. For NATADB, there are 139 unique chemicals that have a negative
assay result, 129 that have a positive result, and 47 that have an
equivocal result. Observe that since there are 71 unique `dtxsid` values
with assay results in CCL4 and 153 in NATADB, there are several
chemicals that have multiple assay results.

Next, determine the chemicals from each chemical list that are known to
have genotoxic effects. For this, examine which chemicals produce at
least one positive response in the `assayResult` column.

``` r

ccl4_genetox_details[, .(is_positive = any(assayResult == 'positive')), 
                     by = .(dtxsid)][is_positive == TRUE, dtxsid]
#>  [1] "DTXSID0020153" "DTXSID0020573" "DTXSID0020600" "DTXSID0020814" "DTXSID0021464" "DTXSID0021541"
#>  [7] "DTXSID0024341" "DTXSID1021407" "DTXSID1021740" "DTXSID1021798" "DTXSID1024338" "DTXSID1026164"
#> [13] "DTXSID1031040" "DTXSID2021028" "DTXSID2021317" "DTXSID2021731" "DTXSID3020203" "DTXSID3020702"
#> [19] "DTXSID3020833" "DTXSID3024869" "DTXSID3031864" "DTXSID4020533" "DTXSID4021503" "DTXSID4022361"
#> [25] "DTXSID4022367" "DTXSID5020023" "DTXSID5020576" "DTXSID5020601" "DTXSID5021207" "DTXSID5024182"
#> [31] "DTXSID5039224" "DTXSID6020301" "DTXSID6021030" "DTXSID6021032" "DTXSID6022422" "DTXSID7020005"
#> [37] "DTXSID7020215" "DTXSID7020637" "DTXSID7021029" "DTXSID8020044" "DTXSID8020090" "DTXSID8020832"
#> [43] "DTXSID8021062" "DTXSID8023846" "DTXSID8023848" "DTXSID8025541" "DTXSID8031865" "DTXSID9020243"
#> [49] "DTXSID9021390" "DTXSID9021427" "DTXSID9022366" "DTXSID9023380" "DTXSID9023914"
natadb_genetox_details[, .(is_positive = any(assayResult == 'positive')),
                       by = .(dtxsid)][is_positive == TRUE, dtxsid]
#>   [1] "DTXSID0020153" "DTXSID0020448" "DTXSID0020529" "DTXSID0020600" "DTXSID0020868"
#>   [6] "DTXSID0021381" "DTXSID0021383" "DTXSID0021541" "DTXSID0021834" "DTXSID0021965"
#>  [11] "DTXSID0024187" "DTXSID0039227" "DTXSID0039229" "DTXSID1020148" "DTXSID1020302"
#>  [16] "DTXSID1020306" "DTXSID1020431" "DTXSID1020512" "DTXSID1020516" "DTXSID1020566"
#>  [21] "DTXSID1021374" "DTXSID1021798" "DTXSID1021827" "DTXSID1022057" "DTXSID1023786"
#>  [26] "DTXSID1024045" "DTXSID1026164" "DTXSID1049641" "DTXSID2020137" "DTXSID2020262"
#>  [31] "DTXSID2020507" "DTXSID2020682" "DTXSID2020844" "DTXSID2021284" "DTXSID2021286"
#>  [36] "DTXSID2021319" "DTXSID2021658" "DTXSID2021731" "DTXSID2021781" "DTXSID3020203"
#>  [41] "DTXSID3020257" "DTXSID3020413" "DTXSID3020415" "DTXSID3020596" "DTXSID3020679"
#>  [46] "DTXSID3020702" "DTXSID3020833" "DTXSID3021431" "DTXSID3025091" "DTXSID3039242"
#>  [51] "DTXSID4020161" "DTXSID4020298" "DTXSID4020402" "DTXSID4020533" "DTXSID4020583"
#>  [56] "DTXSID4020874" "DTXSID4020901" "DTXSID4021006" "DTXSID4021056" "DTXSID4021395"
#>  [61] "DTXSID4039231" "DTXSID5020023" "DTXSID5020027" "DTXSID5020029" "DTXSID5020071"
#>  [66] "DTXSID5020316" "DTXSID5020449" "DTXSID5020491" "DTXSID5020601" "DTXSID5020607"
#>  [71] "DTXSID5020865" "DTXSID5021124" "DTXSID5021207" "DTXSID5021380" "DTXSID5021386"
#>  [76] "DTXSID5024055" "DTXSID5024059" "DTXSID5039224" "DTXSID6020145" "DTXSID6020307"
#>  [81] "DTXSID6020353" "DTXSID6020432" "DTXSID6020438" "DTXSID6020515" "DTXSID6020569"
#>  [86] "DTXSID6020981" "DTXSID6021828" "DTXSID6022422" "DTXSID6023947" "DTXSID6023949"
#>  [91] "DTXSID7020005" "DTXSID7020009" "DTXSID7020267" "DTXSID7020637" "DTXSID7020689"
#>  [96] "DTXSID7020710" "DTXSID7020716" "DTXSID7021029" "DTXSID7021100" "DTXSID7021106"
#> [101] "DTXSID7021318" "DTXSID7021360" "DTXSID7021368" "DTXSID7021948" "DTXSID7024166"
#> [106] "DTXSID7024370" "DTXSID7024532" "DTXSID7025180" "DTXSID7026156" "DTXSID8020090"
#> [111] "DTXSID8020173" "DTXSID8020250" "DTXSID8020599" "DTXSID8020759" "DTXSID8020832"
#> [116] "DTXSID8021195" "DTXSID8021197" "DTXSID8021432" "DTXSID8021434" "DTXSID8021438"
#> [121] "DTXSID8024286" "DTXSID9020168" "DTXSID9020243" "DTXSID9020247" "DTXSID9020293"
#> [126] "DTXSID9020827" "DTXSID9021138" "DTXSID9021261" "DTXSID9041522"
```

Given the amount of genotoxicity data, consider one chemical,
DTXSID0020153, to get a sense of the assays, the number of each type of
result, and which correspond to “positive” results. To determine this,
group by `assayResult` and calculate `.N` for each group. We also
isolate which were positive and output a data.table with the number of
each type.

``` r

ccl4_genetox_details[dtxsid == 'DTXSID0020153', .(Number = .N), 
                     by = .(assayResult)]
#>    assayResult Number
#>         <char>  <int>
#> 1:    positive     20
#> 2:    negative      5
#> 3:   equivocal      1
ccl4_genetox_details[dtxsid == 'DTXSID0020153' & assayResult == 'positive', 
                     .(Number_of_assays = .N), by = .(assayType)][order(-Number_of_assays),]
#>                     assayType Number_of_assays
#>                        <char>            <int>
#>  1:                 InVitroCA                2
#>  2:                InVitroMLA                2
#>  3:                      Ames                2
#>  4: Sister-chromatid exchange                2
#>  5: bacterial reverse mutatio                2
#>  6: Rec-assay, DNA effects (b                2
#>  7: Unscheduled DNA synthesis                1
#>  8: Mitotic recombination or                 1
#>  9: In vivo carcinogenicity s                1
#> 10: Histidine reverse gene mu                1
#> 11:                 InVitroMN                1
#> 12:                   Overall                1
#> 13: Cell transformation, clon                1
#> 14: Rec-assay, spot test, DNA                1
```

There were five assays that produced a negative result, 20 that produced
a positive result, and one that produced an equivocal result. Of the 22
positive assays, “bacterial reverse mutation assay” and “Ames” are among
those that are most numerous, with three each.

## Conclusion

In this vignette, a variety of functions that access different types of
data found in the `Hazard` endpoints of the CTX APIs were explored.
While this exploration was not exhaustive, it provides a basic
introduction to how one may access data and work with it. Additional
endpoints and corresponding functions exist and we encourage the user to
explore these while keeping in mind the examples contained in this
vignette.
