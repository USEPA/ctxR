# ctxR: Exposure API

### Introduction

In this vignette, the [CTX Exposure
API](https://comptox.epa.gov/ctx-api/docs/exposure.html) will be
explored.

[![](Pictures/ctxR_hex.png)](https://CRAN.R-project.org/package=ctxR)

Data provided by the Exposure API are broadly organized in five
different areas: Functional Use, Product Data, List Presence, High
Throughput Toxicokinetic (HTTK) parameters, and Exposure estimates.

- Data from the Functional Use, Product Data, and List Presence
  resources (aside from the Functional Use Probability endpoint) are
  developed from publicly available documents and are also accessible
  using the Chemical Exposure Knowledgebase
  ([ChempExpo](https://comptox.epa.gov/chemexpo/)) interactive web
  application developed by the United States Environmental Protection
  Agency. The underlying database for the Functional Use, Product Data,
  and List Presence endpoints of the Exposure API and ChemExpo is the
  Chemicals and Products Database (CPDat). CPDat provides reported
  information on how chemicals are used in commerce and (where possible)
  at what quantities they occur in consumer and industrial products; see
  [(Dionisio et al. 2018)](https://www.nature.com/articles/sdata2018125)
  for more information on CPDat.

- The data provided by the Functional Use Probability endpoint are
  predictions from EPA’s Quantitative Structure Use Relationship (QSUR)
  models [(Phillips et
  al. 2017)](https://pubs.rsc.org/en/content/articlelanding/2017/gc/c6gc02744j).

- HTTK data are represented by predictions from the
  [`httk`](https://CRAN.R-project.org/package=httk) R package,
  introduced in [(Pearce, R. et
  al. 2017)](https://doi.org/10.18637%2Fjss.v079.i04). These data are
  particularly relevant for examining *in vitro* to *in vitro*
  extrapolation (IVIVE).

- Exposure estimates are provided via several exposure models, including
  the SEEM models. Information on the SEEM2 model can be found at
  [(Wambaugh, J. et al. 2014)](http://dx.doi.org/10.1021/es503583j) and
  on the SEEM3 model can be found at [(Ring, C. et
  al. 2018)](http://dx.doi.org/10.1021/acs.est.8b04056)

Product Data are organized by harmonized Product Use Categories (PUCs).
The PUCs are assigned to products (which are associated with Composition
Documents) and indicate the type of product associated to each data
record. They are organized hierarchically, with General Category
containing Product Family, which in turn contains Product Type. The
Exposure API also provide information on how the PUC was assigned. Do
note that a natural language processing model is used to assign PUCs
with the “classificationmethod” equal to “Automatic”. As such, these
assignments are less certain and may contain inaccuracies. More
information on PUC categories can be found in [(Isaacs et
al. 2020)](https://doi.org/10.1038/s41370-019-0187-5). The associated
endpoints are organized within the [Product Data
Resource](#product-data-resource).

List Presence Data reflect the occurrence of chemicals on lists present
in publicly available documents (sourced from a variety of federal and
state agencies and trade associations). These lists are tagged with List
Presence Keywords (LPKs) that together describe information contained in
the document relevant to how the chemical was used. LPKs are an updated
version of the cassettes provided in the Chemical and Product Categories
(CPCat) database; see [(Dionisio et
al. 2015)](https://doi.org/10.1016/j.toxrep.2014.12.009). For the most
up to date information on the current LPKs and to see how the CPCat
cassettes were updated, see [(Koval et
al. 2022)](https://www.nature.com/articles/s41370-022-00451-8). The
associated endpoints are organized within the [List Presence
Resource](#list-presence-resource).

Both reported and predicted Function Use Information is available.
Reported functional use information is organized by harmonized Function
Categories (FCs) that describe the role a chemical serves in a product
or industrial process. The harmonized technical function categories and
definitions were developed by the Organisation for Economic Co-operation
and Development (OECD) (with the exception of a few categories unique to
consumer products which are noted as being developed by EPA). These
categories have been augmented with additional categories needed to
describe chemicals in personal care, pharmaceutical, or other commercial
sectors. The reported function data form the basis for ORD’s QSUR models
[(Phillips et
al. 2016)](https://pubs.rsc.org/en/content/articlelanding/2017/GC/C6GC02744J).
These models provide the structure-based predictions of chemical
function available in the Functional Use Probability endpoint. Note that
these models were developed prior to the OECD function categories, so
their function categories are not yet aligned with the harmonized
categories used in the reported data. Updated models for the harmonized
categories are under development. The associated endpoints are organized
within the [Functional Use Resource](#functional-use-resource).

The R package `httk` provides users with a variety of tools to
incorporate toxicokinetics and IVIVE into bioinformatics and comes with
pre-made models that can be used with specific chemical data. The `httk`
endpoint is found within the [`httk` Data
Resource](#httk-data-resource).

The SEEM models were developed to provide predictions for potential
human exposure to chemicals with little or no exposure data. For SEEM2,
Bayesian methods were used to infer ranges of exposure consistent with
data from the National Health and Nutrition Examination Survey.
Predictions for different demographic groups were made. For SEEM3,
chemical exposures through four different pathways were predicted and in
turn weighting of different models through these exposure pathways was
conducted to produce consensus predictions. The exposure prediction
endpoints are organized within [Exposure
Predictions](#exposure-predictions).

Information for ChemExpo is sourced from: Sakshi Handa, Katherine A.
Phillips, Kenta Baron-Furuyama, and Kristin K. Isaacs. 2023. “ChemExpo
Knowledgebase User Guide”.
<https://comptox.epa.gov/chemexpo/static/user_guide/index.html>.

**NOTE:** Please see the introductory vignette for an overview of the
*ctxR* package and initial set up instruction with API key storage.

Several ctxR functions can be used to access the CTX Exposure API data,
as described in the following sections. Tables output in each example
have been filtered to only display the first few rows of data.

## Functional Use Resource

Functional uses for chemicals may be searched.

### Functional Use

[`get_exposure_functional_use()`](https://usepa.github.io/ctxR/reference/get_exposure_functional_use.md)
retrieves FCs and associated metadata for a specific chemical (by
DTXSID).

``` r

exp_fun_use <- get_exposure_functional_use(DTXSID = 'DTXSID7020182')
```

| id | dtxsid | datatype | docid | doctitle | docdate | reportedfunction | functioncategory |
|---:|:---|:---|---:|:---|:---|:---|:---|
| 221654 | DTXSID7020182 | Function | 1631119 | 4 |  | phenolics | NA |
| 221655 | DTXSID7020182 | Function | 1630343 | Octolite 485 |  | antioxidants\>phenolics | Antioxidant |
| 221656 | DTXSID7020182 | Chemical presence list | 1371471 | The 25 Chemicals Found in All Nine of the Biosolids Studied |  | fire retardant | Flame retardant |
| 221657 | DTXSID7020182 | Function | 1514560 | 4,4’-isopropylidenediphenol |  | not reported | No specific technical function |
| 221658 | DTXSID7020182 | Composition | 1550827 | Thin-Set_Epoxy_Terrazzo_Flooring-Master_Terrazzo_Technologies-2017-02-02 | february 2, 2017 | epoxy hardener | NA |
| 221659 | DTXSID7020182 | Composition | 1550827 | Thin-Set_Epoxy_Terrazzo_Flooring-Master_Terrazzo_Technologies-2017-02-02 | february 2, 2017 | curing agent | Hardener |

### Functional Use Probability

[`get_exposure_functional_use_probability()`](https://usepa.github.io/ctxR/reference/get_exposure_functional_use_probability.md)
retrieves the probability of functional use within different FCs for a
given chemical (by DTXSID). Each value represents the probability of the
chemical being classified as having this function, as predicted by the
QSUR models.

``` r

exp_fun_use_prob <- get_exposure_functional_use_probability(DTXSID = 'DTXSID7020182')
```

| dtxsid        | harmonizedFunctionalUse | probability |
|:--------------|:------------------------|------------:|
| DTXSID7020182 | antimicrobial           |      0.3722 |
| DTXSID7020182 | antioxidant             |      0.8941 |
| DTXSID7020182 | catalyst                |      0.2031 |
| DTXSID7020182 | colorant                |      0.1560 |
| DTXSID7020182 | crosslinker             |      0.7743 |
| DTXSID7020182 | flame_retardant         |      0.2208 |

#### Functional Use Probability Batch

We demonstrate how the individual results differ from the batch results
when retrieving functional use probabilities via
[`get_exposure_functional_use_probability_batch()`](https://usepa.github.io/ctxR/reference/get_exposure_functional_use_probability_batch.md).

``` r

bpa_prob <- get_exposure_functional_use_probability(DTXSID = 'DTXSID7020182')
caf_prob <- get_exposure_functional_use_probability(DTXSID = 'DTXSID0020232')

bpa_caf_prob <- get_exposure_functional_use_probability_batch(DTXSID = c('DTXSID7020182', 'DTXSID0020232'))
```

    #>           dtxsid harmonizedFunctionalUse probability
    #> 1  DTXSID7020182           antimicrobial      0.3722
    #> 2  DTXSID7020182             antioxidant      0.8941
    #> 3  DTXSID7020182                catalyst      0.2031
    #> 4  DTXSID7020182                colorant      0.1560
    #> 5  DTXSID7020182             crosslinker      0.7743
    #> 6  DTXSID7020182         flame_retardant      0.2208
    #> 7  DTXSID7020182               flavorant      0.0314
    #> 8  DTXSID7020182               fragrance      0.2071
    #> 9  DTXSID7020182         heat_stabilizer      0.5119
    #> 10 DTXSID7020182        skin_conditioner      0.1168
    #> 11 DTXSID7020182         skin_protectant      0.3306
    #> 12 DTXSID7020182             uv_absorber      0.8046

    #>          dtxsid harmonizedFunctionalUse probability
    #> 1 DTXSID0020232           antimicrobial      0.4808
    #> 2 DTXSID0020232                  buffer      0.6370
    #> 3 DTXSID0020232                colorant      0.3962
    #> 4 DTXSID0020232        skin_conditioner      0.9821

    #>           DTXSID antimicrobial antioxidant catalyst colorant crosslinker
    #>           <char>         <num>       <num>    <num>    <num>       <num>
    #> 1: DTXSID7020182        0.3722      0.8941   0.2031   0.1560      0.7743
    #> 2: DTXSID0020232        0.4808          NA       NA   0.3962          NA
    #> 8 variables not shown: [flame_retardant <num>, flavorant <num>, fragrance <num>, heat_stabilizer <num>, skin_conditioner <num>, skin_protectant <num>, uv_absorber <num>, buffer <num>]

Observe that Caffeine only has probabilities assigned to four functional
use categories while Bisphenol A has probabilities assigned to twelve
categories. For single chemical search, functional use categories denote
the row. However, when using the batch search function, all reported
categories are included as columns, with rows corresponding to each
chemical. If a chemical does not have a probability associated to a
functional use, the corresponding entry is given by an NA.

### Functional Use Categories

`get_exposure_functional_use_categories()` retrieves definitions of all
the available FCs. This is not specific to a chemical, but rather a list
of all FCs.

``` r

exp_fun_use_cat <- get_exposure_functional_use_category()
```

| id | category | definition |
|---:|:---|:---|
| 36 | Deflocculant | Chemical substance used to fluidize concentrated slurries to reduce their bulk viscosity or stickiness in processing or handling. See closely related: anti-caking agent. |
| 37 | Defoamer | Chemical substance that controls foam; prevents foam from forming; breaks down any foam that does form; and reduces foaming from proteins, gases, or nitrogenous materials. Reduces the tendency of finished products to generate foam upon shaking or agitation. The ability of a material to act as antifoam depends on its tendency to concentrate on the surface of existing or forming bubbles and to disrupt the continuous films of liquid surrounding them. Used as a process aid to improve filtration, dewatering, washing, and drainage of many types of suspensions, mixtures, and slurries. Also referred to as an antifoaming agent. |
| 38 | Degradant/impurity (EPA) | Chemical substance reported as a degradant, impurity, contaminant, etc. |
| 39 | Dehydrating agent (desiccant) | Chemical substance used to absorb and remove water from gases or liquids to induce or maintain a state of dryness. Substances are usually hygroscopic materials. See closely related: humectant; adsorbent. |
| 40 | Demulsifier | Chemical substance used to destroy an emulsion or prevent its formation. |
| 41 | Density modifier | Chemical substance that modifies the density of a material. Also referred to as density modifying agents. See closely related: viscosity modifier; thickening agent. |

## Product Data Resource

There are a few resources for retrieving product use data associated
with chemical identifiers (DTXSID) or general use.

### Product Data

[`get_exposure_product_data()`](https://usepa.github.io/ctxR/reference/get_exposure_product_data.md)
retrieves the product data (PUCs and related data) for products that use
the specified chemical (by DTXSID).

``` r

exp_prod_dat <- get_exposure_product_data(DTXSID = 'DTXSID7020182')
```

| id | dtxsid | docid | doctitle | docdate | productname | gencat | prodfam | prodtype | classificationmethod | rawmincomp | rawmaxcomp | rawcentralcomp | unittype | lowerweightfraction | upperweightfraction | centralweightfraction | weightfractiontype | component |
|---:|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|:---|:---|
| 10934 | DTXSID7020182 | 1098930 | EPOLITE 2152 HARDENER | 03/09/1992 | epolite 2152 hardener | Raw materials | adhesives |  | Manual Batch | 30 | 45 |  | percent | 0.30 | 0.45 | NA | reported |  |
| 12328 | DTXSID7020182 | 1099279 | HYSOL KS4008 PART B KB4008, KS4008 | 02/12/1992 | hysol ks4008 part b kb4008\_ ks4008 | Specialty occupational products | aviation |  | Manual Batch | 1 | 15 |  | percent | 0.01 | 0.15 | NA | reported |  |
| 169768 | DTXSID7020182 | 1153212 | PRIMER COATING, EPOXY PART B 09 ID724116 | 08/16/1988 | primer coating\_ epoxy part b 09 id724116 | Home maintenance | paint/stain and related products |  | Manual Batch |  |  |  | NA | NA | NA | NA | reported |  |
| 18357 | DTXSID7020182 | 1100923 | SCOTCH-WELD EC-3578 B/A PART A STRUCTURAL (SUPDAT) | 08/12/1993 | scotch-weld ec-3578 b/a part a structural (supdat) | Home maintenance | adhesives and adhesive removers | multipurpose adhesive | Manual Batch | 1 | 10 |  | percent | 0.01 | 0.10 | NA | reported |  |
| 174406 | DTXSID7020182 | 1154642 | RP 1250 HARDENER | 12/07/1989 | rp 1250 hardener | Raw materials | coatings |  | Manual Batch |  |  | 1.665 | percent | NA | NA | 0.01665 | reported |  |
| 22439 | DTXSID7020182 | 1102189 | CHEMLOK 805 ELASTOMER ADHESIVE, 98050W | 01/28/1993 | chemlok 805 elastomer adhesive\_ 98050w | Laboratory supplies |  |  | Manual Batch |  |  |  | NA | NA | NA | NA | reported |  |

### Product Use Category Data

[`get_exposure_product_data_puc()`](https://usepa.github.io/ctxR/reference/get_exposure_product_data_puc.md)
retrieves the definitions of all the PUCs. This is not specific to a
chemical, but rather a list of all PUCs.

``` r

exp_prod_data_puc <- get_exposure_product_data_puc()
```

| id | kindName | genCat | prodfam | prodtype | definition |
|---:|:---|:---|:---|:---|:---|
| 254 | Formulation | Personal care | sexual wellness |  | sexual wellness products, including personal lubricants |
| 255 | Formulation | Personal care | shaving and hair removal |  | shaving or hair removal products which do not fit into a more refined category |
| 256 | Formulation | Personal care | shaving and hair removal | aftershave | products applied to the skin following shaving to provide scent, or improve skin characteristics |
| 257 | Formulation | Personal care | shaving and hair removal | clipper lubricant/cleaner | cleaning and lubricating products for hair clippers |
| 258 | Formulation | Personal care | shaving and hair removal | depilatory | products for removal of body or facial hair |
| 259 | Formulation | Personal care | shaving and hair removal | shaving cream | shaving creams, foams, balms and soaps |

## `httk` Data Resource

Predictions from the `httk` R package are available.

### `httk` Data

There is a single resource that returns `httk` model data when
available.

``` r

bpa_httk <- get_httk_data(DTXSID = 'DTXSID7020182')
head(data.table(bpa_httk))
#>        id        dtxsid    parameter measuredText measured predictedText
#>     <int>        <char>       <char>       <char>    <num>        <char>
#> 1: 101171 DTXSID7020182          Css       0.0083   0.0083         1.114
#> 2: 101172 DTXSID7020182          Css       0.0083   0.0083        0.5297
#> 3: 101173 DTXSID7020182          Css       0.0083   0.0083         1.076
#> 4: 101174 DTXSID7020182          Css       0.0083   0.0083        0.5116
#> 5: 101175 DTXSID7020182 TK.Half.Life         0.19   0.1900         139.5
#> 6: 101176 DTXSID7020182     Days.Css           NA       NA           112
#> 7 variables not shown: [predicted <num>, units <char>, model <char>, reference <char>, percentile <char>, species <char>, dataSourceSpecies <char>]
```

## List Presence Resource

There are a few resources for retrieving list data for specific
chemicals (by DTXSID) or general list presence information.

### List Presence Tags

[`get_exposure_list_presence_tags()`](https://usepa.github.io/ctxR/reference/get_exposure_list_presence_tags.md)
retrieves all the list presence keywords. This is not specific to a
chemical, but rather a list of the the list presence keywords. Note that
some List Presence Keywords align with PUCs, but the keywords are
assigned to documents that refer to product category as a whole, while
PUCs are assigned to documents referring to specific products (e.g.,
ingredient list).

``` r

exp_list_tags <- get_exposure_list_presence_tags()
```

| id | tagName | tagDefinition | tagKind |
|---:|:---|:---|:---|
| 46 | Cleaning products and household care - shoes | Products related to the care of footwear which do not fit into a more refined category | PUC - formulation |
| 47 | Cleaning products and household care - shoes - shoe polish or protectant | products applied to footwear to color, polish, clean, or add a protective surface | PUC - formulation |
| 48 | Cons electronics mech appliances and machinery | Including small and large consumer electronics appliances (e.g. refrigerator, washing machine, vacuum cleaner, computer, phone, smoke detector, electric tools, lamps); includes small electronics with direct personal contact such as massagers; excludes electronics specifically intended for use by children; excludes electronics with direct food contact (e.g. electric blender) | PUC - article |
| 49 | Construction and building materials | Materials used for construction (e.g. flooring, tile, sinks, bathtubs, mirrors, wall materials/drywall, wall-to-wall carpets, insulation, playground surfaces); includes semi-permanent fixtures such as faucets and light fixtures | PUC - article |
| 50 | consumer_product | Indicated as related to, or in, consumer products, but with no more specific information designating product category | General use |
| 51 | cotton | of or related to the growing of cotton, or processed cotton used as a textile for consumer goods | Foods & Agriculture |

### List Presence Tag Data

[`get_exposure_list_presence_tags_by_dtxsid()`](https://usepa.github.io/ctxR/reference/get_exposure_list_presence_tags_by_dtxsid.md)
retrieves LPKs and associated data for a specific chemical (by DTXSID).

``` r

exp_list_tags_dat <- get_exposure_list_presence_tags_by_dtxsid(DTXSID = 'DTXSID7020182')
```

| id | dtxsid | docid | doctitle | docsubtitle | docdate | organization | reportedfunction | functioncategory | component | keywordset |
|---:|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|
| 24597 | DTXSID7020182 | 1371471 | The 25 Chemicals Found in All Nine of the Biosolids Studied | median concentrations are normalized to organic carbon and are given in micorgrams per kilogram of organic carbon (µg/kg OC) |  | USGS | fire retardant | Flame retardant |  | detected; wastewater |
| 31967 | DTXSID7020182 | 1372153 | Emission and evaluation of chemical substances from selected electrical and electronic products- Table 2.10 | Table 2.10 Unwanted substances in electric and electronic equipment according to the UMP system. | 2003 | Danish Environmental Protection Agency | NA | NA |  | Cons electronics mech appliances and machinery; Europe |
| 21358 | DTXSID7020182 | 1370059 | FDA Cumulative Estimated Daily Intake | The database lists publicly available cumulative estimated daily intakes (CEDIs) for a large number of food contact substances |  | FDA | NA | NA |  | CEDI |
| 28659 | DTXSID7020182 | 1372108 | Annex to the HYDROCHECK procedure for acceptance of Materials in contact with Drinking Water | 1.2 Compounds that may be used in the manufacture of plastics, elastomers and natural and synthetic rubber products | 23 March 2010 | Belgaqua | NA | NA |  | drinking_water; Europe; manufacturing; plastic_additive |
| 43751 | DTXSID7020182 | 1372213 | Indirect Additives used in Food Contact Substances | FDA authorizes Indirect Food Additives by identity, intended use, and conditions of use; the presence of a substance in this list indicates that only certain intended uses and use conditions are authorized by FDA regulations | 10/4/2018 | FDA | NA | NA |  | Indirect additives food contact (10/2018) |
| 9996 | DTXSID7020182 | 1359540 | Actively Registered AI’s by Common Name |  |  | California Department of Pesticide Regulation | NA | NA |  | active_ingredient; Pesticides |

## Exposure Predictions

There are two endpoints that provide access to exposure prediction data.
The first provides general information on exposure pathways while the
second provides exposure predictions from a variety of exposure models.
The general information from the first endpoint corresponds to SEEM3
consensus predictions of exposure pathways. The exposure predictions
from the second endpoint feature SEEM2 predictions broken down by
demographic groups, general consensus exposure rate predictions from
SEEM3, and in some cases additional exposure predictions from other
models

### General Exposure Predictions

[`get_general_exposure_prediction()`](https://usepa.github.io/ctxR/reference/get_general_exposure_prediction.md)
returns general exposure information for a given chemical.

``` r

bpa_general_exposure <- get_general_exposure_prediction(DTXSID = 'DTXSID7020182')
head(bpa_general_exposure)
#>           dtxsid productionVolume  units stockholmConvention probabilityDietary
#>           <char>            <int> <char>               <int>              <num>
#> 1: DTXSID7020182          2780000 kg/day                   0                  1
#> 3 variables not shown: [probabilityResidential <num>, probabilityPesticde <num>, probabilityIndustrial <num>]
```

### Demographic Exposure Predictions

[`get_demographic_exposure_prediction()`](https://usepa.github.io/ctxR/reference/get_demographic_exposure_prediction.md)
returns exposure prediction information split across different
demographics for a given chemical.

``` r

bpa_demographic_exposure <- get_demographic_exposure_prediction(DTXSID = 'DTXSID7020182')
head(data.table(bpa_demographic_exposure))
#>        id        dtxsid        demographic       predictor       median
#>     <int>        <char>             <char>          <char>        <num>
#> 1: 488214 DTXSID7020182              Total SEEM3 Consensus 5.497000e-05
#> 2: 772655 DTXSID7020182              Total          RAIDAR 3.770000e+00
#> 3: 768361 DTXSID7020182              Total    Food.Contact 1.766000e-02
#> 4: 697139 DTXSID7020182 Repro. Age Females SEEM2 Heuristic 1.364275e-05
#> 5: 709226 DTXSID7020182              Males SEEM2 Heuristic 3.867956e-05
#> 6: 711258 DTXSID7020182            Females SEEM2 Heuristic 1.244431e-05
#> 8 variables not shown: [medianText <char>, l95 <num>, l95Text <char>, u95 <num>, u95Text <char>, units <char>, ad <int>, reference <char>]
```

## Comptox Chemicals Dashboard (CCD)

There are a variety of endpoints that provide access to data available
from the CCD.

### Product Data

Retrieve the product use categories via
[`get_product_use_category()`](https://usepa.github.io/ctxR/reference/get_product_use_category.md).

``` r

# Caffeine product use categories
caffeine_product_use <- get_product_use_category('DTXSID0020232')
head(data.table(caffeine_product_use))
#>       id        dtxsid                displayPuc     pucKind prodCount
#>    <int>        <char>                    <char>      <char>     <int>
#> 1: 54945 DTXSID0020232 Personal care:acne treatm Formulation         2
#> 2: 55118 DTXSID0020232 Personal care:after sun p Formulation        13
#> 3: 55433 DTXSID0020232 Personal care:eye care an Formulation       130
#> 4: 55438 DTXSID0020232 Personal care:facial clea Formulation       175
#> 5: 56864 DTXSID0020232 Food and drug:pharmaceuti Formulation         2
#> 6: 57096 DTXSID0020232 Personal care:self-tanner Formulation        19
#> 4 variables not shown: [genCat <char>, prodfam <char>, prodtype <char>, definition <char>]
```

Retrieve production volume data via
[`get_production_volume()`](https://usepa.github.io/ctxR/reference/get_production_volume.md).

``` r

# Caffeine production volume
caffeine_prod_vol <- get_production_volume('DTXSID0020232')
data.table(caffeine_prod_vol)
#>       id        dtxsid                      name  amount
#>    <int>        <char>                    <char>  <char>
#> 1:   425 DTXSID0020232 2019 NATIONALLY AGGREGATE 219,582
#> 2:   426 DTXSID0020232 2018 NATIONALLY AGGREGATE 292,600
#> 3:   427 DTXSID0020232 2017 NATIONALLY AGGREGATE 543,400
#> 4:   428 DTXSID0020232 2016 NATIONALLY AGGREGATE 627,000
```

### Biomonitoring data

Retrieve biomonitoring data via
[`get_biomonitoring_data()`](https://usepa.github.io/ctxR/reference/get_biomonitoring_data.md).

``` r

# BPA biomonitoring data
bpa_biom <- get_biomonitoring_data('DTXSID7020182')
head(data.table(bpa_biom))
#>       id        dtxsid   demographic     median upperBound lowerBound
#>    <int>        <char>        <char>      <num>      <num>      <num>
#> 1:   606 DTXSID7020182         Total 1.7293e-05 1.7903e-05 1.6691e-05
#> 2:   607 DTXSID7020182          Male 1.8757e-05 2.1005e-05 1.6888e-05
#> 3:   608 DTXSID7020182        Female 1.6025e-05 1.7202e-05 1.4924e-05
#> 4:   609 DTXSID7020182   3 - 5 years 6.3964e-05 7.9916e-05 5.1602e-05
#> 5:   610 DTXSID7020182  6 - 11 years 2.7678e-05 3.1685e-05 2.4398e-05
#> 6:   611 DTXSID7020182 12 - 19 years 1.6023e-05 1.7608e-05 1.4722e-05
#> 1 variable not shown: [nhanesCohort <char>]
```

### Chemical use and weight fractions

Retrieve general use keywords via
[`get_general_use_keywords()`](https://usepa.github.io/ctxR/reference/get_general_use_keywords.md).

``` r

# BPA general use keywords
bpa_gen_use <- get_general_use_keywords('DTXSID7020182')
head(data.table(bpa_gen_use))
#>       id                keywordset sourceCount        dtxsid
#>    <int>                    <char>       <int>        <char>
#> 1: 40511 OEHHA Proposition 65 (3/2           1 DTXSID7020182
#> 2: 40512 detected, Europe, Toys an           2 DTXSID7020182
#> 3: 40513 Europe, nondetect, Other            3 DTXSID7020182
#> 4: 40514 detected, Europe, Other d           6 DTXSID7020182
#> 5: 40515 artificial_sweat, detecte           2 DTXSID7020182
#> 6: 40516 detected, MN Chemical Scr           8 DTXSID7020182
```

Retrieve functional use via
[`get_reported_functional_use()`](https://usepa.github.io/ctxR/reference/get_reported_functional_use.md).

``` r

# BPA reported functional use
bpa_reported_use <- get_reported_functional_use('DTXSID7020182')
head(data.table(bpa_reported_use))
#>       id        dtxsid        category             definition
#>    <int>        <char>          <char>                 <char>
#> 1: 28665 DTXSID7020182        Hardener               hardener
#> 2:  3071 DTXSID7020182     Antioxidant antioxidants>phenolics
#> 3:  4351 DTXSID7020182          Binder                 binder
#> 4:  5717 DTXSID7020182        Catalyst               catalyst
#> 5:  9261 DTXSID7020182        Hardener           curing agent
#> 6: 14100 DTXSID7020182 Flame retardant         fire retardant
```

Retrieve chemical weight fractions via
[`get_chemical_weight_fraction()`](https://usepa.github.io/ctxR/reference/get_chemical_weight_fraction.md).

``` r

# BPA chemical weight fractions
bpa_weight_fractions <- get_chemical_weight_fraction('DTXSID7020182')
head(data.table(bpa_weight_fractions))
#>         id        dtxsid                  prodName          displayPuc pucKind
#>      <int>        <char>                    <char>              <char>  <char>
#> 1: 1322890 DTXSID7020182     cr-180 hardener_ pt b Not yet Categorized    <NA>
#> 2: 1322891 DTXSID7020182       curing agent ta30-b Not yet Categorized    <NA>
#> 3: 1322892 DTXSID7020182 cw 1661 cronasphere harde Not yet Categorized    <NA>
#> 4: 1322889 DTXSID7020182 concresive 1001 lpl part  Not yet Categorized    <NA>
#> 5: 1322811 DTXSID7020182  cat-l-ink 50-407r medium Not yet Categorized    <NA>
#> 6: 1322808 DTXSID7020182          3197 steel works Not yet Categorized    <NA>
#> 12 variables not shown: [lowerweightfraction <num>, upperweightfraction <num>, weightfractiontype <char>, gencat <char>, prodfam <char>, prod_type <char>, pucDefinition <char>, sourceName <char>, sourceDescription <char>, sourceUrl <char>, ...]
```

## Multimedia Database (MMDB)

There are several endpoints that provide access to data from the MMDB.

### Medium Categories

First, one can retrieve the MMDB medium categories using
[`get_medium_categories()`](https://usepa.github.io/ctxR/reference/get_medium_categories.md).

``` r

medium_categories <- get_medium_categories()
head(medium_categories)
#>                                                                                                                                       harmonizedMediumDesc
#> 1                                                                                                                                      Outdoor ambient air
#> 2                                                                                                                                        Human breast milk
#> 3                                                           Treated or untreated drinking water supplies, tap water, bottled drinking water, cooking water
#> 4 Processed food products, including dairy products, breads, cooked meats, processed (e.g., canned or frozen) fruit and vegetable products, infant formula
#> 5                                                                                                         Water from groundwater sources (wells, aquifers)
#> 6                                                Human whole blood, blood cells, serum, plasma, or other extractants, including fetal or umbilical samples
#>                   harmonizedMedium
#> 1                      ambient air
#> 2                      breast milk
#> 3                   drinking water
#> 4                     food product
#> 5                      groundwater
#> 6 human blood (whole/serum/plasma)
```

### Single Sample records

Single sample records from MMDB can be retrieved either by DTXSID or by
medium.

``` r

# Data on methylphenanthrene
methylphenanthrene <- get_single_sample_records_by_dtxsid(DTXSID = 'DTXSID001025673')
head(data.table(methylphenanthrene))
#>          id            fullSourceName       chemicalName          dtxsid
#>       <int>                    <char>             <char>          <char>
#> 1: 18925051 Chem Theatre              methylphenanthrene DTXSID001025673
#> 2: 18926743 Chem Theatre              methylphenanthrene DTXSID001025673
#> 3: 18924367 Chem Theatre              methylphenanthrene DTXSID001025673
#> 4: 18925501 Chem Theatre              methylphenanthrene DTXSID001025673
#> 5: 18925267 Chem Theatre              methylphenanthrene DTXSID001025673
#> 6: 18926365 Chem Theatre              methylphenanthrene DTXSID001025673
#> 33 variables not shown: [preferredName <char>, casrn <char>, result <char>, units <char>, cleanedUnits <char>, lod <lgcl>, loq <lgcl>, detectionFlag <lgcl>, resultFlag <lgcl>, detected <int>, ...]

# Data from soil
indoor_air <- get_single_sample_records_by_medium(Medium = 'indoor air')
head(data.table(indoor_air$data))
#>           id            fullSourceName              chemicalName        dtxsid
#>        <int>                    <char>                    <char>        <char>
#> 1: 118822214 USGS Monitoring Data Nati trans-1,2-Dichloroethylen DTXSID7024031
#> 2: 118822215 USGS Monitoring Data Nati            Vinyl chloride DTXSID8021434
#> 3: 119838929 USGS Monitoring Data Nati       Tetrachloroethylene DTXSID2021319
#> 4: 123900830 USGS Monitoring Data Nati      1,1-Dichloroethylene DTXSID8021438
#> 5: 123900831 USGS Monitoring Data Nati  cis-1,2-Dichloroethylene DTXSID2024030
#> 6: 123900832 USGS Monitoring Data Nati         Trichloroethylene DTXSID0021383
#> 33 variables not shown: [preferredName <char>, casrn <char>, result <char>, units <char>, cleanedUnits <char>, lod <lgcl>, loq <char>, detectionFlag <char>, resultFlag <char>, detected <int>, ...]
```

### Aggregate Records

Aggregate records from MMDB can also be retrieved either by DTXSID or by
medium.

``` r

# Data on caffeine
caffeine_agg <- get_aggregate_records_by_dtxsid(DTXSID = 'DTXSID0020232')
head(data.table(caffeine_agg))
#>          id            fullSourceName chemicalName        dtxsid preferredName
#>       <int>                    <char>       <char>        <char>        <char>
#> 1: 61423179 EPA Discharge Monitoring      Caffeine DTXSID0020232      Caffeine
#> 2: 34309226 Information Platform for      Caffeine DTXSID0020232      Caffeine
#> 3: 61425076 EPA Discharge Monitoring      Caffeine DTXSID0020232      Caffeine
#> 4: 63662830 Comparative Toxicogenomic     Caffeine DTXSID0020232      Caffeine
#> 5: 48499404 EPA Discharge Monitoring      Caffeine DTXSID0020232      Caffeine
#> 6: 61426047 EPA Discharge Monitoring      Caffeine DTXSID0020232      Caffeine
#> 32 variables not shown: [casrn <char>, result <char>, units <char>, cleanedUnits <char>, statistic <char>, sampleSize <char>, lod <lgcl>, loq <char>, numDetects <lgcl>, numNonDetects <char>, ...]

# Data from soil
indoor_air_agg <- get_aggregate_records_by_medium(Medium = 'indoor air')
head(data.table(indoor_air_agg$data))
#>          id            fullSourceName chemicalName        dtxsid preferredName
#>       <int>                    <char>       <char>        <char>        <char>
#> 1: 63216485 California Air Resources  Chlorpyrifos DTXSID4020458  Chlorpyrifos
#> 2: 63216491 California Air Resources  Chlorpyrifos DTXSID4020458  Chlorpyrifos
#> 3: 63216492 California Air Resources  Chlorpyrifos DTXSID4020458  Chlorpyrifos
#> 4: 63216493 California Air Resources  Chlorpyrifos DTXSID4020458  Chlorpyrifos
#> 5: 63216498 California Air Resources  Chlorpyrifos DTXSID4020458  Chlorpyrifos
#> 6: 63216499 California Air Resources  Chlorpyrifos DTXSID4020458  Chlorpyrifos
#> 32 variables not shown: [casrn <char>, result <char>, units <char>, cleanedUnits <char>, statistic <char>, sampleSize <lgcl>, lod <char>, loq <lgcl>, numDetects <lgcl>, numNonDetects <lgcl>, ...]
```

### Batch Search

There are batch search versions for several endpoints that gather data
specific to a chemical. Namely,
[`get_exposure_functional_use_batch()`](https://usepa.github.io/ctxR/reference/get_exposure_functional_use_batch.md),
[`get_exposure_functional_use_probability()`](https://usepa.github.io/ctxR/reference/get_exposure_functional_use_probability.md),
[`get_exposure_product_data_batch()`](https://usepa.github.io/ctxR/reference/get_exposure_product_data_batch.md),
[`get_exposure_list_presence_tags_by_dtxsid_batch()`](https://usepa.github.io/ctxR/reference/get_exposure_list_presence_tags_by_dtxsid_batch.md),
[`get_general_exposure_prediction_batch()`](https://usepa.github.io/ctxR/reference/get_general_exposure_prediction_batch.md),
[`get_demographic_exposure_prediction_batch()`](https://usepa.github.io/ctxR/reference/get_demographic_exposure_prediction_batch.md),
[`get_product_use_categories_batch()`](https://usepa.github.io/ctxR/reference/get_product_use_categories_batch.md),
[`get_production_volume_batch()`](https://usepa.github.io/ctxR/reference/get_production_volume_batch.md),
[`get_biomonitoring_data_batch()`](https://usepa.github.io/ctxR/reference/get_biomonitoring_data_batch.md),
[`get_general_use_keywords_batch()`](https://usepa.github.io/ctxR/reference/get_general_use_keywords_batch.md),
[`get_reported_functional_use_batch()`](https://usepa.github.io/ctxR/reference/get_reported_functional_use_batch.md),
[`get_chemical_weight_fraction_batch()`](https://usepa.github.io/ctxR/reference/get_chemical_weight_fraction_batch.md),
[`get_single_sample_records_by_dtxsid_batch()`](https://usepa.github.io/ctxR/reference/get_single_sample_records_by_dtxsid_batch.md),
[`get_single_sample_records_by_medium_batch()`](https://usepa.github.io/ctxR/reference/get_single_sample_records_by_medium_batch.md),
[`get_aggregate_records_by_dtxsid_batch()`](https://usepa.github.io/ctxR/reference/get_aggregate_records_by_dtxsid_batch.md),
and
[`get_aggregate_records_by_medium_batch()`](https://usepa.github.io/ctxR/reference/get_aggregate_records_by_medium_batch.md).
The function
[`get_exposure_functional_use_probability()`](https://usepa.github.io/ctxR/reference/get_exposure_functional_use_probability.md)
returns a data.table with each row corresponding to a unique chemical
and each column representing a functional use category associated to at
least one input chemical. The other batch functions return a named list
of data.frames or data.tables (somtimes with additional meta data), the
names corresponding to the unique chemicals input and the data.frames or
data.tables corresponding to the information for each individual
chemical.

## Conclusion

There are several CTX `Exposure` API endpoints and ctxR contains
functions for each, and batch versions for some of these as well. These
allow users to access various types of exposure data associated to a
given chemical. In this vignette, we explored all of the non-batch
versions and discussed the batch versions. We encourage the user to
experiment with the different endpoints to understand better what sorts
of data are available.
