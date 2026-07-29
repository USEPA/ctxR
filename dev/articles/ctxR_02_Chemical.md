# ctxR: Chemical API

### Introduction

[![](Pictures/ctxR_hex.png)](https://CRAN.R-project.org/package=ctxR)

In this vignette, [CTX Chemical
API](https://comptox.epa.gov/ctx-api/docs/chemical.html) will be
explored.

The foundation of toxicology, toxicokinetics, and exposure is embedded
in the physics and chemistry of chemical-biological interactions. The
accurate characterization of chemical structure linked to commonly used
identifiers, such as names and Chemical Abstracts Service Registry
Numbers (CASRNs), is essential to support both predictive modeling of
the data as well as dissemination and application of the data for
chemical safety decisions.

With cheminformatics as the backbone for research efforts, sources of
available data through the CTX Chemical API include:

- Chemical structures, nomenclature, synonyms, IDs, list associations,
  physicochemical property, environmental fate and transport data from
  the Distributed Structure-Searchable Toxicity
  ([DSSTox](https://www.epa.gov/comptox-tools/distributed-structure-searchable-toxicity-dsstox-database))
  database. DSSTox substance identifiers (DTXSIDs) support linking
  chemical information to a specific chemical across a variety of EPA
  chemical resources. For early references, see [(Richard, A. et
  al. 2002)](https://doi.org/10.1016/S0027-5107(01)00289-5),
  [(Richard, A. et
  al. 2006)](https://files.toxplanet.com/cpdb/pdfs/structure_tox_on_web.pdf),
  and [(Richard, A. et al
  2008)](https://doi.org/10.1080/15376510701857452).
- Predictions from Toxicity Estimation Software Tool
  ([TEST](https://www.epa.gov/comptox-tools/toxicity-estimation-software-tool-test))
  suite of QSAR models. For early references, see [(Martin, T. et
  al. 2001)](https://pubs.acs.org/doi/10.1021/tx0155045), [(Martin, T.
  et al. 2007)](https://doi.org/10.1080/15376510701857353), and
  [(Young, D. et al. 2008)](https://doi.org/10.1002/qsar.200810084).

More information on Chemicals and Chemistry Data can be found here:
<https://www.epa.gov/comptox-tools/downloadable-computational-toxicology-data#SCD>.

**NOTE:** Please see the introductory vignette for an overview of the
*ctxR* package and initial set up instruction with API key storage.

Several ctxR functions can be used to access the CTX Chemical API data,
as described in the following sections.Tables output in each example
have been filtered to only display the first few rows of data.

## Chemical Details Resource

### Get chemical data

[`get_chemical_details()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_details.md)
retrieves chemical detail data either using the chemical identifier
DTXSID or DTXCID. Alternate parameter “projection” determines the type
of data returned. Examples for each are provided below:

#### By DTXSID

``` r

chemical_details_by_dtxsid <- get_chemical_details(DTXSID = 'DTXSID7020182')
```

#### By DTXCID

``` r

chemical_details_by_dtxcid <- get_chemical_details(DTXCID = 'DTXCID30182')
```

#### By Batch Search

``` r

vector_dtxsid<- c("DTXSID7020182", "DTXSID9020112", "DTXSID8021430")
chemical_details_by_batch_dtxsid <- get_chemical_details_batch(DTXSID = vector_dtxsid)

vector_dtxcid <- c("DTXCID30182", "DTXCID801430", "DTXCID90112")
chemical_details_by_batch_dtxcid <- get_chemical_details_batch(DTXCID = vector_dtxcid)
```

## Pubchem Link to GHS classification

### GHS classification

[`check_existence_by_dtxsid()`](https://usepa.github.io/ctxR/dev/reference/check_existence_by_dtxsid.md)
checks if the supplied DTXSID is valid and returns a URL for additional
information on the chemical in the case of a valid DTXSID.

#### By DTXSID

``` r

dtxsid_check_true <- check_existence_by_dtxsid(DTXSID = 'DTXSID7020182')
dtxsid_check_false <- check_existence_by_dtxsid(DTXSID = 'DTXSID7020182f')
```

#### By Batch Search

``` r

vector_dtxsid_and_non_dtxsid <- c('DTXSID7020182F', 'DTXSID7020182', 'DTXSID0020232F')
dtxsid_checks <- check_existence_by_dtxsid_batch(DTXSID = vector_dtxsid_and_non_dtxsid)
```

## Chemical Property Resource

[`get_chemical_by_property_range()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_by_property_range.md)
retrieves data for chemicals that have a specified property within the
input range.

``` r

chemical_by_property_range <- get_chemical_by_property_range(start = 1.311, 
                                         end = 1.313, 
                                         property = 'Density')
```

[`get_chem_info()`](https://usepa.github.io/ctxR/dev/reference/get_chem_info.md)
retrieves specific chemical information for an input chemical. This
includes both experimental and predicted values by default, but
providing “experimental” or “predicted” to the type parameter will
return the specific associated information.

``` r

chemical_info <- get_chem_info(DTXSID = 'DTXSID7020182')
```

## Chemical Fate Resource

[`get_fate_by_dtxsid()`](https://usepa.github.io/ctxR/dev/reference/get_fate_by_dtxsid.md)
retrieves chemical fate data.

``` r

fate_by_dtxsid <- get_fate_by_dtxsid(DTXSID = 'DTXSID7020182')
```

## Chemical Search Resource

Chemicals can be searched using string values. These values can be a
chemical name, DTXSID, DTXCID, CAS Registry Number (CASRN), or InChIKey.
Examples for each are provided by the following:

### By starting value

DTXSID must be complete DTXCID must be complete CAS Registry Number
(CASRN) must be complete InChIKey must contain first 14 characters

``` r

search_starts_with_dtxsid <- chemical_starts_with(word = 'DTXSID7020182')
search_starts_with_chem_name <- chemical_starts_with(word = 'Bisph')
search_starts_with_casrn <- chemical_starts_with(word = '80-05-7')
search_starts_with_inchikey <- chemical_starts_with(word = 'IISBACLAFKSPIT')
```

### By exact value

``` r

search_exact_dtxsid <- chemical_equal(word = 'DTXSID7020182')
search_exact_chem_name <- chemical_equal(word = 'Bisphenol A')
search_exact_casrn <- chemical_equal(word = '80-05-7')
search_exact_inchikey <- chemical_equal(word = 'IISBACLAFKSPIT-UHFFFAOYSA-N')
```

### By substring value

``` r

search_contains_dtxsid <- chemical_contains(word = 'DTXSID702018')
search_contains_chem_name <- chemical_contains(word = 'Bisph')
search_contains_casrn <- chemical_contains(word = '80-05-7')
search_contains_inchikey <- chemical_contains(word = 'IISBACLAF')
```

### Subset for MS-Ready Structures

MS-Ready [(McEachran, A. et
al. 2018)](https://doi.org/10.1186/s13321-018-0299-2) data can be
retrieved using a variety of input information. Examples for each are
provided below:

#### By Mass Range

``` r

msready_by_mass <- get_msready_by_mass(start = 200.9, 
                              end = 200.95)
```

#### By Chemical Formula

``` r

msready_by_formula <- get_msready_by_formula(formula = 'C16H24N2O5S')
```

#### By DTXCID

``` r

msready_by_dtxcid <- get_msready_by_dtxcid(DTXCID = 'DTXCID30182')
```

## List Resource

There are several lists of chemicals one can access using the CCD list
search. These can be filtered by the type, name, inclusion of a specific
chemical, or name of list.

### Get all list types

``` r

get_all_list_types()
```

### All lists by type

``` r

chemical_lists_by_type <- get_chemical_lists_by_type(type =  'federal')
```

### List by name

``` r

public_chemical_list_by_name <- get_public_chemical_list_by_name(listname = 'CCL4')
```

### Lists containing a specific chemical

[`get_lists_containing_chemical()`](https://usepa.github.io/ctxR/dev/reference/get_lists_containing_chemical.md)
retrieves a list of names of chemical lists, each of which contains the
specified chemical.

``` r

lists_containing_chemical <- get_lists_containing_chemical(DTXSID = 'DTXSID7020182')
```

### DTXSIDs for chemical list and starting value

[`get_chemicals_in_list_start()`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list_start.md)
retrieves a list of DTXSIDs for a given starting character string in a
specified list of chemicals.

``` r

chemicals_in_ccl4_start <- get_chemicals_in_list_start(list_name = 'CCL4', word = 'Bi')
```

### DTXSIDs for chemical list and exact value

[`get_chemicals_in_list_exact()`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list_exact.md)
retrieves a list of DTXSIDs matching exactly a given character string in
a specified list of chemicals.

``` r

chemicals_in_ccl4_exact <- get_chemicals_in_list_exact(list_name = 'BIOSOLIDS2021', word = 'Bisphenol A')
```

### DTXSIDs for chemical list and containing value

[`get_chemicals_in_list_contain()`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list_contain.md)
retrieves a list of DTXSIDs that contain a given character string in a
specified list of chemicals.

``` r

chemicals_in_ccl4_contain <- get_chemicals_in_list_contain(list_name = 'CCL4', word = 'Bis')
```

### Chemicals in a specific list

[`get_chemicals_in_list()`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list.md)
retrieves the specific chemical information for each chemical contained
in the specified list.

``` r

chemicals_in_list <- get_chemicals_in_list(list_name = 'CCL4')
```

## Chemical File Resource

There are mrv, mol, and image files that can be accessed using either
the DTXSID or DTXCID. Examples are provided below:

### Get mrv by DTXSID or DTXCID

[`get_chemical_mrv()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_mrv.md)
retrieves mrv file information for a chemical specified either by DTXSID
or DTXCID.

``` r

chemical_mrv_by_dtxsid <- get_chemical_mrv(DTXSID = 'DTXSID7020182')
chemical_mrv_by_dtxcid <- get_chemical_mrv(DTXCID = 'DTXCID30182')
```

### Get mol by DTXSID or DTXCID

[`get_chemical_mol()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_mol.md)
retrieves mol file information for a chemical specified either by DTXSID
or DTXCID.

``` r

chemical_mol_by_dtxsid <- get_chemical_mol(DTXSID = 'DTXSID7020182')
chemical_mol_by_dtxcid <- get_chemical_mol(DTXCID = 'DTXCID30182')
```

### Get structure image by DTXSID, DTXCID, or SMILES

[`get_chemical_image()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_image.md)
retrieves image file information for a chemical specified either by
DTXSID or DTXCID. To visualize the returned array of image information,
the user may use either the
[`png::writePNG()`](https://rdrr.io/pkg/png/man/writePNG.html) or
[`countcolors::plotArrayAsImage()`](https://rdrr.io/pkg/countcolors/man/plotArrayAsImage.html)
functions, among many choices.

``` r

chemical_image_by_dtxsid <- get_chemical_image(DTXSID = 'DTXSID7020182')
chemical_image_by_dtxcid <- get_chemical_image(DTXCID = 'DTXCID30182')
chemical_image_by_smiles <- get_chemical_image(SMILES = 'CC(C)(C1=CC=C(O)C=C1)C1=CC=C(O)C=C1')

countcolors::plotArrayAsImage(chemical_image_by_dtxsid)
countcolors::plotArrayAsImage(chemical_image_by_dtxcid)
countcolors::plotArrayAsImage(chemical_image_by_smiles)
```

## Chemical Synonym Resource

[`get_chemical_synonym()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_synonym.md)
retrieves synonyms for the specified chemical.

``` r

chemical_synonym <- get_chemical_synonym(DTXSID = 'DTXSID7020182')
```

## Conclusion

In this vignette, a variety of functions that access different types of
data found in the `Chemical` endpoints of the CTX APIs were explored.
While this exploration was not exhaustive, it provides a basic
introduction to how one may access data and work with it. Additional
endpoints and corresponding functions exist and we encourage the user to
explore these while keeping in mind the examples contained in this
vignette.
