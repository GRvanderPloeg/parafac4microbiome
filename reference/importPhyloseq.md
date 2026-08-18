# Import Phyloseq object for PARAFAC modelling

Import Phyloseq object for PARAFAC modelling

## Usage

``` r
importPhyloseq(phyloseqObject, subjectIDs, thirdMode)
```

## Arguments

- phyloseqObject:

  Phyloseq object containing at least an otu table and sample data,
  preferably also taxonomic information.

- subjectIDs:

  Column name in sam_data corresponding to the subject IDs.

- thirdMode:

  Column name in sam_data corresponding to the study design aspect to
  put in the third mode of the data cube.

## Value

List object containing:

- 'data': data cube

- 'mode1': metadata of the subject mode

- 'mode2': taxonomy information

- 'mode3': metadata of the third mode

## Examples

``` r
library(phyloseq)
data(GlobalPatterns)
GP = GlobalPatterns

# Add custom subject IDs to the sample data to make this example work
alteredSampleData = sample_data(GP)
alteredSampleData$subjectID = c(1,2,3,1,2,1,2,3,1,2,1,2,1,2,3,1,2,3,1,2,3,4,5,1,2,3)
df = phyloseq(otu_table(GP), tax_table(GP), alteredSampleData)

# Make a data cube with SampleType (soil, feces, etc.) as the third mode.
result = importPhyloseq(df, subjectIDs = "subjectID", thirdMode="SampleType")
```
