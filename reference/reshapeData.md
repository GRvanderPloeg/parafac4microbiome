# Reorganize longitudinal microbiome into a data cube ready for PARAFAC modelling.

Reorganize longitudinal microbiome into a data cube ready for PARAFAC
modelling.

## Usage

``` r
reshapeData(
  Xlong,
  subjectMetadata,
  featureMetadata,
  timepointMetadata,
  timepointOrder = sort(unique(timepointMetadata))
)
```

## Arguments

- Xlong:

  Longitudinal microbiome count data in matrix (long) format.

- subjectMetadata:

  Vector containing the subjects corresponding to the measurements in
  Xlong.

- featureMetadata:

  Taxonomic classification of the microbiota, ordered the same as the
  columns in Xlong.

- timepointMetadata:

  Vector containing the time points corresponding to the measurements in
  Xlong.

- timepointOrder:

  Vector containing the required order of the timepoints in
  timepointMetadata (default: sort(unique(timepointMetadata)) ).

## Value

\#' A list object containing

- data:

  Array object of the data cube

- mode1:

  Dataframe with the subjects, ordered the same as the rows in the data
  cube.

- mode2:

  Taxonomic classification of the microbiota, ordered the same as the
  columns in the data cube.

- mode3:

  Dataframe with the time metadata, ordered the same as the third
  dimension in the data cube.

## Examples

``` r
library(dplyr)

Xlong = array(rnorm(108*5*10), c(108*5, 10))
subjects = rep(1:108, 5)
features = rep(1:10)
timepoints = rep(1:5, each=108)

dataset = reshapeData(Xlong, subjects, features, timepoints)
```
