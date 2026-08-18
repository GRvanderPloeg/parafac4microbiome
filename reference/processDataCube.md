# Process a multi-way array of count data.

Process a multi-way array of count data.

## Usage

``` r
processDataCube(
  dataset,
  sparsityThreshold = 1,
  considerGroups = FALSE,
  groupVariable = "",
  CLR = TRUE,
  centerMode = 0,
  scaleMode = 0
)
```

## Arguments

- dataset:

  A longitudinal microbiome dataset, formatted as follows:

  data

  :   Array object of the data cube filled with counts

  mode1

  :   Dataframe with all the subject metadata, ordered the same as the
      rows in the data cube.

  mode2

  :   Taxonomic classification of the microbiota, ordered the same as
      the columns in the data cube.

  mode3

  :   Dataframe with the time metadata, ordered the same as the third
      dimension in the array.

  See
  [Fujita2023](https://grvanderploeg.github.io/parafac4microbiome/reference/Fujita2023.md),
  [Shao2019](https://grvanderploeg.github.io/parafac4microbiome/reference/Shao2019.md)
  or
  [vanderPloeg2024](https://grvanderploeg.github.io/parafac4microbiome/reference/vanderPloeg2024.md)
  for more information.

- sparsityThreshold:

  Maximum sparsity for a feature to be selected (default=1, i.e. do not
  select features).

- considerGroups:

  Consider groups when calculating sparsity (default=FALSE).

- groupVariable:

  Column name in dataset\$mode1 that should be used to consider groups
  (default="").

- CLR:

  Perform a centered log-ratio transformation of the count data
  (default=TRUE).

- centerMode:

  Mode to center across: 1=subjects,2=features,3=time (default 0, i.e.
  do not center). See
  [`multiwayCenter()`](https://grvanderploeg.github.io/parafac4microbiome/reference/multiwayCenter.md)
  for more information.

- scaleMode:

  Mode to scale within: 1=subjects,2=features,3=time (default 0, i.e. do
  not scale). See
  [`multiwayScale()`](https://grvanderploeg.github.io/parafac4microbiome/reference/multiwayScale.md)
  for more information.

## Value

CLRed, centered and scaled cube

## Examples

``` r
processedCube = processDataCube(Fujita2023)
```
