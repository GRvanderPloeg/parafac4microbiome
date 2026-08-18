# Fujita2023 longitudinal microbiome data

The Fujita2023 longitudinal microbiome dataset as a three-dimensional
array, with replicates in mode 1, microbial abundances in mode 2 and
time in mode 3.

## Usage

``` r
Fujita2023
```

## Format

### `Fujita2023`

A list object with three elements:

- data:

  Array object of the data cube

- mode1:

  Dataframe with all the subject metadata, ordered the same as the rows
  in the data cube.

- mode2:

  Taxonomic classification of the microbiota, ordered the same as the
  columns in the data cube.

- mode3:

  Dataframe with the time metadata, ordered the same as the third
  dimension in the array.

## Source

[doi:10.1186/s40168-023-01474-5](https://doi.org/10.1186/s40168-023-01474-5)
