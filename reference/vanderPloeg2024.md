# vanderPloeg2024 longitudinal multi-omics dataset

The vanderPloeg2024 longitudinal multi-omics dataset containing six oral
microbiome niches, as well as salivary metabolomics and oral health
measurements.

## Usage

``` r
vanderPloeg2024
```

## Format

### `vanderPloeg2024`

Each measured dataset contains three elements:

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

[doi:10.1101/2024.03.18.585469](https://doi.org/10.1101/2024.03.18.585469)
