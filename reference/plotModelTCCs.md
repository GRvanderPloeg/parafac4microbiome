# Plots Tucker Congruence Coefficients of randomly initialized models.

Plots Tucker Congruence Coefficients of randomly initialized models.

## Usage

``` r
plotModelTCCs(models)
```

## Arguments

- models:

  Models list output of
  [`parafac()`](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac.md)
  using output="all".

## Value

Plot of TCCs

## Examples

``` r
processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
models = parafac(processedFujita$data, 3, nstart=10, output="all")
plotModelTCCs(models)
```
