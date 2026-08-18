# Sign flip the loadings of many randomly initialized models to make consistent overview plots.

Sign flip the loadings of many randomly initialized models to make
consistent overview plots.

## Usage

``` r
flipLoadings(models, X)
```

## Arguments

- models:

  Output of
  [parafac](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac.md).

- X:

  Input dataset of parafac modelling procedure.

## Value

models with sign flipped components where applicable.

## Examples

``` r
A = array(rnorm(108*2), c(108,2))
B = array(rnorm(100*2), c(100,2))
C = array(rnorm(10*2), c(10,2))
X = reinflateTensor(A, B, C)
models = parafac(X, 2, nstart=10, output="all", sortComponents=TRUE)
flippedModels = flipLoadings(models, X)
```
