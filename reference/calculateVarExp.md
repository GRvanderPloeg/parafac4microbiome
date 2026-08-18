# Calculate the variation explained by a PARAFAC model.

Calculate the variation explained by a PARAFAC model.

## Usage

``` r
calculateVarExp(Fac, X)
```

## Arguments

- Fac:

  Fac object output from the
  [`parafac()`](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac.md)
  function.

- X:

  Input data of the PARAFAC model.

## Value

The variation explained by the model, expressed as a fraction (between
0-1).

## Examples

``` r
X = Fujita2023$data
model = parafac(X, nfac=1, nstart=1)
calculateVarExp(model$Fac, X)
#> [1] 0.7372695
```
