# Calculate the variance explained of a PARAFAC model, per component

Calculate the variance explained of a PARAFAC model, per component

## Usage

``` r
calcVarExpPerComponent(Fac, X)
```

## Arguments

- Fac:

  Fac object output of a model

- X:

  Input dataset

## Value

Vector of scalars of the percentage of variation explained per component

## Examples

``` r
X = array(rnorm(108*100*10), c(108,100,10))
model = parafac(X, 2)
calcVarExpPerComponent(model$Fac, X)
#> [1] 0.3490251 0.3213559
```
