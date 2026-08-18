# Sort PARAFAC components based on variance explained per component.

Sort PARAFAC components based on variance explained per component.

## Usage

``` r
sortComponents(Fac, X)
```

## Arguments

- Fac:

  Fac object output of a
  [parafac](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac.md)
  model

- X:

  Input data

## Value

Fac object of sorted components

## Examples

``` r
X = array(rnorm(108*100*10), c(108,100,10))
model = parafac(X, 2)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======                                                               |  10%  |                                                                              |==============                                                        |  20%  |                                                                              |=====================                                                 |  30%  |                                                                              |============================                                          |  40%  |                                                                              |===================================                                   |  50%  |                                                                              |==========================================                            |  60%  |                                                                              |=================================================                     |  70%  |                                                                              |========================================================              |  80%  |                                                                              |===============================================================       |  90%  |                                                                              |======================================================================| 100%
sortedFac = sortComponents(model$Fac, X)
#> Error in Fac[[1]]: subscript out of bounds
```
