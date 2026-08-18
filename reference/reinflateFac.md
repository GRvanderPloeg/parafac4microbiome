# Calculate Xhat from a model Fac object

Calculate Xhat from a model Fac object

## Usage

``` r
reinflateFac(Fac, X, returnAsTensor = FALSE)
```

## Arguments

- Fac:

  Fac object from parafac

- X:

  Input data X

- returnAsTensor:

  Boolean to return Xhat as rTensor tensor (TRUE) or matrix (default,
  FALSE).

## Value

Xhat

## Examples

``` r
processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
model = parafac(processedFujita$data, nfac=1, nstart=1)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
Xhat = reinflateFac(model$Fac, processedFujita$data)
#> Error in Fac[[1]]: subscript out of bounds
```
