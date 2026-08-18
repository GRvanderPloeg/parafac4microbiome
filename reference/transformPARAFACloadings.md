# Transform PARAFAC loadings to an orthonormal basis. Note: this function only works for 3-way PARAFAC models.

Transform PARAFAC loadings to an orthonormal basis. Note: this function
only works for 3-way PARAFAC models.

## Usage

``` r
transformPARAFACloadings(Fac, modeToCorrect, moreOutput = FALSE)
```

## Arguments

- Fac:

  Fac object from a PARAFAC object, see
  [`parafac()`](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac.md).

- modeToCorrect:

  Correct the subject (1), feature (2) or time mode (3).

- moreOutput:

  Give orthonormal basis and transformation matrices as part of output
  (default FALSE).

## Value

Corrected loadings of the specified mode.

## Examples

``` r
processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
model = parafac(processedFujita$data, nfac=2, nstart=1)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
transformedA = transformPARAFACloadings(model$Fac, 1)
#> Error in Fac[[1]]: subscript out of bounds
```
