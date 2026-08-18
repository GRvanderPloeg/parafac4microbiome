# Convert vectorized output of PARAFAC to a Fac list object with all loadings per mode.

Convert vectorized output of PARAFAC to a Fac list object with all
loadings per mode.

## Usage

``` r
vect_to_fac(vect, X, sortComponents = FALSE)
```

## Arguments

- vect:

  Vectorized output of PARAFAC modelling

- X:

  Input data

- sortComponents:

  Sort the order of the components by variation explained (default
  FALSE).

## Value

Fac: list object with all loadings in all components per mode, ordered
the same way as Z\$modes.

## Examples

``` r
set.seed(123)
A = array(rnorm(108*2), c(108, 2))
B = array(rnorm(100*2), c(100, 2))
C = array(rnorm(10*2), c(10, 2))

X = reinflateTensor(A, B, C)
result = initializePARAFAC(X, 2, initialization="random", output="vect")
Fac = vect_to_fac(result, X)
```
