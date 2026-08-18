# Vectorize Fac object

Vectorize Fac object

## Usage

``` r
fac_to_vect(Fac)
```

## Arguments

- Fac:

  Fac object output of
  [parafac](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac.md).

## Value

Vectorized Fac object

## Examples

``` r
set.seed(123)
A = array(rnorm(108*2), c(108, 2))
B = array(rnorm(100*2), c(100, 2))
C = array(rnorm(10*2), c(10, 2))
Fac = list(A, B, C)
v = fac_to_vect(Fac)
```
