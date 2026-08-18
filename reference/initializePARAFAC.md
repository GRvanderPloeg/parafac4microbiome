# Initialize PARAFAC algorithm input vectors

Initialize PARAFAC algorithm input vectors

## Usage

``` r
initializePARAFAC(Tensor, nfac, initialization = "random", output = "Fac")
```

## Arguments

- Tensor:

  Input dataset matrix or tensor

- nfac:

  Number of components to initialize.

- initialization:

  Either "random" for random initialization or "svd" for svd based.

- output:

  Output the initialized components as a Fac object ("Fac", default) or
  as a vector ("vect").

## Value

Fac or vector with initialized components.

## Examples

``` r
A = array(rnorm(108,2), c(108,2))
B = array(rnorm(100,2), c(100,2))
C = array(rnorm(10,2), c(10,2))
Tensor = reinflateTensor(A, B, C, returnAsTensor=TRUE)
init = initializePARAFAC(Tensor, 2)
```
