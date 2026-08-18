# Internal PARAFAC alternating least-squares (ALS) core algorithm

Internal PARAFAC alternating least-squares (ALS) core algorithm

## Usage

``` r
parafac_core_als(Tensor, nfac, init, maxit = 500, ctol = 1e-04)
```

## Arguments

- Tensor:

  Tensor data object

- nfac:

  Number of components to compute

- init:

  Initialization from
  [initializePARAFAC](https://grvanderploeg.github.io/parafac4microbiome/reference/initializePARAFAC.md).

- maxit:

  Maximum number of iterations to run (default 500).

- ctol:

  Loss function tolerance for convergence (default 1e-4)

## Value

List containing a Fac object and the loss per iteration

## Examples

``` r
A = array(rnorm(108*2), c(108,2))
B = array(rnorm(100*2), c(100,2))
C = array(rnorm(10*2), c(10,2))
Tensor = reinflateTensor(A, B, C)
init = initializePARAFAC(Tensor, 2)
model = parafac_core_als(Tensor, 2, init)
```
