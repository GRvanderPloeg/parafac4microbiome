# Create a tensor out of a set of matrices similar to a component model.

Create a tensor out of a set of matrices similar to a component model.

## Usage

``` r
reinflateTensor(A, B, C, returnAsTensor = FALSE)
```

## Arguments

- A:

  I x N matrix corresponding to loadings in the first mode for N
  components.

- B:

  J x N matrix corresponding to loadings in the second mode for N
  components.

- C:

  K x N matrix corresponding to loadings in the third mode for N
  components.

- returnAsTensor:

  Boolean return as rTensor S4 tensor object (default FALSE).

## Value

M, an I x J x K tensor.

## Examples

``` r
A = rnorm(108)
B = rnorm(100)
C = rnorm(10)
M = reinflateTensor(A,B,C)
```
