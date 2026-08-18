# Core Consistency Diagnostic (CORCONDIA) calculation

Core Consistency Diagnostic (CORCONDIA) calculation

## Usage

``` r
corcondia(X, Fac)
```

## Arguments

- X:

  Input data matrix

- Fac:

  PARAFAC model Fac object

## Value

Scalar of the CORCONDIA value

## Examples

``` r
X = Fujita2023$data
model = parafac(X, 2)
corcondia(X, model$Fac)
#> [1] 89.55985
```
