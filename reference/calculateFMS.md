# Calculate Factor Match Score for all initialized models.

Calculate Factor Match Score for all initialized models.

## Usage

``` r
calculateFMS(models)
```

## Arguments

- models:

  Output of
  [`parafac()`](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac.md)
  using output="all".

## Value

Vector containing FMS scores of all comparisons

## Examples

``` r
A = array(rnorm(108*2), c(108, 2))
B = array(rnorm(100*2), c(100, 2))
C = array(rnorm(10*2), c(10, 2))
X = reinflateTensor(A, B, C)
models = parafac(X, 2, initialization="random", nstart=10, maxit=2, output="all")
calculateFMS(models)
#>  [1] 0.019222617 0.051410324 0.044553311 0.013180252 0.031413279 0.080113014
#>  [7] 0.070364495 0.052823131 0.081888309 0.021390005 0.032625534 0.049462377
#> [13] 0.020698580 0.012998548 0.050191097 0.024177485 0.010568126 0.071054816
#> [19] 0.021058105 0.064456801 0.028841366 0.010927542 0.016008336 0.008524141
#> [25] 0.032627296 0.022839585 0.038337194 0.018410821 0.046819722 0.019912162
#> [31] 0.010937011 0.085768189 0.007195913 0.043129164 0.063321470 0.011410836
#> [37] 0.020875075 0.055114088 0.036329486 0.018802141 0.022493230 0.016800785
#> [43] 0.012460249 0.022959609 0.012384079
```
