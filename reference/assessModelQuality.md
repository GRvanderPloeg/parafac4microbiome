# Create randomly initialized models to determine the correct number of components by assessing model quality metrics.

Create randomly initialized models to determine the correct number of
components by assessing model quality metrics.

## Usage

``` r
assessModelQuality(
  X,
  minNumComponents = 1,
  maxNumComponents = 5,
  numRepetitions = 100,
  ctol = 1e-04,
  maxit = 500,
  numCores = 1
)
```

## Arguments

- X:

  Input data

- minNumComponents:

  Minimum number of components (default 1).

- maxNumComponents:

  Maximum number of components (default 5).

- numRepetitions:

  Number of randomly initialized models to create (default 100).

- ctol:

  Relative change in loss tolerated to call the algorithm converged in
  the ALS case (default 1e-4).

- maxit:

  Maximum number of iterations allowed without convergence (default
  500).

- numCores:

  Number of cores to use. If set larger than 1, it will run the job in
  parallel (default 1)

## Value

A list object of the following:

- plots: Plots of all assessed metrics and an overview plot showing a
  summary of all of them.

- metrics: metrics of every created model (number of iterations, sum of
  squared errors, CORCONDIA score and variance explained).

- models: all created models.

## Examples

``` r
X = Fujita2023$data

# Run assessModelQuality with less strict convergence parameters as example
assessment = assessModelQuality(X,
                                minNumComponents=1,
                                maxNumComponents=3,
                                numRepetitions=5)
assessment$plots$overview
```
