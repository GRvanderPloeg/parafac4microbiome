# Perform a centered log-ratio transform over a multi-way array

Note: Propagates NAs corresponding to missing samples.

## Usage

``` r
multiwayCLR(X, pseudocount = 1)
```

## Arguments

- X:

  Multi-way array of counts

- pseudocount:

  Pseudocount value to use (default 1).

## Value

CLRed cube

## Examples

``` r
cubeCLR = multiwayCLR(Fujita2023$data)
```
