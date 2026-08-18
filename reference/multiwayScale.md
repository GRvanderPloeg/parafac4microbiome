# Scale a multi-way array

Scale a multi-way array

## Usage

``` r
multiwayScale(X, mode = 2)
```

## Arguments

- X:

  Multi-way array

- mode:

  Mode to scale within: 1=subjects,2=features,3=time (default 2).

## Value

Scaled multi-way array

## Examples

``` r
cube_scl = multiwayCenter(Fujita2023$data)
```
