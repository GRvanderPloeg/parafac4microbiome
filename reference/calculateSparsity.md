# Calculate sparsity across the feature mode of a multi-way array.

Calculate sparsity across the feature mode of a multi-way array.

## Usage

``` r
calculateSparsity(dataset, considerGroups = FALSE, groupVariable = "")
```

## Arguments

- dataset:

  See
  [Fujita2023](https://grvanderploeg.github.io/parafac4microbiome/reference/Fujita2023.md),
  [Shao2019](https://grvanderploeg.github.io/parafac4microbiome/reference/Shao2019.md)
  or
  [vanderPloeg2024](https://grvanderploeg.github.io/parafac4microbiome/reference/vanderPloeg2024.md).

- considerGroups:

  Consider subject groups in calculating sparsity (default FALSE)

- groupVariable:

  Column name in dataset\$mode1 that should be used to consider groups
  (default "")

## Value

Vector of sparsity fractions (N x J) where N is the number of groups and
J is the number of features.

## Examples

``` r
# No groups
sparsity = calculateSparsity(Fujita2023)
length(sparsity)
#> [1] 28
hist(sparsity)


# Consider groups
colnames(Shao2019$mode1)
#> [1] "Individual"    "Delivery_mode"
sparsity = calculateSparsity(Shao2019, considerGroups=TRUE, groupVariable="Delivery_mode")
dim(sparsity)
#> [1]   2 959
hist(sparsity[1,])

hist(sparsity[2,])

```
