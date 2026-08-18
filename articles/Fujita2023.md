# Fujita2023

## Introduction

In this vignette a PARAFAC model is created for the `Fujita2023` data.
This is done by first processing the count data. Subsequently, the
appropriate number of components are determined. Then the PARAFAC model
is created and visualized.

``` r

library(parafac4microbiome)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(ggpubr)
```

## Processing the data cube

The data cube in `Fujita2023$data` contains unprocessed counts. The
function
[`processDataCube()`](https://grvanderploeg.github.io/parafac4microbiome/reference/processDataCube.md)
performs the processing of these counts with the following steps:

- It performs feature selection based on the sparsityThreshold setting.
  Sparsity is here defined as the fraction of samples where a microbial
  abundance (ASV/OTU or otherwise) is zero.
- It performs a centered log-ratio transformation of each sample using
  the
  [`compositions::clr()`](https://rdrr.io/pkg/compositions/man/clr.html)
  function with a pseudo-count of one (on all features, prior to
  selection based on sparsity).
- It centers and scales the three-way array. This is a complex subject,
  for which we refer to a [paper by Rasmus Bro and Age
  Smilde](https://doi.org/10.1002/cem.773). By centering across the
  subject mode, we make the subjects comparable to each other within
  each time point. Scaling within the feature mode avoids the PARAFAC
  model focusing on features with abnormally high variation.

The outcome of processing is a new version of the dataset called
`processedFujita`. Please refer to the documentation of
[`processDataCube()`](https://grvanderploeg.github.io/parafac4microbiome/reference/processDataCube.md)
for more information.

``` r

processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, CLR=TRUE, centerMode=1, scaleMode=2)
```

## Determining the correct number of components

A critical aspect of PARAFAC modelling is to determine the correct
number of components. We have developed the functions
[`assessModelQuality()`](https://grvanderploeg.github.io/parafac4microbiome/reference/assessModelQuality.md)
and
[`assessModelStability()`](https://grvanderploeg.github.io/parafac4microbiome/reference/assessModelStability.md)
for this purpose. First, we will assess the model quality and specify
the minimum and maximum number of components to investigate and the
number of randomly initialized models to attempt for each number of
components.

Note: this vignette reflects a minimum working example for analyzing
this dataset due to computational limitations in automatic vignette
rendering. Hence, we only look at 1-3 components with 5 random
initializations each. These settings are not ideal for real datasets.
Please refer to the documentation of
[`assessModelQuality()`](https://grvanderploeg.github.io/parafac4microbiome/reference/assessModelQuality.md)
for more information.

``` r

# Setup
minNumComponents = 1
maxNumComponents = 3
numRepetitions = 3 # number of randomly initialized models
numFolds = 4 # number of jack-knifed models
ctol = 1e-5
maxit = 200
numCores= 1

# Plot settings
colourCols = c("", "Genus", "")
legendTitles = c("", "Genus", "")
xLabels = c("Replicate", "Feature index", "Time point")
legendColNums = c(0,5,0)
arrangeModes = c(FALSE, TRUE, FALSE)
continuousModes = c(FALSE,FALSE,TRUE)

# Assess the metrics to determine the correct number of components
qualityAssessment = assessModelQuality(processedFujita$data, minNumComponents, maxNumComponents, numRepetitions, ctol=ctol, maxit=maxit, numCores=numCores)
```

The overview plot showcases the number of iterations, the sum-of-squared
error, the CORCONDIA and the variance explained for 1-3 components.

``` r

qualityAssessment$plots$overview
```

![](Fujita2023_files/figure-html/overview%20plot-1.png) The overview
plots show that we can reach ~40% explained variation if we take 3
components. The CORCONDIA for those models are ~98, which is well above
the minimum requirement of 60. Based on this overview, either 2 or 3
components seems fine.

## Jack-knifed models

Next, we investigate the stability of the models when jack-knifing out
samples using
[`assessModelStability()`](https://grvanderploeg.github.io/parafac4microbiome/reference/assessModelStability.md).
This will give us more information to choose between 2 or 3 components.

``` r

stabilityAssessment = assessModelStability(processedFujita, minNumComponents=1, maxNumComponents=3, numFolds=numFolds, considerGroups=FALSE,
                                           groupVariable="", colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
                                           ctol=ctol, maxit=maxit, numCores=numCores)
stabilityAssessment$modelPlots[[1]]
```

![](Fujita2023_files/figure-html/model%20stability-1.png)

``` r

stabilityAssessment$modelPlots[[2]]
```

![](Fujita2023_files/figure-html/model%20stability-2.png)

``` r

stabilityAssessment$modelPlots[[3]]
```

![](Fujita2023_files/figure-html/model%20stability-3.png) The
three-component model is stable and can be safely chosen as the final
model.

## Model selection

Since a three-component model is the most appropriate for the
`Fujita2023` dataset, we can now select one of the random
initializations from the
[`assessModelQuality()`](https://grvanderploeg.github.io/parafac4microbiome/reference/assessModelQuality.md)
output as the final model. The selected model corresponds to the one
that explained the largest amount of variation.

``` r

numComponents = 3
modelChoice = which(qualityAssessment$metrics$varExp[,numComponents] == max(qualityAssessment$metrics$varExp[,numComponents]))
finalModel = qualityAssessment$models[[numComponents]][[modelChoice]]
```

Finally, we visualize the model using
[`plotPARAFACmodel()`](https://grvanderploeg.github.io/parafac4microbiome/reference/plotPARAFACmodel.md).

``` r

plotPARAFACmodel(finalModel$Fac, processedFujita, 3, colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Fujita PARAFAC model")
```

![](Fujita2023_files/figure-html/model%20plot-1.png)

You will observe that the loadings for some modes in some components are
all negative. This is due to sign flipping: two modes having negative
loadings cancel out but describe the same thing as two positive
loadings. The
[`flipLoadings()`](https://grvanderploeg.github.io/parafac4microbiome/reference/flipLoadings.md)
function automatically performs this procedure and also sorts the
components by how much variation they describe.

``` r

finalModel = flipLoadings(finalModel, processedFujita$data)

plotPARAFACmodel(finalModel$Fac, processedFujita, 3, colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Fujita PARAFAC model")
```

![](Fujita2023_files/figure-html/flip%20loadings-1.png)
