# Import TreeSummarizedExperiment object for PARAFAC modelling

Import TreeSummarizedExperiment object for PARAFAC modelling

## Usage

``` r
importTreeSummarizedExperiment(
  treeObject,
  subjectIDs,
  thirdMode,
  taxa_are_rows
)
```

## Arguments

- treeObject:

  TreeSummarizedExperiment object containing at least an OTU table and
  sample information, preferably also taxonomic information.

- subjectIDs:

  Column name in the sample information corresponding to the subject
  IDs.

- thirdMode:

  Column name in the sample information corresponding to the study
  design aspect to put in the third mode of the data cube.

- taxa_are_rows:

  Boolean specifying if the taxa are in the rows of the OTU table (TRUE)
  or not (FALSE).

## Value

List object containing:

- 'data': data cube

- 'mode1': metadata of the subject mode

- 'mode2': taxonomy information

- 'mode3': metadata of the third mode

## Examples

``` r
library(TreeSummarizedExperiment)
#> Loading required package: SingleCellExperiment
#> Loading required package: SummarizedExperiment
#> Loading required package: MatrixGenerics
#> Loading required package: matrixStats
#> 
#> Attaching package: ‘MatrixGenerics’
#> The following objects are masked from ‘package:matrixStats’:
#> 
#>     colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
#>     colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
#>     colDiffs, colIQRDiffs, colIQRs, colLogSumExps, colMadDiffs,
#>     colMads, colMaxs, colMeans2, colMedians, colMins, colOrderStats,
#>     colProds, colQuantiles, colRanges, colRanks, colSdDiffs, colSds,
#>     colSums2, colTabulates, colVarDiffs, colVars, colWeightedMads,
#>     colWeightedMeans, colWeightedMedians, colWeightedSds,
#>     colWeightedVars, rowAlls, rowAnyNAs, rowAnys, rowAvgsPerColSet,
#>     rowCollapse, rowCounts, rowCummaxs, rowCummins, rowCumprods,
#>     rowCumsums, rowDiffs, rowIQRDiffs, rowIQRs, rowLogSumExps,
#>     rowMadDiffs, rowMads, rowMaxs, rowMeans2, rowMedians, rowMins,
#>     rowOrderStats, rowProds, rowQuantiles, rowRanges, rowRanks,
#>     rowSdDiffs, rowSds, rowSums2, rowTabulates, rowVarDiffs, rowVars,
#>     rowWeightedMads, rowWeightedMeans, rowWeightedMedians,
#>     rowWeightedSds, rowWeightedVars
#> Loading required package: GenomicRanges
#> Loading required package: stats4
#> Loading required package: BiocGenerics
#> Loading required package: generics
#> 
#> Attaching package: ‘generics’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
#> 
#> Attaching package: ‘BiocGenerics’
#> The following objects are masked from ‘package:stats’:
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from ‘package:base’:
#> 
#>     Filter, Find, Map, Position, Reduce, anyDuplicated, aperm, append,
#>     as.data.frame, basename, cbind, colnames, dirname, do.call,
#>     duplicated, eval, evalq, get, grep, grepl, is.unsorted, lapply,
#>     mapply, match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     rank, rbind, rownames, sapply, saveRDS, table, tapply, unique,
#>     unsplit, which.max, which.min
#> Loading required package: S4Vectors
#> 
#> Attaching package: ‘S4Vectors’
#> The following object is masked from ‘package:utils’:
#> 
#>     findMatches
#> The following objects are masked from ‘package:base’:
#> 
#>     I, expand.grid, unname
#> Loading required package: IRanges
#> 
#> Attaching package: ‘IRanges’
#> The following object is masked from ‘package:phyloseq’:
#> 
#>     distance
#> Loading required package: Seqinfo
#> Loading required package: Biobase
#> Welcome to Bioconductor
#> 
#>     Vignettes contain introductory material; view with
#>     'browseVignettes()'. To cite Bioconductor, see
#>     'citation("Biobase")', and for packages 'citation("pkgname")'.
#> 
#> Attaching package: ‘Biobase’
#> The following object is masked from ‘package:MatrixGenerics’:
#> 
#>     rowMedians
#> The following objects are masked from ‘package:matrixStats’:
#> 
#>     anyMissing, rowMedians
#> The following object is masked from ‘package:phyloseq’:
#> 
#>     sampleNames
#> Loading required package: Biostrings
#> Loading required package: XVector
#> 
#> Attaching package: ‘Biostrings’
#> The following object is masked from ‘package:base’:
#> 
#>     strsplit

fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
fakeTaxa = as.matrix(Fujita2023$mode2)
fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
colnames(fakeSam) = c("replicate.id", "timepoint")

fakeTreeObj = TreeSummarizedExperiment(assays = list(Count = fakeOTU),
                                      rowData = fakeSam,
                                      colData = fakeTaxa)
dataset = importTreeSummarizedExperiment(fakeTreeObj,
                                         subjectIDs="replicate.id",
                                         thirdMode="timepoint",
                                         taxa_are_rows=FALSE)
```
