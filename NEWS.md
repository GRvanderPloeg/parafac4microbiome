# parafac4microbiome 1.1.0

# parafac4microbiome 1.0.3

* Minor changes to tests for `parafac_gradient` to make them more robust towards various platforms checked by CRAN.

# parafac4microbiome 1.0.2

* Further rework of `importPhyloseq`, `importTreeSummarizedExperiment` and `importMicrobiotaProcess` to meet CRAN requirements.

# parafac4microbiome 1.0.1

* Streamlined examples in `importPhyloseq`, `importTreeSummarizedExperiment` and `importMicrobiotaProcess`.

# parafac4microbiome 1.0.0

* Edited DESCRIPTION to meet CRAN requirements.
* `importPhyloseq` was modified to use rTensor-based cube folding instead of a for-loop.
* `importTreeSummarizedExperiment` was modified to use rTensor-based cube folding instead of a for-loop.
* `importMicrobiotaProcess` was modified to use rTensor-based cube folding instead of a for-loop.

# parafac4microbiome 0.2.0

* 'parafac' is now capable of running an all-at-once optimization using the methods="opt" parameter. For now, the default remains methods="als" (i.e. the ALS algorithm) because it converges faster to a similar solution.
* `importPhyloseq` allows the user to import a phyloseq object for parafac modelling.
* `importTreeSummarizedExperiment` allows the user to import a TreeSummarizedExperiment object for parafac modelling.
* `importMicrobiotaProcess` allows the user to import a MicrobiotaProcess object for parafac modelling.
* The text in the vignettes were updated to better reflect the changes per version 0.1.0.
* The readme and vignettes figures now use sign flipping to make comparison with the paper easier.
* Some documentation and testing changes anticipating a CRAN release.

# parafac4microbiome 0.1.0

* `parafac` is now a custom function based on an ALS algorithm allowing for much more output (see documentation).
* `initializePARAFAC` initializes the input vectors either randomly on based on a best-guess SVD model of the unfolded array.
* `parafac_core_als` contains this ALS algorithm.
* `parafac_fun` calculates the loss of a parafac model in anticipation of an all-at-once optimization implementation.
* `assessNumComponents` and `checkModelStability` have been renamed into `assessModelQuality` and `assessModelStability` respectively to clarify their use.
* `checkModelStability` now works with a minimum and maximum number of components.
* `checkModelStability` reports Factor Match Score in a plot.
* `calculateFMS` calculates pairwise Factor Match Scores for a list of model objects.
* `parafac`, `multiwayCenter`, `multiwayScale` and `multiwayCLR` are now based on the new rTensor dependency for tensor unfolding
* `plotOverallTCCs` has been merged with `plotModelTCCs`.
* Many bugfixes. PARAFAC model solutions should be more stable as a result.
* Many documentation changes across the board. Overall, the use case per function should be much clearer.
* Vignettes are updated to reflect and utilize the new changes.
* Removed dependency: paramGUI

# parafac4microbiome 0.0.2

* Added `NEWS.md`
