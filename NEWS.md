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
