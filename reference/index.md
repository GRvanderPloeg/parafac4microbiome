# Package index

## All functions

- [`Fujita2023`](https://grvanderploeg.github.io/parafac4microbiome/reference/Fujita2023.md)
  : Fujita2023 longitudinal microbiome data
- [`Shao2019`](https://grvanderploeg.github.io/parafac4microbiome/reference/Shao2019.md)
  : Shao2019 longitudinal microbiome data
- [`assessModelQuality()`](https://grvanderploeg.github.io/parafac4microbiome/reference/assessModelQuality.md)
  : Create randomly initialized models to determine the correct number
  of components by assessing model quality metrics.
- [`assessModelStability()`](https://grvanderploeg.github.io/parafac4microbiome/reference/assessModelStability.md)
  : Bootstrapping procedure to determine PARAFAC model stability for a
  given number of components.
- [`calcVarExpPerComponent()`](https://grvanderploeg.github.io/parafac4microbiome/reference/calcVarExpPerComponent.md)
  : Calculate the variance explained of a PARAFAC model, per component
- [`calculateFMS()`](https://grvanderploeg.github.io/parafac4microbiome/reference/calculateFMS.md)
  : Calculate Factor Match Score for all initialized models.
- [`calculateSparsity()`](https://grvanderploeg.github.io/parafac4microbiome/reference/calculateSparsity.md)
  : Calculate sparsity across the feature mode of a multi-way array.
- [`calculateVarExp()`](https://grvanderploeg.github.io/parafac4microbiome/reference/calculateVarExp.md)
  : Calculate the variation explained by a PARAFAC model.
- [`corcondia()`](https://grvanderploeg.github.io/parafac4microbiome/reference/corcondia.md)
  : Core Consistency Diagnostic (CORCONDIA) calculation
- [`fac_to_vect()`](https://grvanderploeg.github.io/parafac4microbiome/reference/fac_to_vect.md)
  : Vectorize Fac object
- [`flipLoadings()`](https://grvanderploeg.github.io/parafac4microbiome/reference/flipLoadings.md)
  : Sign flip the loadings of many randomly initialized models to make
  consistent overview plots.
- [`importPhyloseq()`](https://grvanderploeg.github.io/parafac4microbiome/reference/importPhyloseq.md)
  : Import Phyloseq object for PARAFAC modelling
- [`importTreeSummarizedExperiment()`](https://grvanderploeg.github.io/parafac4microbiome/reference/importTreeSummarizedExperiment.md)
  : Import TreeSummarizedExperiment object for PARAFAC modelling
- [`initializePARAFAC()`](https://grvanderploeg.github.io/parafac4microbiome/reference/initializePARAFAC.md)
  : Initialize PARAFAC algorithm input vectors
- [`multiwayCLR()`](https://grvanderploeg.github.io/parafac4microbiome/reference/multiwayCLR.md)
  : Perform a centered log-ratio transform over a multi-way array
- [`multiwayCenter()`](https://grvanderploeg.github.io/parafac4microbiome/reference/multiwayCenter.md)
  : Center a multi-way array
- [`multiwayScale()`](https://grvanderploeg.github.io/parafac4microbiome/reference/multiwayScale.md)
  : Scale a multi-way array
- [`parafac()`](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac.md)
  : Parallel Factor Analysis
- [`parafac_core_als()`](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac_core_als.md)
  : Internal PARAFAC alternating least-squares (ALS) core algorithm
- [`parafac_fun()`](https://grvanderploeg.github.io/parafac4microbiome/reference/parafac_fun.md)
  : PARAFAC loss function calculation
- [`plotModelMetric()`](https://grvanderploeg.github.io/parafac4microbiome/reference/plotModelMetric.md)
  : Plot diagnostics of many initialized PARAFAC models.
- [`plotModelStability()`](https://grvanderploeg.github.io/parafac4microbiome/reference/plotModelStability.md)
  : Plot a summary of the loadings of many initialized parafac models.
- [`plotModelTCCs()`](https://grvanderploeg.github.io/parafac4microbiome/reference/plotModelTCCs.md)
  : Plots Tucker Congruence Coefficients of randomly initialized models.
- [`plotPARAFACmodel()`](https://grvanderploeg.github.io/parafac4microbiome/reference/plotPARAFACmodel.md)
  : Plot a PARAFAC model
- [`processDataCube()`](https://grvanderploeg.github.io/parafac4microbiome/reference/processDataCube.md)
  : Process a multi-way array of count data.
- [`reinflateFac()`](https://grvanderploeg.github.io/parafac4microbiome/reference/reinflateFac.md)
  : Calculate Xhat from a model Fac object
- [`reinflateTensor()`](https://grvanderploeg.github.io/parafac4microbiome/reference/reinflateTensor.md)
  : Create a tensor out of a set of matrices similar to a component
  model.
- [`reshapeData()`](https://grvanderploeg.github.io/parafac4microbiome/reference/reshapeData.md)
  : Reorganize longitudinal microbiome into a data cube ready for
  PARAFAC modelling.
- [`sortComponents()`](https://grvanderploeg.github.io/parafac4microbiome/reference/sortComponents.md)
  : Sort PARAFAC components based on variance explained per component.
- [`transformPARAFACloadings()`](https://grvanderploeg.github.io/parafac4microbiome/reference/transformPARAFACloadings.md)
  : Transform PARAFAC loadings to an orthonormal basis. Note: this
  function only works for 3-way PARAFAC models.
- [`vanderPloeg2024`](https://grvanderploeg.github.io/parafac4microbiome/reference/vanderPloeg2024.md)
  : vanderPloeg2024 longitudinal multi-omics dataset
- [`vect_to_fac()`](https://grvanderploeg.github.io/parafac4microbiome/reference/vect_to_fac.md)
  : Convert vectorized output of PARAFAC to a Fac list object with all
  loadings per mode.
