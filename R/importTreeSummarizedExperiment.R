#' Import TreeSummarizedExperiment object for PARAFAC modelling
#'
#' @param treeObject TreeSummarizedExperiment object containing at least an OTU table and sample information, preferably also taxonomic information.
#' @param subjectIDs Column name in the sample information corresponding to the subject IDs.
#' @param thirdMode Column name in the sample information corresponding to the study design aspect to put in the third mode of the data cube.
#' @param taxa_are_rows Boolean specifying if the taxa are in the rows of the OTU table (TRUE) or not (FALSE).
#'
#' @return List object containing:
#'  * 'data': data cube
#'  * 'mode1': metadata of the subject mode
#'  * 'mode2': taxonomy information
#'  * 'mode3': metadata of the third mode
#' @export
#'
#' @examplesIf rlang::is_installed("TreeSummarizedExperiment")
#' library(TreeSummarizedExperiment)
#'
#' fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
#' fakeTaxa = as.matrix(Fujita2023$mode2)
#' fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
#' colnames(fakeSam) = c("replicate.id", "timepoint")
#'
#' fakeTreeObj = TreeSummarizedExperiment(assays = list(Count = fakeOTU),
#'                                       rowData = fakeSam,
#'                                       colData = fakeTaxa)
#' dataset = importTreeSummarizedExperiment(fakeTreeObj,
#'                                          subjectIDs="replicate.id",
#'                                          thirdMode="timepoint",
#'                                          taxa_are_rows=FALSE)
importTreeSummarizedExperiment = function(treeObject, subjectIDs, thirdMode, taxa_are_rows){
  stopifnot(methods::is(treeObject, "TreeSummarizedExperiment"))

  if(is.null(SummarizedExperiment::assays(treeObject)[[1]])){
    stop("TreeSummarizedExperiment object cannot be imported without an OTU count table.")
  }
  if((taxa_are_rows) && min(dim(SummarizedExperiment::colData(treeObject))) == 0){
    stop("TreeSummarizedExperiment object cannot be imported without sample information.")
  }
  if((!taxa_are_rows) && min(dim(SummarizedExperiment::rowData(treeObject))) == 0){
    stop("TreeSummarizedExperiment object cannot be imported without sample information.")
  }

  if(taxa_are_rows){
    otu = SummarizedExperiment::assays(treeObject)[[1]] %>% as.data.frame() %>% dplyr::as_tibble() %>% t() %>% as.data.frame() %>% dplyr::as_tibble()
    sampleInfo = SummarizedExperiment::colData(treeObject) %>% as.data.frame() %>% dplyr::as_tibble()

    if(min(dim(SummarizedExperiment::rowData(treeObject))) == 0){
      warning("TreeSummarizedExperiment object will be imported without taxonomic information.")
      taxInfo = 1:ncol(otu) %>% as.matrix() %>% as.data.frame() %>% dplyr::as_tibble()
    } else{
      taxInfo = SummarizedExperiment::rowData(treeObject) %>% as.data.frame() %>% dplyr::as_tibble()
    }
  } else{
    otu = SummarizedExperiment::assays(treeObject)[[1]] %>% as.data.frame() %>% dplyr::as_tibble()
    sampleInfo = SummarizedExperiment::rowData(treeObject) %>% as.data.frame() %>% dplyr::as_tibble()

    if(min(dim(SummarizedExperiment::colData(treeObject))) == 0){
      warning("TreeSummarizedExperiment object will be imported without taxonomic information.")
      taxInfo = 1:ncol(otu) %>% as.matrix() %>% as.data.frame() %>% dplyr::as_tibble()
    } else{
      taxInfo = SummarizedExperiment::colData(treeObject) %>% as.data.frame() %>% dplyr::as_tibble()
    }
  }

  mode1 = sampleInfo %>%
    dplyr::select(dplyr::all_of(subjectIDs)) %>%
    dplyr::arrange(!!dplyr::sym(subjectIDs)) %>%
    unique()

  mode2 = taxInfo

  mode3 = sampleInfo %>%
    dplyr::select(dplyr::all_of(thirdMode)) %>%
    dplyr::arrange(!!dplyr::sym(thirdMode)) %>%
    unique()

  I = nrow(mode1)
  J = nrow(mode2)
  K = nrow(mode3)

  data = array(NA, c(I,J,K))
  otu_matrix = as.matrix(otu)
  subjectItems = mode1 %>% dplyr::pull() %>% as.vector()
  thirdModeItems = mode3 %>% dplyr::pull() %>% as.vector()

  # Create subject and thirdMode indexes as vectors
  subject_idx = match(sampleInfo[[subjectIDs]], subjectItems)
  third_idx = match(sampleInfo[[thirdMode]], thirdModeItems)

  # Assign otu data to its proper place in the data cube
  for (r in 1:nrow(otu_matrix)) {
    i = subject_idx[r]
    k = third_idx[r]
    if (!is.na(i) && !is.na(k)) {
      data[i,,k] = otu_matrix[r, ]
    }
  }

  return(list("data"=data, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3))
}
