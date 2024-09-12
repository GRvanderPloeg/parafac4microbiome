#' Import MicrobiotaProcess object for PARAFAC modelling
#'
#' @param MPobject MicrobiotaProcess object containing at least an OTU table and sample information, preferably also taxonomic information.
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
#' @examples
#' library(phyloseq)
#' library(MicrobiotaProcess)
#'
#' fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
#' fakeTaxa = as.matrix(Fujita2023$mode2)
#' fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
#' colnames(fakeSam) = c("replicate.id", "timepoint")
#'
#' fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=FALSE),
#'                         phyloseq::tax_table(fakeTaxa),
#'                         sample_data(fakeSam))
#' mpse = as.MPSE(fakePhyloseq)
#'
#' dataset = importMicrobiotaProcess(mpse, subjectIDs="replicate.id", thirdMode="timepoint")
importMicrobiotaProcess = function(MPobject, subjectIDs, thirdMode, taxa_are_rows=TRUE){
  stopifnot(methods::is(MPobject, "MPSE"))

  if(is.null(SummarizedExperiment::assays(MPobject)[[1]])){
    stop("MicrobiotaProcess object cannot be imported without an OTU count table.")
  }
  if(is.null(MicrobiotaProcess::mp_extract_sample(MPobject))){
    stop("MicrobiotaProcess object cannot be imported without sample information.")
  }

  otu = SummarizedExperiment::assays(MPobject)[[1]] %>% as.data.frame() %>% dplyr::as_tibble()
  sampleInfo = MicrobiotaProcess::mp_extract_sample(MPobject)

  if(taxa_are_rows){
    otu = otu %>% t() %>% as.data.frame() %>% dplyr::as_tibble()
  }

  if(is.null(MicrobiotaProcess::mp_extract_taxonomy(MPobject))){
    warning("Phyloseq object will be imported without taxonomic information.")
    taxInfo = 1:ncol(otu) %>% as.matrix() %>% as.data.frame() %>% dplyr::as_tibble()
  } else{
    taxInfo = MicrobiotaProcess::mp_extract_taxonomy(MPobject)
  }

  mode1 = sampleInfo %>% dplyr::select(dplyr::all_of(subjectIDs)) %>% dplyr::arrange(subjectIDs) %>% unique()
  mode2 = taxInfo
  mode3 = sampleInfo %>% dplyr::select(dplyr::all_of(thirdMode)) %>% dplyr::arrange(thirdMode) %>% unique()

  I = nrow(mode1)
  J = nrow(mode2)
  K = nrow(mode3)
  items = mode3 %>% dplyr::pull() %>% as.vector()
  data = array(0L, c(I,J,K))
  for(k in 1:K){
    item = items[k]
    mask = sampleInfo[thirdMode] == item
    data[,,k] = cbind(otu[mask,], sampleInfo[mask,]) %>%
      dplyr::as_tibble() %>%
      dplyr::right_join(mode1, by=subjectIDs) %>%
      dplyr::arrange(subjectIDs) %>%
      dplyr::select(-colnames(sampleInfo)) %>%
      as.matrix()
  }

  return(list("data"=data, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3))
}
