#' Import Phyloseq object for PARAFAC modelling
#'
#' @param phyloseqObject Phyloseq object containing at least an otu table and sample data, preferably also taxonomic information.
#' @param subjectIDs Column name in sam_data corresponding to the subject IDs.
#' @param thirdMode Column name in sam_data corresponding to the study design aspect to put in the third mode of the data cube.
#'
#' @return List object containing:
#'  * 'data': data cube
#'  * 'mode1': metadata of the subject mode
#'  * 'mode2': taxonomy information
#'  * 'mode3': metadata of the third mode
#' @export
#'
#' @examplesIf rlang::is_installed("phyloseq")
#' library(phyloseq)
#' data(GlobalPatterns)
#' GP = GlobalPatterns
#'
#' # Add custom subject IDs to the sample data to make this example work
#' alteredSampleData = sample_data(GP)
#' alteredSampleData$subjectID = c(1,2,3,1,2,1,2,3,1,2,1,2,1,2,3,1,2,3,1,2,3,4,5,1,2,3)
#' df = phyloseq(otu_table(GP), tax_table(GP), alteredSampleData)
#'
#' # Make a data cube with SampleType (soil, feces, etc.) as the third mode.
#' result = importPhyloseq(df, subjectIDs = "subjectID", thirdMode="SampleType")
importPhyloseq = function(phyloseqObject, subjectIDs, thirdMode){
  stopifnot(methods::is(phyloseqObject, "phyloseq"))

  if(is.null(phyloseqObject@otu_table)){
    stop("Phyloseq object cannot be imported without an OTU count table.")
  }
  if(is.null(phyloseqObject@sam_data)){
    stop("Phyloseq object cannot be imported without sample information.")
  }

  otu = phyloseqObject@otu_table %>% as.data.frame() %>% dplyr::as_tibble()
  sampleInfo = phyloseqObject@sam_data %>% as.data.frame() %>% dplyr::as_tibble() %>% dplyr::select(dplyr::all_of(c(subjectIDs,thirdMode)))

  if(phyloseqObject@otu_table@taxa_are_rows){
    otu = otu %>% t() %>% as.data.frame() %>% dplyr::as_tibble()
  }

  if(is.null(phyloseqObject@tax_table)){
    warning("Phyloseq object will be imported without taxonomic information.")
    taxInfo = 1:ncol(otu) %>% as.matrix() %>% as.data.frame() %>% dplyr::as_tibble()
  } else{
    taxInfo = phyloseqObject@tax_table %>% as.data.frame() %>% dplyr::as_tibble()
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

  subjectItems = mode1 %>% dplyr::pull() %>% as.vector()
  thirdModeItems = mode3 %>% dplyr::pull() %>% as.vector()
  expectedMetadata = data.frame(x=rep(subjectItems, K), y=rep(thirdModeItems, each=I))
  colnames(expectedMetadata) = c(subjectIDs, thirdMode)

  completeData = cbind(otu, sampleInfo) %>%
    dplyr::as_tibble() %>%
    dplyr::right_join(expectedMetadata, by=c(subjectIDs, thirdMode)) %>%
    dplyr::arrange(!!dplyr::sym(thirdMode), !!dplyr::sym(subjectIDs)) %>%
    dplyr::select(-colnames(sampleInfo)) %>%
    t() %>%
    as.matrix() %>%
    rTensor::as.tensor()

  data = rTensor::k_fold(completeData, m=2, modes=c(I,J,K))@data

  return(list("data"=data, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3))
}
