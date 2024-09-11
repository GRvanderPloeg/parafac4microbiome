#' Import Phyloseq object for PARAFAC modelling
#'
#' @param phyloseqObject Phyloseq object
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
#' @examples
#' library(phyloseq)
#' data(GlobalPatterns)
#' GP = GlobalPatterns
#'
#' # Add subject IDs to the sample data to make this example work
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

  mode1 = sampleInfo %>% dplyr::select(-dplyr::all_of(thirdMode)) %>% dplyr::arrange(subjectIDs) %>% unique()
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
