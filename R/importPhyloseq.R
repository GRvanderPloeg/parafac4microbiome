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

  otu = phyloseqObject@otu_table

  sampleInfo = phyloseqObject@sam_data %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::select(dplyr::all_of(c(subjectIDs,thirdMode)))

  if(phyloseqObject@otu_table@taxa_are_rows){
    otu = t(otu)
  }

  if(is.null(phyloseqObject@tax_table)){
    warning("Phyloseq object will be imported without taxonomic information.")
    taxInfo = 1:ncol(otu) %>% as.matrix() %>% as.data.frame() %>% dplyr::as_tibble()
  } else{
    taxInfo = phyloseqObject@tax_table %>% as.data.frame() %>% dplyr::as_tibble()
  }

  mode1 = unique(sampleInfo[order(sampleInfo[[subjectIDs]]), subjectIDs])
  mode2 = taxInfo
  mode3 = unique(sampleInfo[order(sampleInfo[[thirdMode]]), thirdMode])

  data = array(NA, c(nrow(mode1),nrow(mode2),nrow(mode3)))
  subjectItems = mode1 %>% dplyr::pull() %>% as.vector()
  thirdModeItems = mode3 %>% dplyr::pull() %>% as.vector()

  # Create subject and thirdMode indexes as vectors
  subject_idx = match(sampleInfo[[subjectIDs]], subjectItems)
  third_idx = match(sampleInfo[[thirdMode]], thirdModeItems)

  # Assign otu data to its proper place in the data cube
  for (r in 1:nrow(otu)) {
    i = subject_idx[r]
    k = third_idx[r]
    if (!is.na(i) && !is.na(k)) {
      data[i,,k] = otu[r, ]
    }
  }

  return(list("data"=data, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3))
}
