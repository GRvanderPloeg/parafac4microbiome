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
#' @examplesIf rlang::is_installed("MicrobiotaProcess")
#' library(MicrobiotaProcess)
#' data(mouse.time.mpse)
#'
#' dataset = importMicrobiotaProcess(mouse.time.mpse[1:5, 1:3],
#'                                   subjectIDs="Sample",
#'                                   thirdMode="time",
#'                                   taxa_are_rows=TRUE)
importMicrobiotaProcess = function(MPobject, subjectIDs, thirdMode, taxa_are_rows=TRUE){
  stopifnot(methods::is(MPobject, "MPSE"))

  if(is.null(SummarizedExperiment::assays(MPobject)[[1]])){
    stop("MicrobiotaProcess object cannot be imported without an OTU count table.")
  }
  if(is.null(MicrobiotaProcess::mp_extract_sample(MPobject))){
    stop("MicrobiotaProcess object cannot be imported without sample information.")
  }

  otu = SummarizedExperiment::assays(MPobject)[[1]]
  sampleInfo = MicrobiotaProcess::mp_extract_sample(MPobject)

  if(taxa_are_rows){
    otu = t(otu)
  }

  if(is.null(MicrobiotaProcess::mp_extract_taxonomy(MPobject))){
    warning("Phyloseq object will be imported without taxonomic information.")
    taxInfo = 1:ncol(otu) %>% as.matrix() %>% as.data.frame() %>% dplyr::as_tibble()
  } else{
    taxInfo = MicrobiotaProcess::mp_extract_taxonomy(MPobject)
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
      data[i,,k] = otu[r,]
    }
  }

  return(list("data"=data, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3))
}
