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
#' \donttest{
#' library(MicrobiotaProcess)
#'
#' # Generate synthetic data
#' sample_info = data.frame(Sample = factor(c("S1", "S2", "S3", "S4", "S5")),
#'                          time = factor(c("T1", "T2", "T1", "T2", "T1")))
#' otu_table = matrix(runif(25, min = 0, max = 100), nrow = 5, ncol = 5,
#'                    dimnames = list(paste0("OTU", 1:5), sample_info$Sample))
#'
#' taxonomy_table = data.frame(OTU = paste0("OTU", 1:5),
#'                             Kingdom = rep("King", 5),
#'                             Phylum = rep("Phy", 5),
#'                             Class = rep("Cla", 5),
#'                             Order = rep("Ord", 5),
#'                             Family = rep("Fam", 5),
#'                             Genus = rep("Gen", 5))
#'
#' # Create Summarized Experiment
#' synthetic_SE = SummarizedExperiment::SummarizedExperiment(
#'                assays = list(otu = otu_table),
#'                colData = sample_info,
#'                rowData = taxonomy_table)
#'
#' # Convert to MicrobiotaProcess object
#' synthetic_MPSE = as.MPSE(synthetic_SE)
#'
#' dataset = importMicrobiotaProcess(synthetic_MPSE,
#'                                   subjectIDs = "Sample",
#'                                   thirdMode = "time",
#'                                   taxa_are_rows = TRUE)
#' }
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
