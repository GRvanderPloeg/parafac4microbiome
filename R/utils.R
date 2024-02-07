#' Reformat parafac model output to plottable data.
#'
#' @param model Model object (output of [multiway::parafac()])
#' @param metadataPerMode List object with metadata per mode in the cube

#' @return List object of the reformatted modes.
#' @export
#'
#' @examples
#' library(multiway)
#' library(dplyr)
#' library(ggplot2)
#' set.seed(0)
#'
#' # Make PARAFAC model
#' model = parafac(Fujita2023$data, nfac=3, nstart=100)
#'
#' # Flipping the sign of some components to make the output plot equal to the paper.
#' model = resign(model, mode="A", absorb="C")
#' model = resign(model, mode="B", absorb="C")
#'
#' # Convert model output to plottable data
#' subjectMetadata = Fujita2023$sampleMetadata %>%
#' filter(treat2=="WC") %>%
#' select(replicate.id) %>%
#' unique()
#'
#' featureMetadata = Fujita2023$taxonomy
#'
#' conditionMetadata = Fujita2023$sampleMetadata %>%
#' select(time) %>%
#' unique()
#'
#' metadataPerMode = list(subjectMetadata, featureMetadata, conditionMetadata)
#' convertedModel = convertModelFormat(model, metadataPerMode)
convertModelFormat = function(model, metadataPerMode=list()){
  stopifnot(class(model) == "parafac")
  stopifnot(class(metadataPerMode) == "list")

  numModes = length(model$const)
  numComponents = ncol(model$A)

  # Convert model object to list for easy iterative access
  if(numModes == 3){
    listedModel = list(model$A, model$B, model$C)
  }
  if(numModes == 4){
    listedModel = list(model$A, model$B, model$C, model$D)
  }

  # Check if the metadata per mode makes sense
  useMetadata = rep(FALSE, numModes)
  if(length(metadataPerMode) != 0){
    for(i in 1:numModes){
      loadings = listedModel[[i]]
      metadata = metadataPerMode[[i]]

      if(is.null(nrow(metadata))){
        warning(paste0("Metadata table of mode ", i, " does not match the model."))
      } else if(nrow(loadings) != nrow(metadata)){
        warning(paste0("Metadata table of mode ", i, " does not match the model."))
      } else{
        useMetadata[i] = TRUE
      }
    }
  }

  # Convert format
  result = list()
  for(i in 1:numModes){
    if(useMetadata[i] == TRUE){
      result[[i]] = cbind(listedModel[[i]], metadataPerMode[[i]]) %>% dplyr::as_tibble()
      colnames(result[[i]]) = c(paste0("Component_", 1:numComponents), colnames(metadataPerMode[[i]]))
    }
    else{
      result[[i]] = listedModel[[i]] %>% dplyr::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet=TRUE))
      colnames(result[[i]]) = paste0("Component_", 1:numComponents)
    }
  }

  return(result)
}
