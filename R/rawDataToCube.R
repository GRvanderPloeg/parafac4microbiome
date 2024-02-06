#' Convert long formatted data tables (two-way) into data cubes (three-way)
#'
#' @param Xlong Tibble object
#' @param subjectMeta Tibble object
#' @param conditionMeta Tibble object
#' @param keepIndividuals Boolean
#'
#' @return list object with the cube and associated sample and condition metadata.
#' @export
#'
#' @examples output = rawDataToCube(Xlong, subjectMeta, conditionMeta, keepIndividuals=TRUE)
rawDataToCube <- function(Xlong, subjectMeta, conditionMeta, keepIndividuals) {

  # Check for duplicates in metadata
  meta <- cbind(subjectMeta, conditionMeta)

  if (sum(duplicated(meta)) > 0) {
    cat("Duplicate sample metadata detected.\n")
    cat("Cube output will only use the first occurrence of each unique subject-condition combination.\n")
    cat("This behavior can be avoided by making sure every subject-condition combination only occurs once.\n")
    cat("The following number of samples will be removed:\n")
    removed <- nrow(Xlong) - length(unique(rownames(Xlong)))
    cat(removed, "\n")
    Xlong <- Xlong[!duplicated(meta), ]
    subjectMeta <- subjectMeta[!duplicated(meta), ]
    conditionMeta <- conditionMeta[!duplicated(meta), ]
  }

  # Determine what the third mode looks like
  uniqueConditions <- unique(conditionMeta)
  numConditions <- length(uniqueConditions)

  # Identify how many samples are missing
  subjects <- unique(subjectMeta)
  expectedNumSamples <- length(subjects) * numConditions
  realNumSamples <- nrow(Xlong)

  numMissingSamples <- expectedNumSamples - realNumSamples

  # Identification of individuals missing one or more samples
  missingSubjects <- character(numMissingSamples)

  numOccurences <- table(subjectMeta)
  missingSampleIterator <- 1

  for (i in seq_along(subjects)) {
    subject <- subjects[i]
    discrepancy <- numConditions - numOccurences[[subject]]
    if (discrepancy > 0) {
      missingSubjects[missingSampleIterator:(missingSampleIterator + discrepancy - 1)] <- rep(subject, discrepancy)
      missingSampleIterator <- missingSampleIterator + discrepancy
    }
  }

  if (keepIndividuals) {

    # Identification of the third mode conditions that are missing
    missingConditions <- character(numMissingSamples)
    uniqueMissingSubjects <- unique(missingSubjects)
    missingSampleIterator <- 1

    for (i in seq_along(uniqueMissingSubjects)) {
      subject <- uniqueMissingSubjects[i]
      conditions <- conditionMeta[subjectMeta == subject]
      expectedConditions <- uniqueConditions[!uniqueConditions %in% conditions]

      if (length(expectedConditions) > 1) {
        missingConditions[missingSampleIterator:(missingSampleIterator + length(expectedConditions) - 1)] <- as.character(expectedConditions)
        missingSampleIterator <- missingSampleIterator + length(expectedConditions)
      } else {
        missingConditions[missingSampleIterator] <- as.character(expectedConditions)
        missingSampleIterator <- missingSampleIterator + 1
      }
    }

    # Append missing data to the dataset
    missingData <- matrix(NA, nrow = numMissingSamples, ncol = ncol(Xlong))
    Xlong <- rbind(Xlong, missingData)

    # Create metadata table
    Xmeta <- tibble(subjectID = c(subjectMeta, missingSubjects),
                    condition = c(conditionMeta, missingConditions))

  } else {

    # Remove subjects that have missing samples from the data
    removeMask <- subjectMeta %in% missingSubjects
    subjectMeta <- subjectMeta[!removeMask]
    conditionMeta <- conditionMeta[!removeMask]
    Xlong <- Xlong[!removeMask, ]

    # Create metadata table
    Xmeta <- tibble(subjectID = subjectMeta, condition = conditionMeta)
  }

  # Sort the data by condition, then subjectID
  Xmeta <- bind_cols(Xmeta, index = 1:nrow(Xmeta)) %>%
    arrange(condition, subjectID)
  keepRowIndices <- Xmeta$index
  Xlong <- Xlong[keepRowIndices, ]

  # Reshape into cube
  I <- nrow(Xlong) / numConditions
  J <- ncol(Xlong)
  K <- numConditions

  Xcube <- array(dim = c(I, J, K))
  for (k in 1:K) {
    Xcube[, , k] <- t(Xlong[((k - 1) * I + 1):(k * I), ])
  }

  # Supply metadata output
  newSubjectMeta <- unique(Xmeta$subjectID)
  newConditionMeta <- unique(Xmeta$condition)

  return(list(Xcube = Xcube, newSubjectMeta = newSubjectMeta, newConditionMeta = newConditionMeta))
}
