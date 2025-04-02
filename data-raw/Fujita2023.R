## code to prepare `Fujita2023` dataset goes here

library(tidyverse)

# Import sample info
sample_info = readRDS("./data-raw/Fujita2023/Fujita2023_sample_info.rds")
Taxa_list = readRDS("./data-raw/Fujita2023/Fujita2023_taxa_list.rds")
matrixList = readRDS("./data-raw/Fujita2023/Fujita2023_matrixList.rds")

# Prepare data - only WC samples
WC = matrixList$`Water/Medium-C` %>% as_tibble() %>% mutate(sampleID = row.names(matrixList$`Water/Medium-C`))
sampleIDs = WC$sampleID %>% as_tibble()
colnames(sampleIDs) = c("sampleID")
WC = WC %>% select(-sampleID) %>% round()

sampleInfo = sample_info %>% as_tibble() %>% mutate(sampleID = row.names(sample_info))
sampleInfo = sampleIDs %>% left_join(sampleInfo)

taxonomy = Taxa_list %>% mutate(ID = levels(ID)) %>% filter(ID %in% colnames(WC)) %>% select(-identified)

# Remove samples corresponding to NA time points
mask = !is.na(sampleInfo$time)
sampleInfo_filtered = sampleInfo[mask,]
WC_filtered = WC[mask,]

# This is the new approach (after 20250402):
#
# Reshape into cube of counts
Fujita2023 = reshapeData(WC_filtered, sampleInfo_filtered$replicate.id, taxonomy, sampleInfo_filtered$time)

# Fix modes to avoid breaking changes
Fujita2023$mode1 = as.numeric(Fujita2023$mode1$subjectMetadata) %>% as_tibble()
colnames(Fujita2023$mode1) = "replicate.id"

Fujita2023$mode2 = as.data.frame(Fujita2023$mode2 %>% select(-index))

Fujita2023$mode3 = Fujita2023$mode3$timepointMetadata %>% as_tibble()
colnames(Fujita2023$mode3) = "time"

# 20250402
# Test for equality in terms of contents to avoid breaking changes:
#
# names(parafac4microbiome::Fujita2023$mode2$Kingdom) used to return strings of DNA
# this is now removed and should not break anything
#
# all.equal(Fujita2023, parafac4microbiome::Fujita2023, check.attributes=FALSE)
# [1] TRUE

# The following was the old approach (prior to 20250402):
#
# # Mode 1 - replicate mode
# mode1 = sampleInfo_filtered %>% filter(time==1) %>% arrange(replicate.id) %>% select(replicate.id)
#
# # Mode 2 - taxonomy
# mode2 = taxonomy
# row.names(mode2) = NULL
#
# # Mode 3 - time
# mode3 = sampleInfo_filtered %>% filter(time %in% 1:110) %>% select(time) %>% unique()
#
# # Prepare count cube
# I = 8
# J = 28
# K = 110
# cube = array(NA, dim=c(I,J,K))
#
# for(k in 1:K){
#   temp = cbind(WC, sampleInfo) %>% as_tibble()
#   cube[,,k] = temp %>%
#     filter(time == k) %>%
#     arrange(replicate.id) %>%
#     select(-all_of(colnames(sampleInfo))) %>%
#     as.matrix()
# }
#
# # Somehow there are half counts in there, so round the data
# cube = round(cube)
#
# Export
# Fujita2023 = list("data"=cube, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)

usethis::use_data(Fujita2023, overwrite = TRUE)
