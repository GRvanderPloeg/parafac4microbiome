## code to prepare `Fujita2023` dataset goes here

library(tidyverse)

# Import sample info
sample_info = readRDS("./data-raw/Fujita2023_sample_info.rds")
sampleInfo = sample_info %>% as_tibble() %>% mutate(sampleID = row.names(sample_info))

Taxa_list = readRDS("./data-raw/Fujita2023_taxa_list.rds")

matrixList = readRDS("./data-raw/Fujita2023_matrixList.rds")

# Prepare data
df = matrix(0L, nrow=nrow(sampleInfo), ncol=nrow(Taxa_list))
colnames(df) = Taxa_list$ID
df = df %>% as_tibble() %>% mutate(sampleID = sampleInfo$sampleID)

WA = matrixList$`Water/Medium-A` %>% as_tibble() %>% mutate(sampleID = row.names(matrixList$`Water/Medium-A`))
WB = matrixList$`Water/Medium-B` %>% as_tibble() %>% mutate(sampleID = row.names(matrixList$`Water/Medium-B`))
WC = matrixList$`Water/Medium-C` %>% as_tibble() %>% mutate(sampleID = row.names(matrixList$`Water/Medium-C`))

SA = matrixList$`Soil/Medium-A` %>% as_tibble() %>% mutate(sampleID = row.names(matrixList$`Soil/Medium-A`))
SB = matrixList$`Soil/Medium-B` %>% as_tibble() %>% mutate(sampleID = row.names(matrixList$`Soil/Medium-B`))
SC = matrixList$`Soil/Medium-C` %>% as_tibble() %>% mutate(sampleID = row.names(matrixList$`Soil/Medium-C`))

for (i in 1:length(levels(Taxa_list$ID))){
  id = levels(Taxa_list$ID)[i]

  if (!(id %in% colnames(WA))){
    WA[id] = rep(0, nrow(WA))
  }
  if (!(id %in% colnames(WB))){
    WB[id] = rep(0, nrow(WB))
  }
  if (!(id %in% colnames(WC))){
    WC[id] = rep(0, nrow(WC))
  }

  if(!(id %in% colnames(SA))){
    SA[id] = rep(0, nrow(SA))
  }
  if(!(id %in% colnames(SB))){
    SB[id] = rep(0, nrow(SB))
  }
  if(!(id %in% colnames(SC))){
    SC[id] = rep(0, nrow(SC))
  }

}

sampleIDs = c(WA$sampleID, WB$sampleID, WC$sampleID, SA$sampleID, SB$sampleID, SC$sampleID) %>% as_tibble()
colnames(sampleIDs) = c("sampleID")
sampleInfo = sampleIDs %>% left_join(sampleInfo)

df = rbind(WA %>% select(levels(Taxa_list$ID)),
           WB %>% select(levels(Taxa_list$ID)),
           WC %>% select(levels(Taxa_list$ID)),
           SA %>% select(levels(Taxa_list$ID)),
           SB %>% select(levels(Taxa_list$ID)),
           SC %>% select(levels(Taxa_list$ID))
) %>% as_tibble()

Fujita2023 = list("sampleMetadata"=sampleInfo, "taxonomy"=Taxa_list, "counts"=df)
usethis::use_data(Fujita2023, overwrite = TRUE)
