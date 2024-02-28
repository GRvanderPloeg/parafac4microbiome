## code to prepare `Fujita2023` dataset goes here

library(tidyverse)

# Import sample info
sample_info = readRDS("./data-raw/Fujita2023_sample_info.rds")
Taxa_list = readRDS("./data-raw/Fujita2023_taxa_list.rds")
matrixList = readRDS("./data-raw/Fujita2023_matrixList.rds")

# Prepare data - only WC samples
WC = matrixList$`Water/Medium-C` %>% as_tibble() %>% mutate(sampleID = row.names(matrixList$`Water/Medium-C`))
sampleIDs = WC$sampleID %>% as_tibble()
colnames(sampleIDs) = c("sampleID")
WC = WC %>% select(-sampleID)

sampleInfo = sample_info %>% as_tibble() %>% mutate(sampleID = row.names(sample_info))
sampleInfo = sampleIDs %>% left_join(sampleInfo)

# Mode 1 - replicate mode
mode1 = sampleInfo %>% filter(time==1) %>% arrange(replicate.id) %>% select(replicate.id)

# Mode 2 - taxonomy
mode2 = Taxa_list %>% mutate(ID = levels(ID)) %>% filter(ID %in% colnames(WC)) %>% select(-identified)
row.names(mode2) = NULL

# Mode 3 - time
mode3 = sampleInfo %>% filter(time %in% 1:110) %>% select(time) %>% unique()

# Prepare count cube
I = 8
J = 28
K = 110
cube = array(NA, dim=c(I,J,K))

for(k in 1:K){
  temp = cbind(WC, sampleInfo) %>% as_tibble()
  cube[,,k] = temp %>%
    filter(time == k) %>%
    arrange(replicate.id) %>%
    select(-all_of(colnames(sampleInfo))) %>%
    as.matrix()
}

# Export
Fujita2023 = list("data"=cube, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)
usethis::use_data(Fujita2023, overwrite = TRUE)
