library(tidyverse)
library(ggplot2)
library(ggpubr)
library(stringr)
library(parafac4microbiome)
library(CMTFtoolbox)

# Load count data
raw_data = read.csv("./data-raw/vanderPloeg2024/20221005_wp2/count-table.tsv", sep="\t") %>% as_tibble()
metadata = raw_data %>% select(sample, subject, visit, group, niche)
counts   = raw_data %>% select(-sample, -subject, -visit, -group, -niche)
taxa = read.csv("./data-raw/vanderPloeg2024/20221005_wp2/taxonomic-classification.tsv", sep="\t") %>% as_tibble()

# Load RF data
rf_data = read.csv("./data-raw/vanderPloeg2024/RFdata.csv")
colnames(rf_data) = c("subject", "id", "fotonr", "day", "group", "RFgroup", "MQH", "SPS(tm)", "Area_delta_R30", "Area_delta_Rmax", "Area_delta_R30_x_Rmax", "gingiva_mean_R_over_G", "gingiva_mean_R_over_G_upper_jaw", "gingiva_mean_R_over_G_lower_jaw")
rf_data = rf_data %>% as_tibble()

rf_data[rf_data$subject == "VSTPHZ", 1] = "VSTPH2"
rf_data[rf_data$subject == "D2VZH0", 1] = "DZVZH0"
rf_data[rf_data$subject == "DLODNN", 1] = "DLODDN"
rf_data[rf_data$subject == "O3VQFX", 1] = "O3VQFQ"
rf_data[rf_data$subject == "F80LGT", 1] = "F80LGF"
rf_data[rf_data$subject == "26QQR0", 1] = "26QQrO"

# rf_data2 = read.csv("./data-raw/vanderPloeg2024_RFdata2.csv") %>% as_tibble()
# rf_data2 = rf_data2[,c(2,4,181:192)]
# rf_data = rf_data %>% left_join(rf_data2)

rf = rf_data %>% select(subject, RFgroup) %>% unique()

# Attach RF data to metadata
metadata = metadata %>% left_join(rf)

# Remove test
mask = metadata$group == "control"
counts = counts[mask,]
metadata = metadata[mask,]

# Prepare export of metadata
mode1 = metadata %>% select(subject, RFgroup) %>% unique() %>% arrange(subject)
mode3 = metadata %>% select(visit) %>% unique() %>% arrange(visit) %>% mutate(status=c("Baseline", "EG", "EG", "EG", "EG", "EG", "Resolution"))

# Tongue
tongueMask = metadata$niche == "tongue"
df_tongue = counts[tongueMask,]
metadata_tongue = metadata[tongueMask,]

# Prep feature metadata - THIS IS DIFFERENT FROM TIFN
sparsity = colSums(df_tongue == 0) / nrow(df_tongue)
featureMask = sparsity < 1
df_tongue = df_tongue[,featureMask]
mode2 = taxa[featureMask,]

tongue = reshapeData(df_tongue, metadata_tongue$subject, mode2, metadata_tongue$visit)

colnames(tongue$mode1) = c("subject", "index")
tongue$mode1 = tongue$mode1 %>% left_join(rf) %>% select(-index)
tongue$mode2 = tongue$mode2 %>% select(-index)
colnames(tongue$mode3) = c("visit", "index")
tongue$mode3 = tongue$mode3 %>% mutate(status=c("Baseline", "EG", "EG", "EG", "EG", "EG", "Resolution")) %>% select(-index)

# # Put into cube
# I = length(unique(metadata$subject))
# J = ncol(df_tongue)
# K = max(metadata$visit)
# X = array(0L, c(I,J,K))
#
# for(k in 1:K){
#   X[,,k] = cbind(df_tongue, metadata_tongue) %>%
#     as_tibble() %>%
#     filter(visit == k) %>%
#     right_join(metadata %>% select(subject) %>% unique()) %>%
#     arrange(subject) %>%
#     select(-all_of(colnames(metadata))) %>%
#     as.matrix()
# }
#
# tongue = list("data"=X, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)

# Lowling
lowlingMask = metadata$niche == "lower jaw, lingual"
df_lowling = counts[lowlingMask,]
metadata_lowling = metadata[lowlingMask,]

# Prep feature metadata - THIS IS DIFFERENT FROM TIFN
sparsity = colSums(df_lowling == 0) / nrow(df_lowling)
featureMask = sparsity < 1
df_lowling = df_lowling[,featureMask]
mode2 = taxa[featureMask,]

lowling = reshapeData(df_lowling, metadata_lowling$subject, mode2, metadata_lowling$visit)

colnames(lowling$mode1) = c("subject", "index")
lowling$mode1 = lowling$mode1 %>% left_join(rf) %>% select(-index)
lowling$mode2 = lowling$mode2 %>% select(-index)
colnames(lowling$mode3) = c("visit", "index")
lowling$mode3 = lowling$mode3 %>% mutate(status=c("Baseline", "EG", "EG", "EG", "EG", "EG", "Resolution")) %>% select(-index)

# # Put into cube
# I = length(unique(metadata$subject))
# J = ncol(df_lowling)
# K = max(metadata$visit)
# X = array(0L, c(I,J,K))
#
# for(k in 1:K){
#   X[,,k] = cbind(df_lowling, metadata_lowling) %>%
#     as_tibble() %>%
#     filter(visit == k) %>%
#     right_join(metadata %>% select(subject) %>% unique()) %>%
#     arrange(subject) %>%
#     select(-all_of(colnames(metadata))) %>%
#     as.matrix()
# }
#
# lowling = list("data"=X, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)

# Lowinter
lowinterMask = metadata$niche == "lower jaw, interproximal"
df_lowinter = counts[lowinterMask,]
metadata_lowinter = metadata[lowinterMask,]

# Prep feature metadata - THIS IS DIFFERENT FROM TIFN
sparsity = colSums(df_lowinter == 0) / nrow(df_lowinter)
featureMask = sparsity < 1
df_lowinter = df_lowinter[,featureMask]
mode2 = taxa[featureMask,]

lowinter = reshapeData(df_lowinter, metadata_lowinter$subject, mode2, metadata_lowinter$visit)

colnames(lowinter$mode1) = c("subject", "index")
lowinter$mode1 = lowinter$mode1 %>% left_join(rf) %>% select(-index)
lowinter$mode2 = lowinter$mode2 %>% select(-index)
colnames(lowinter$mode3) = c("visit", "index")
lowinter$mode3 = lowinter$mode3 %>% mutate(status=c("Baseline", "EG", "EG", "EG", "EG", "EG", "Resolution")) %>% select(-index)

# # Put into cube
# I = length(unique(metadata$subject))
# J = ncol(df_lowinter)
# K = max(metadata$visit)
# X = array(0L, c(I,J,K))
#
# for(k in 1:K){
#   X[,,k] = cbind(df_lowinter, metadata_lowinter) %>%
#     as_tibble() %>%
#     filter(visit == k) %>%
#     right_join(metadata %>% select(subject) %>% unique()) %>%
#     arrange(subject) %>%
#     select(-all_of(colnames(metadata))) %>%
#     as.matrix()
# }
#
# lowinter = list("data"=X, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)

# Upling
uplingMask = metadata$niche == "upper jaw, lingual"
df_upling = counts[uplingMask,]
metadata_upling = metadata[uplingMask,]

# Prep feature metadata - THIS IS DIFFERENT FROM TIFN
sparsity = colSums(df_upling == 0) / nrow(df_upling)
featureMask = sparsity < 1
df_upling = df_upling[,featureMask]
mode2 = taxa[featureMask,]

upling = reshapeData(df_upling, metadata_upling$subject, mode2, metadata_upling$visit)

colnames(upling$mode1) = c("subject", "index")
upling$mode1 = upling$mode1 %>% left_join(rf) %>% select(-index)
upling$mode2 = upling$mode2 %>% select(-index)
colnames(upling$mode3) = c("visit", "index")
upling$mode3 = upling$mode3 %>% mutate(status=c("Baseline", "EG", "EG", "EG", "EG", "EG", "Resolution")) %>% select(-index)

# # Put into cube
# I = length(unique(metadata$subject))
# J = ncol(df_upling)
# K = max(metadata$visit)
# X = array(0L, c(I,J,K))
#
# for(k in 1:K){
#   X[,,k] = cbind(df_upling, metadata_upling) %>%
#     as_tibble() %>%
#     filter(visit == k) %>%
#     right_join(metadata %>% select(subject) %>% unique()) %>%
#     arrange(subject) %>%
#     select(-all_of(colnames(metadata))) %>%
#     as.matrix()
# }
#
# upling = list("data"=X, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)

# Upinter
upinterMask = metadata$niche == "upper jaw, interproximal"
df_upinter = counts[upinterMask,]
metadata_upinter = metadata[upinterMask,]

# Prep feature metadata - THIS IS DIFFERENT FROM TIFN
sparsity = colSums(df_upinter == 0) / nrow(df_upinter)
featureMask = sparsity < 1
df_upinter = df_upinter[,featureMask]
mode2 = taxa[featureMask,]

upinter = reshapeData(df_upinter, metadata_upinter$subject, mode2, metadata_upinter$visit)

colnames(upinter$mode1) = c("subject", "index")
upinter$mode1 = upinter$mode1 %>% left_join(rf) %>% select(-index)
upinter$mode2 = upinter$mode2 %>% select(-index)
colnames(upinter$mode3) = c("visit", "index")
upinter$mode3 = upinter$mode3 %>% mutate(status=c("Baseline", "EG", "EG", "EG", "EG", "EG", "Resolution")) %>% select(-index)

# # Put into cube
# I = length(unique(metadata$subject))
# J = ncol(df_upinter)
# K = max(metadata$visit)
# X = array(0L, c(I,J,K))
#
# for(k in 1:K){
#   X[,,k] = cbind(df_upinter, metadata_upinter) %>%
#     as_tibble() %>%
#     filter(visit == k) %>%
#     right_join(metadata %>% select(subject) %>% unique()) %>%
#     arrange(subject) %>%
#     select(-all_of(colnames(metadata))) %>%
#     as.matrix()
# }
#
# upinter = list("data"=X, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)

# Saliva
salivaMask = metadata$niche == "saliva"
df_saliva = counts[salivaMask,]
metadata_saliva = metadata[salivaMask,]

# Prep feature metadata - THIS IS DIFFERENT FROM TIFN
sparsity = colSums(df_saliva == 0) / nrow(df_saliva)
featureMask = sparsity < 1
df_saliva = df_saliva[,featureMask]
mode2 = taxa[featureMask,]

saliva = reshapeData(df_saliva, metadata_saliva$subject, mode2, metadata_saliva$visit)

colnames(saliva$mode1) = c("subject", "index")
saliva$mode1 = saliva$mode1 %>% left_join(rf) %>% select(-index)
saliva$mode2 = saliva$mode2 %>% select(-index)
colnames(saliva$mode3) = c("visit", "index")
saliva$mode3 = saliva$mode3 %>% mutate(status=c("Baseline", "EG", "EG", "EG", "EG", "EG", "Resolution")) %>% select(-index)

# # Put into cube
# I = length(unique(metadata$subject))
# J = ncol(df_saliva)
# K = max(metadata$visit)
# X = array(0L, c(I,J,K))
#
# for(k in 1:K){
#   X[,,k] = cbind(df_saliva, metadata_saliva) %>%
#     as_tibble() %>%
#     filter(visit == k) %>%
#     right_join(metadata %>% select(subject) %>% unique()) %>%
#     arrange(subject) %>%
#     select(-all_of(colnames(metadata))) %>%
#     as.matrix()
# }
#
# saliva = list("data"=X, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)

# Process
# processedTongue = processDataCube(tongue, sparsityThreshold=0.50, considerGroups=TRUE, groupVariable="RFgroup", CLR=TRUE, centerMode=1, scaleMode=2)
# processedLowling = processDataCube(lowling, sparsityThreshold=0.50, considerGroups=TRUE, groupVariable="RFgroup", CLR=TRUE, centerMode=1, scaleMode=2)
# processedLowinter = processDataCube(lowinter, sparsityThreshold=0.50, considerGroups=TRUE, groupVariable="RFgroup", CLR=TRUE, centerMode=1, scaleMode=2)
# processedUpling = processDataCube(upling, sparsityThreshold=0.50, considerGroups=TRUE, groupVariable="RFgroup", CLR=TRUE, centerMode=1, scaleMode=2)
# processedUpinter = processDataCube(upinter, sparsityThreshold=0.50, considerGroups=TRUE, groupVariable="RFgroup", CLR=TRUE, centerMode=1, scaleMode=2)
# processedSaliva = processDataCube(saliva, sparsityThreshold=0.50, considerGroups=TRUE, groupVariable="RFgroup", CLR=TRUE, centerMode=1, scaleMode=2)

# Metabolomics
df = read.csv("./data-raw/vanderPloeg2024/processed_metabolome/Metabolomics.csv", header=FALSE) %>% as_tibble()
mode1 = read.csv("./data-raw/vanderPloeg2024/processed_metabolome/Metabolomics_id_meta.csv", header=FALSE) %>% as_tibble() %>% mutate(subject = V1) %>% select(-V1) %>% left_join(rf)
mode2 = read.csv("./data-raw/vanderPloeg2024/processed_metabolome/Metabolomics_feature_meta.csv", header=FALSE) %>% as_tibble()
colnames(mode2) = c("Type", "Function", "Name")
mode3 = mode3 %>% filter(visit %in% 1:5)

I = length(unique(mode1$subject))
J = nrow(mode2)
K = max(mode3$visit)
X = array(0L, c(I,J,K))

for(k in 1:K){
  X[,,k] = df[,(400*(k-1)+1):(400*k)] %>%
    as.matrix()
}

metabolomics = list("data"=X, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)

# Save
vanderPloeg2024 = list()
vanderPloeg2024$tongue = tongue
vanderPloeg2024$saliva = saliva
vanderPloeg2024$lower_jaw_lingual = lowling
vanderPloeg2024$lower_jaw_interproximal = lowinter
vanderPloeg2024$upper_jaw_lingual = upling
vanderPloeg2024$upper_jaw_interproximal = upinter
vanderPloeg2024$metabolomics = metabolomics

usethis::use_data(vanderPloeg2024, overwrite = TRUE)
