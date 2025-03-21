## code to prepare `vanderPloeg2024` dataset goes here

library(tidyverse)

# RFdata
rf_data = read.csv("./data-raw/vanderPloeg2024_RFdata.csv")
colnames(rf_data) = c("subject", "id", "fotonr", "day", "group", "RFgroup", "MQH", "SPS(tm)", "Area_delta_R30", "Area_delta_Rmax", "Area_delta_R30_x_Rmax", "gingiva_mean_R_over_G", "gingiva_mean_R_over_G_upper_jaw", "gingiva_mean_R_over_G_lower_jaw")
rf_data = rf_data %>% as_tibble()

rf_data[rf_data$subject == "VSTPHZ", 1] = "VSTPH2"
rf_data[rf_data$subject == "D2VZH0", 1] = "DZVZH0"
rf_data[rf_data$subject == "DLODNN", 1] = "DLODDN"
rf_data[rf_data$subject == "O3VQFX", 1] = "O3VQFQ"
rf_data[rf_data$subject == "F80LGT", 1] = "F80LGF"
rf_data[rf_data$subject == "26QQR0", 1] = "26QQrO"

rf_data2 = read.csv("./data-raw/vanderPloeg2024_RFdata2.csv") %>% as_tibble()
rf_data2 = rf_data2[,c(2,4,181:192)]
rf_data = rf_data %>% left_join(rf_data2)

rf = rf_data %>% select(subject, RFgroup) %>% unique()

# Subject metadata
age_gender = read.csv("./data-raw/vanderPloeg2024_demographics.csv", sep=";")
age_gender = age_gender[2:nrow(age_gender),2:ncol(age_gender)]
age_gender = age_gender %>% as_tibble() %>% filter(onderzoeksgroep == 0) %>% select(naam, leeftijd, geslacht)
colnames(age_gender) = c("subject", "age", "gender")

# Correction for incorrect subject ids
age_gender[age_gender$subject == "VSTPHZ", 1] = "VSTPH2"
age_gender[age_gender$subject == "D2VZH0", 1] = "DZVZH0"
age_gender[age_gender$subject == "DLODNN", 1] = "DLODDN"
age_gender[age_gender$subject == "O3VQFX", 1] = "O3VQFQ"
age_gender[age_gender$subject == "F80LGT", 1] = "F80LGF"
age_gender[age_gender$subject == "26QQR0", 1] = "26QQrO"

age_gender = age_gender %>% arrange(subject)

# Raw data import
microbiome.raw = read.csv("./data-raw/vanderPloeg2024_counts.tsv", sep="\t") %>% as_tibble()
microbiome.meta = microbiome.raw[,1:5] %>% as_tibble()
microbiome.numeric = microbiome.raw[,6:ncol(microbiome.raw)] %>% as_tibble()
taxa = read.csv("./data-raw/vanderPloeg2024_speciesMetadata.tsv", sep="\t")

# Prep subject metadata
sampleInfo = microbiome.meta %>% left_join(rf) %>% left_join(age_gender)
sampleMask = sampleInfo$group == "control" & sampleInfo$niche == "upper jaw, lingual"

sampleInfo_filtered = sampleInfo[sampleMask,]
df_filtered = microbiome.numeric[sampleMask,]

# Prep feature metadata
sparsity = colSums(df_filtered == 0) / nrow(df_filtered)
featureMask = sparsity < 1
df_filtered = df_filtered[,featureMask]
taxa = taxa[featureMask,]

# Fold into data cube
I = 41
J = 2253
K = 7
cube = array(0L, dim=c(I,J,K))

for(k in 1:K){
  temp = cbind(df_filtered, sampleInfo_filtered) %>% as_tibble()
  cube[,,k] = temp %>%
    filter(visit == k) %>%
    right_join(sampleInfo_filtered %>% select(subject) %>% unique()) %>%
    arrange(subject) %>%
    select(-all_of(colnames(sampleInfo_filtered))) %>%
    as.matrix()
}

# prepare export
mode1 = sampleInfo_filtered %>% select(subject, RFgroup) %>% unique() %>% arrange(subject)
mode2 = taxa %>% select(-representative_sequence)
mode3 = sampleInfo_filtered %>% select(visit) %>% unique() %>% arrange(visit)

# Export
vanderPloeg2024 = list("data"=cube, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)
usethis::use_data(vanderPloeg2024, overwrite = TRUE)

# ## Filter based on sparsity per group
# threshold = 0.50
#
# df_low = cbind(microbiome.numeric, sampleInfo) %>%
#   as_tibble() %>%
#   filter(RFgroup == 0) %>%
#   select(-all_of(colnames(sampleInfo)))
#
# df_mid = cbind(microbiome.numeric, sampleInfo) %>%
#   as_tibble() %>%
#   filter(RFgroup == 1) %>%
#   select(-all_of(colnames(sampleInfo)))
#
# df_high = cbind(microbiome.numeric, sampleInfo) %>%
#   as_tibble() %>%
#   filter(RFgroup == 2) %>%
#   select(-all_of(colnames(sampleInfo)))
#
# lowSparsity = colSums(df_low==0) / nrow(df_low)
# midSparsity = colSums(df_mid==0) / nrow(df_mid)
# highSparsity = colSums(df_high==0) / nrow(df_high)
#
# lowSelection = lowSparsity <= threshold
# midSelection = midSparsity <= threshold
# highSelection = highSparsity <= threshold
#
# featureSelection = colnames(microbiome.numeric)[lowSelection | midSelection | highSelection]
#
# taxonomy_filtered = taxa %>% filter(asv %in% featureSelection)
#
# # Filter df
# df_filtered = cbind(microbiome.numeric, sampleInfo) %>%
#   as_tibble() %>%
#   filter(group == "control", niche == "upper jaw, lingual") %>%
#   select(all_of(featureSelection))
#
# # CLR with pseudocount 1
# df_clr = compositions::clr(df_filtered+1) %>% as_tibble()
#
# # Fold data cube - take missing samples into account
#
#
# # Center
# cube_cnt = array(0L, dim=c(I,J,K))
# for(j in 1:J){
#   for(k in 1:K){
#     cube_cnt[,j,k] = cube[,j,k] - mean(cube[,j,k], na.rm=TRUE)
#   }
# }
#
# # Scale
# cube_cnt_scl = array(0L, dim=c(I,J,K))
# for(j in 1:J){
#   cube_cnt_scl[,j,] = cube_cnt[,j,] / sd(cube_cnt[,j,], na.rm=TRUE)
# }


