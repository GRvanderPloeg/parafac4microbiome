## code to prepare `Shao2019` dataset goes here

library(tidyverse)

# Import sample info
sampleMeta = read.csv("./data-raw/Shao2019/Shao2019_sampleMetadata.csv", skip=2) %>% as_tibble()
mapping = read.csv("./data-raw/Shao2019/Shao2019_mapping.tsv", sep="\t") %>% as_tibble()
sampleInfo = sampleMeta %>% left_join(mapping, by=c("Accession"="secondary_sample_accession"))

# Import and fix taxonomy
taxa = read.csv("./data-raw/Shao2019/Shao2019_speciesMetadata.csv") %>% as_tibble()
taxa = taxa %>%
  mutate(OTU = paste0("OTU", 1:nrow(.)), consensus_taxonomy = X.consensus_taxonomy) %>%
  select(OTU, consensus_taxonomy)

# Fix taxonomic levels
taxonomy = read.csv("./data-raw/Shao2019/db_mOTU_taxonomy_ref-mOTUs.tsv", sep="\t") %>% as_tibble()
taxonomy$kingdom = str_split_fixed(taxonomy$kingdom, " ", 2)[,2]
taxonomy$phylum = str_split_fixed(taxonomy$phylum, " ", 2)[,2]
taxonomy$class = str_split_fixed(taxonomy$class, " ", 2)[,2]
taxonomy$order = str_split_fixed(taxonomy$order, " ", 2)[,2]
taxonomy$family = str_split_fixed(taxonomy$family, " ", 2)[,2]
taxonomy$genus = str_split_fixed(taxonomy$genus, " ", 2)[,2]

fixedIDs = str_sub(taxa$consensus_taxonomy, -20, -2)
fixedIDs = gsub("\\[", "", fixedIDs)

taxa = taxa %>%
  mutate(fixedIDs = fixedIDs) %>%
  left_join(taxonomy, by=c("fixedIDs"="ref.mOTU_v2_ID")) %>%
  select(-consensus_taxonomy,-fixedIDs,-specI_cluster)

# Import count data and process
df = read.csv("./data-raw/Shao2019/Shao2019_counts.csv") %>% as_tibble()
colnames(df) = str_split_fixed(colnames(df),"_",3)[,1]

sampleInfo = sampleInfo %>% filter(run_accession %in% colnames(df))

df = df %>% select(all_of(sampleInfo$run_accession)) %>% t() %>% as_tibble()
colnames(df) = taxa$OTU

# Keep only timepoint 4, 7, 21 and Infancy samples
sampleMask = (sampleInfo$Time_point %in% c(4,7,21,"Infancy")) & (sampleInfo$Infancy_sampling_age_months != "Mother")
sampleMask = sampleMask & !is.na(sampleMask) # correct for NA results in mask
sampleInfo_filtered = sampleInfo[sampleMask,]
df_filtered = df[sampleMask,]

# Remove all-zero features for disk space reasons
sparsity = colSums(df_filtered==0) / nrow(df_filtered)
featureMask = sparsity < 1
df_filtered = df_filtered[,featureMask]
taxa = taxa[featureMask,]

# This is the new approach (after 20250402):
Shao2019 = reshapeData(df_filtered, sampleInfo_filtered$Individual, taxa, sampleInfo_filtered$Time_point, timepointOrder=c("4", "7", "21", "Infancy"))

# Fix modes to avoid breaking changes
colnames(Shao2019$mode1) = c("Individual", "index")
Shao2019$mode1 = Shao2019$mode1 %>% left_join(sampleInfo_filtered %>% select(Individual, Delivery_mode) %>% unique()) %>% select(-index)

Shao2019$mode2 = Shao2019$mode2 %>% select(-index)

Shao2019$mode3 = Shao2019$mode3$timepointMetadata %>% as_tibble()
colnames(Shao2019$mode3) = c("Time_point")

# 20250402
# Test for equality in terms of contents to avoid breaking changes:
#
# all.equal(Shao2019, parafac4microbiome::Shao2019)
# [1] TRUE

# Old approach (prior to 20250204)
# # Fold data cube - take missing samples into account
# I = 395
# J = 959
# K = 4
# cube = array(0L, dim=c(I,J,K))
# timepoints = c("4","7","21","Infancy")
#
# for(k in 1:K){
#   temp = cbind(df_filtered, sampleInfo_filtered) %>% as_tibble()
#   cube[,,k] = temp %>%
#     filter(Time_point == timepoints[k]) %>%
#     right_join(sampleInfo_filtered %>% select(Individual) %>% unique()) %>%
#     arrange(Individual) %>%
#     select(-all_of(colnames(sampleInfo_filtered))) %>%
#     as.matrix()
# }
#
# Prepare metadata
# mode1 = sampleInfo_filtered %>%
#   select(Individual, Delivery_mode) %>%
#   unique() %>%
#   arrange(Individual)
#
# mode2 = taxa
#
# mode3 = sampleInfo_filtered %>% filter(Time_point %in% timepoints) %>% select(Time_point) %>% unique()
#
# Export
# Shao2019 = list("data"=cube, "mode1"=mode1, "mode2"=mode2, "mode3"=mode3)

usethis::use_data(Shao2019, overwrite = TRUE)
