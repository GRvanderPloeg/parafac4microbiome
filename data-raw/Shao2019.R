## code to prepare `Shao2019` dataset goes here

library(tidyverse)

# Import sample info
sampleMeta = read.csv("./data-raw/Shao2019_sampleMetadata.csv", skip=2) %>% as_tibble()
mapping = read.csv("./data-raw/Shao2019_mapping.tsv", sep="\t") %>% as_tibble()
sampleInfo = sampleMeta %>% left_join(mapping, by=c("Accession"="secondary_sample_accession"))

# Import and fix taxonomy
taxa = read.csv("./data-raw/Shao2019_speciesMetadata.csv") %>% as_tibble()
taxa = taxa %>%
  mutate(OTU = paste0("OTU", 1:nrow(.)), consensus_taxonomy = X.consensus_taxonomy) %>%
  select(OTU, consensus_taxonomy)

# Import count data and process
df = read.csv("./data-raw/Shao2019_counts.csv") %>% as_tibble()
colnames(df) = str_split_fixed(colnames(df),"_",3)[,1]

sampleInfo = sampleInfo %>% filter(run_accession %in% colnames(df))

df = df %>% select(all_of(sampleInfo$run_accession)) %>% t() %>% as_tibble()
colnames(df) = taxa$OTU

# Temporary addition: process down to input data for PARAFAC

# Keep only timepoint 4, 7, 21 and Infancy samples
sampleInfo_filtered = sampleInfo %>%
  filter(Infancy_sampling_age_months != "Mother", Time_point %in% c(4,7,21,"Infancy"))

# In-between step: filter taxa to taxa with at least 1 non-zero value
featureSelection = names(colSums(df)[colSums(df) > 0])
taxonomy = taxa %>% filter(OTU %in% featureSelection)

df = cbind(df, sampleInfo) %>%
  filter(Infancy_sampling_age_months != "Mother", Time_point %in% c(4,7,21,"Infancy")) %>%
  select(featureSelection)

## Filter based on sparsity per group
threshold = 0.90

df_vaginal = cbind(df, sampleInfo_filtered) %>%
  as_tibble() %>%
  filter(Delivery_mode == "Vaginal") %>%
  select(-all_of(colnames(sampleInfo_filtered)))

df_csection = cbind(df, sampleInfo_filtered) %>%
  as_tibble() %>%
  filter(Delivery_mode == "Caesarean") %>%
  select(-all_of(colnames(sampleInfo_filtered)))

vaginalSparsity = colSums(df_vaginal==0) / nrow(df_vaginal)
csectionSparsity = colSums(df_csection==0) / nrow(df_csection)

vaginalSelection = vaginalSparsity <= threshold
csectionSelection = csectionSparsity <= threshold
featureSelection = colnames(df)[vaginalSelection | csectionSelection]

taxonomy_filtered = taxa %>% filter(OTU %in% featureSelection)

# Filter df
df_filtered = df %>%
  select(all_of(featureSelection))

# CLR with pseudocount 1
df_clr = compositions::clr(df_filtered+1) %>% as_tibble()

# Fold data cube - take missing samples into account
I = 395
J = 90
K = 4
cube = array(0L, dim=c(I,J,K))
timepoints = c("4","7","21","Infancy")

for(k in 1:K){
  temp = cbind(df_clr, sampleInfo_filtered) %>% as_tibble()
  cube[,,k] = temp %>%
    filter(Time_point == timepoints[k]) %>%
    right_join(sampleInfo_filtered %>% select(Individual) %>% unique()) %>%
    arrange(Individual) %>%
    select(-all_of(colnames(sampleInfo_filtered))) %>%
    as.matrix()
}

# Center
cube_cnt = array(0L, dim=c(I,J,K))
for(j in 1:J){
  for(k in 1:K){
    cube_cnt[,j,k] = cube[,j,k] - mean(cube[,j,k], na.rm=TRUE)
  }
}

# Scale
cube_cnt_scl = array(0L, dim=c(I,J,K))
for(j in 1:J){
  cube_cnt_scl[,j,] = cube_cnt[,j,] / sd(cube_cnt[,j,], na.rm=TRUE)
}

# Export
Shao2019 = list("sampleMetadata"=sampleInfo_filtered, "taxonomy"=taxonomy_filtered, "data"=cube_cnt_scl)
usethis::use_data(Shao2019, overwrite = TRUE)
