## code to prepare `Shao2019` dataset goes here

library(tidyverse)

df = read.csv("./data-raw/Shao2019_counts.csv") %>% as_tibble()
taxa = read.csv("./data-raw/Shao2019_speciesMetadata.csv") %>% as_tibble()
sampleMeta = read.csv("./data-raw/Shao2019_sampleMetadata.csv", skip=2) %>% as_tibble()
mapping = read.csv("./data-raw/Shao2019_mapping.tsv", sep="\t") %>% as_tibble()

temp= sampleMeta %>% left_join(mapping, by=c("Accession"="secondary_sample_accession"))
colnames(df) = str_split_fixed(colnames(df),"_",3)[,1]
order = temp %>% filter(run_accession %in% colnames(df)) %>% select(run_accession)

df = df %>% select(-X)
df = df %>% select(order %>% pull())

sampleMetadata = temp %>% filter(run_accession %in% colnames(df))
df = df %>% t() %>% as_tibble()

Shao2019 = list("sampleMetadata"=sampleMetadata, "taxonomy"=taxa, "counts"=df)

usethis::use_data(Shao2019, overwrite = TRUE)
