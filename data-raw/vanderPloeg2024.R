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

# Prep subject metadata
subjectMetadata = rf %>% left_join(age_gender)

# Raw data import
microbiome.raw = read.csv("./data-raw/vanderPloeg2024_counts.tsv", sep="\t")
taxa = read.csv("./data-raw/vanderPloeg2024_speciesMetadata.tsv", sep="\t")

vanderPloeg2024 = list("subjectMetadata"=subjectMetadata, "taxonomy"=taxa, "counts"=microbiome.raw)

usethis::use_data(vanderPloeg2024, overwrite = TRUE)
