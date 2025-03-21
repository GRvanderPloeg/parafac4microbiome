library(tidyverse)
library(vegan)

# Load microbiome data
microbiome.raw = read.csv("./20221005_wp2/count-table.tsv", sep="\t")
taxa = read.csv("./20221005_wp2/taxonomic-classification.tsv", sep="\t")

microbiome = microbiome.raw[microbiome.raw$group == "control",]
microbiome.meta = microbiome[,1:5]
microbiome.numeric = microbiome[,6:ncol(microbiome)]

alpha.div.result = apply(microbiome.numeric, 1, diversity)

alpha.plot.data = cbind(microbiome.meta, alpha.div.result)
days = c(-14, 0, 2, 5, 9, 14, 21)
alpha.plot.data = alpha.plot.data %>% as_tibble() %>% mutate(day = days[visit]) %>% select(-visit)

# Load RF data
rf_data = read.csv("./RFdata.csv")
colnames(rf_data) = c("subject", "id", "fotonr", "day", "group", "RFgroup", "MQH", "SPS(tm)", "Area_delta_R30", "Area_delta_Rmax", "Area_delta_R30_x_Rmax", "gingiva_mean_R_over_G", "gingiva_mean_R_over_G_upper_jaw", "gingiva_mean_R_over_G_lower_jaw")
rf_data = rf_data %>% as_tibble()

rf_data[rf_data$subject == "VSTPHZ", 1] = "VSTPH2"
rf_data[rf_data$subject == "D2VZH0", 1] = "DZVZH0"
rf_data[rf_data$subject == "DLODNN", 1] = "DLODDN"
rf_data[rf_data$subject == "O3VQFX", 1] = "O3VQFQ"
rf_data[rf_data$subject == "F80LGT", 1] = "F80LGF"
rf_data[rf_data$subject == "26QQR0", 1] = "26QQrO"

rf_data2 = read.csv("./red_fluorescence_data.csv") %>% as_tibble()
rf_data2 = rf_data2[,c(2,4,181:192)]
rf_data = rf_data %>% left_join(rf_data2)

# Load biochemical data
biochemical = read.csv("./biochemical_data.csv",sep=";",dec=",",na.strings="#NULL!")

# Figure 1 from van der Veen et al. 2016
rf_data %>% 
  ggplot(aes(x=day,y=Area_delta_R30, col=as.factor(subject))) + 
  geom_line()

# Overview of RF groups
rf_data %>% 
  filter(day==14) %>%
  ggplot(aes(x=Area_delta_R30, fill=as.factor(RFgroup))) + 
  geom_histogram(bins=25, color="black") +
  xlab("RF% on day 14") +
  ylab("Frequency") +
  scale_fill_discrete(name = "Response group", labels=c("Low", "Mid", "High")) +
  theme(legend.position = "top", text=element_text(size=20))

# Alpha diversity plot
alpha.plot.data %>% 
  ggplot(aes(x=as.factor(day),y=alpha.div.result,fill=as.factor(niche))) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Alpha diversity (Shannon)") +
  scale_fill_discrete(name = "Sampling location") +
  theme(legend.position = "top", text=element_text(size=15))

# pH plot
ph_data = biochemical %>% select("Subject_code", pH_T1, pH_T2, pH_T6, pH_T7, Plaqueformer) %>% as_tibble()
colnames(ph_data) = c("Subject", "pH_T1", "pH_T2", "pH_T6", "pH_T7", "RFgroup")
ph_data = ph_data %>% pivot_longer(-c(Subject, RFgroup))

ph_data$timepoint = str_split_fixed(ph_data$name, "_T", 2)[,2]
ph_data$name = NULL
ph_data$timepoint = as.numeric(ph_data$timepoint)

ph_data %>% 
  mutate(day = days[timepoint]) %>%
  filter(value != "NA") %>%
  ggplot(aes(x=as.factor(day),y=value,fill=as.factor(RFgroup))) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Salivary pH") +
  scale_fill_discrete(name = "Response group", labels=c("Low", "Mid", "High")) +
  theme(legend.position = "top", text=element_text(size=20))

ph_data %>% mutate(day=days[timepoint]) %>% filter(value != "NA", day %in% c(0,14)) %>% ggplot(aes(x=as.factor(RFgroup),y=value,fill=as.factor(day))) + geom_boxplot() + stat_compare_means(method="wilcox.test", label="p.signif")
