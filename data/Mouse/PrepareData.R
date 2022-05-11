library(dplyr)
setwd("/Users/xiangningxue/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Research/circadian/ThePipeline/CircadianPipeline_private/Rshiny/")
rm(list = ls())
# astro <- mget(load("CircadianPipelineRShiny/data/Mouse/mouse_astro_log2DEseq2_log2CPM.RData"))
load("CircadianPipelineRShiny/data/Mouse/mouse_D1D2_log2DEseq2_separate.RData")

write.csv(clinical_D1 %>% mutate(SampleID = sample), "CircadianPipelineRShiny/data/Mouse/meta_D1_all.csv")
write.csv(clinical_D2 %>% mutate(SampleID = sample), "CircadianPipelineRShiny/data/Mouse/meta_D2_all.csv")

write.csv(clinical_D1 %>% mutate(SampleID = sample) %>% filter(sex == "female"),
          "CircadianPipelineRShiny/data/Mouse/meta_D1_female.csv")
write.csv(clinical_D1 %>% mutate(SampleID = sample) %>% filter(sex == "male"),
          "CircadianPipelineRShiny/data/Mouse/meta_D1_male.csv")
write.csv(clinical_D2 %>% mutate(SampleID = sample) %>% filter(sex == "female"),
          "CircadianPipelineRShiny/data/Mouse/meta_D2_female.csv")
write.csv(clinical_D2 %>% mutate(SampleID = sample) %>% filter(sex == "male"),
          "CircadianPipelineRShiny/data/Mouse/meta_D2_male.csv")

all(clinical_D1$sample==colnames(data_CPMfiltered_log2DEseq2_D1))
all(clinical_D2$sample==colnames(data_CPMfiltered_log2DEseq2_D2))
write.csv(data_CPMfiltered_log2DEseq2_D1, "CircadianPipelineRShiny/data/Mouse/data_D1_all.csv")
write.csv(data_CPMfiltered_log2DEseq2_D2, "CircadianPipelineRShiny/data/Mouse/data_D2_all.csv")

write.csv(data_CPMfiltered_log2DEseq2_D1[,clinical_D1$sex == "female"],
          "CircadianPipelineRShiny/data/Mouse/data_D1_female.csv")
write.csv(data_CPMfiltered_log2DEseq2_D1[,clinical_D1$sex == "male"],
          "CircadianPipelineRShiny/data/Mouse/data_D1_male.csv")
write.csv(data_CPMfiltered_log2DEseq2_D2[,clinical_D2$sex == "female"],
          "CircadianPipelineRShiny/data/Mouse/data_D2_female.csv")
write.csv(data_CPMfiltered_log2DEseq2_D2[,clinical_D2$sex == "male"],
          "CircadianPipelineRShiny/data/Mouse/data_D2_male.csv")

# create a small example --------------------------------------------------
rm(list = ls())
# astro <- mget(load("CircadianPipelineRShiny/data/Mouse/mouse_astro_log2DEseq2_log2CPM.RData"))
load("CircadianPipelineRShiny/data/Mouse/mouse_D1D2_log2DEseq2_separate.RData")
ex = readRDS("CircadianPipelineRShiny/test/male_vs_female_in_D1/save/TOJR_Sidak_FS_p-value_0.05.rds")
ex2 = ex$TOJR %>% filter(TOJR!="arrhy") #725 genes
ex3 = ex$TOJR %>% filter(TOJR=="arrhy")
ex3 = ex3[sample(1:nrow(ex3), 1000-nrow(ex2)), ]
ex2 = rbind.data.frame(ex2, ex3)

# write.csv(clinical_D1 %>% mutate(SampleID = sample) %>% filter(sex == "female"),
#           "CircadianPipelineRShiny/data/Mouse/meta_D1_female.csv")
# write.csv(clinical_D1 %>% mutate(SampleID = sample) %>% filter(sex == "male"),
#           "CircadianPipelineRShiny/data/Mouse/meta_D1_male.csv")
write.csv(data_CPMfiltered_log2DEseq2_D1[ex2$gname ,clinical_D1$sex == "female"],
          "CircadianPipelineRShiny/data/Mouse/data_D1_female_1000.csv")
write.csv(data_CPMfiltered_log2DEseq2_D1[ex2$gname,clinical_D1$sex == "male"],
          "CircadianPipelineRShiny/data/Mouse/data_D1_male_1000.csv")










