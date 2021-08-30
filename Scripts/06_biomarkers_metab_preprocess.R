### Summmer Project -- Biomarkers Preprocess- matching participants
### 1st July 2021 - Alicia 

library(utils)
library(tidyverse)
library(openxlsx)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"


### Processing ----
# Load data sets
biomarker=readRDS("../Results/extract_biomarkers.rds")
bb=readRDS("../Results/biobank_clean.rds")


### Pre-processing  ---
# Remove individuals with 20 or more biomarkers missing (participants with $>30$ missing biomarker measurements were excluded.)
biomarker=biomarker[-which(rowSums(is.na(biomarker[-1]))>=20),]


## Select eid of study population 
biomarker=biomarker[which(biomarker$eid %in% bb$eid),]

## Remove two biomarkers with high missing rate (Oestradiol/Rheumatoid_factor)
na_biomk=select_all(biomarker) %>%
  lapply(is.na) %>%
  sapply(function(x)sum(x)/nrow(biomarker),simplify=TRUE)
na_biomk
biomarker=biomarker %>% select(-Oestradiol, -`Rheumatoid factor`)
str(biomarker)

## Log 2 transformation
biomarker[-1]=log2(biomarker[-1])

# Density plots
tmp=biomarker[,-1]
pdf("../Figures/Preliminary/biomarker_distribution.pdf")
par(mfrow = c(4, 7), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 1:ncol(tmp [,-1])){
  xfull=density(tmp[,k], na.rm = T)
  plot(xfull, col="skyblue", xlab="", ylab="", xaxt="n", main=colnames(tmp)[k],
       cex.main = 0.5, cex.axis=0.5, tck=-0.05)
}
dev.off()

saveRDS(biomarker,"../Results/biomarker_master.rds")


# ----------------Metab Annotation--------------------------------------------------------

## Annotate metabolites ## 
metab_all = readRDS(paste(path, "Results/selected_metab_all.rds", sep = ""))
metab_base = readRDS(paste(path, "Results/selected_metab_base.rds", sep = ""))
metab_first = readRDS(paste(path, "Results/selected_metab_first.rds", sep = ""))
metab_annot = read.csv(paste(path, "Dictionaries/Data_Dictionary_Showcase.csv",sep=""))
biobank = readRDS(paste(path, "Results/recoded_covar_clean.rds", sep = ""))

colnames(metab_base) <- sub("-0.0", "", colnames(metab_base))
colnames(metab_first) <- sub("-1.0", "", colnames(metab_first))

metab_col = colnames(metab_all)

select_annot = filter(metab_annot, FieldID %in% metab_col)

# Remove eid
metab_all = metab_all %>% select(!matches("eid_52569"))
metab_base = metab_base %>% select(!matches("eid_52569"))
metab_first = metab_first %>% select(!matches("eid_52569"))

colnames(metab_all)  = select_annot$Field
colnames(metab_base)  = select_annot$Field
colnames(metab_first)  = select_annot$Field

# Save datasets
saveRDS(metab_all, paste(path,"Results/selected_metab_all.rds",sep = ""))
saveRDS(metab_base, paste(path,"Results/selected_metab_base.rds",sep = ""))
saveRDS(metab_first, paste(path,"Results/selected_metab_first.rds",sep = ""))




# Removing repeats from metabolites  --------------------------------------
metab_base = readRDS(paste(path, "Results/selected_metab_base.rds",sep=""))
biomarker = readRDS(paste(path, "Results/biomarker_master.rds", sep=""))
colnames(biomarker)[17] <- "HDL Cholesterol"
col_name=colnames(metab_base[which((colnames(metab_base) %in% colnames(biomarker)))])
### Deleting "Apolipoprotein B"  "Glucose"  "Creatinine"  "Albumin" "HDL Cholesterol" ### 
metab_base = metab_base %>% select(!which(colnames(metab_base) %in% col_name))

## Removing redundant metabolites ##
metab = metab_base %>% select(-"Leucine",-"Isoleucine",-"Valine")
saveRDS(metab, paste(path, "Results/metab_no_bio.rds",sep =""))s

