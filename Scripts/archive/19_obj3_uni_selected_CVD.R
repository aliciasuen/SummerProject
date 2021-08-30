### Summmer Project -- Objective 3 : CVD v.s. each metabolomic feature (Univariate)
### 24th Aug 2021 - Alicia 


# Load packages
library(openxlsx)
library(data.table)
library(dplyr)
library(tidyverse)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

# -------------------------------------------------------------------------
met_no_bio= T

if(met_no_bio){
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_selected_metab.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_selected_biomarker.rds", sep=""))
  combine = readRDS(paste(path, "Results_report/updates/denoised_selected_combined.rds", sep=""))
  combine = combine[order(as.numeric(row.names(combine))),] 
  cvd  = readRDS(paste(path, "Results_report/updates/matched_cvd.rds",sep=""))
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj4/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Univariate_obj4/"
}

cluster = F
comb = F
bio = T

## Metabolites cluster ##
if (cluster){
  metab_clus_glm = metab_clus
  df = metab_clus
  df_glm = metab_clus_glm
}

## Combined dataset ## 
if (comb){
  combine_glm = combine
  df = combine
  df_glm = combine_glm
}

## Biomarkers dataset ## 
if (bio){
  bio_glm = biomarker
  df = biomarker
  df_glm = bio_glm
}

# Exploring pairwise correlations metabolites clusters v.s air --------

df = as.data.frame(scale(df))

X=df
Y=as.factor(cvd$cvd_status)
print(nrow(cvd)==nrow(df))

betas = pval = NULL

for(k in 1:ncol(X)){
  ## Split
  print(paste0("Feature: ",colnames(X)[k]))
  ## logistic
  glm = glm(Y ~ X[,k], family = "binomial")
  betas = c(betas, glm$coefficients[2])
  pval=c(pval, summary(glm)$coefficients[2,4])
}
names(pval)=names(betas)=colnames(X)

sum(p.adjust(pval, method = "bonf") < 0.05) # 18 


if (cluster){
  saveRDS(pval, paste(path_to_Results_report, "pvalues_cluster_uni.rds", sep=""))
  saveRDS(betas, paste(path_to_Results_report, "betas_cluster_uni.rds", sep =""))
  
}
if (comb){
  saveRDS(pval, paste(path_to_Results_report, "pvalues_combine_uni.rds", sep=""))
  saveRDS(betas, paste(path_to_Results_report, "betas_combine_uni.rds", sep =""))
  
}

if (bio){
  saveRDS(pval, paste(path_to_Results_report, "pvalues_bio_uni.rds", sep=""))
  saveRDS(betas, paste(path_to_Results_report, "betas_bio_uni.rds", sep =""))
  
}


