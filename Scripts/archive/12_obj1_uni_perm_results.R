### Summmer Project -- Objective 1 : Each exposure v.s. each metabolomic feature (Univariate)
### 25th June 2021 - Alicia 

## Q: Which metabolites are statistically correlated with which exposures in the data?
## --> Univariate linear regression between all possible pairs: setting a given exposure as the outcome and a given metabolite as the predictor 

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))

# Load packages
library(openxlsx)
library(data.table)
library(dplyr)
library(tidyverse)


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

met_all = F
met_base = T
met_first= F
if(met_all){
  biobank=readRDS(paste(path, "Results/matched_bb_all.rds", sep=""))
  metab = readRDS(paste(path, "Results/matched_metab_all.rds", sep=""))
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Univariate_obj1/Air_met_all/"
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Univariate_obj1/Air_met_all/"
  air = select(biobank, c("24003-0.0", "24005-0.0":"24006-0.0"))
  colnames(air) <- c("NO2","PM10", "PM2.5")
}
if(met_base){
  biobank=readRDS(paste(path, "Results/matched_bb_base.rds", sep=""))
  metab = readRDS(paste(path, "Results/matched_metab_base.rds", sep=""))
  metab_cluster = readRDS(paste(path, "Results/matched_clus_metab_base.rds", sep=""))
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Univariate_obj1/Air_met_base/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Univariate_obj1/Air_met_base/"
  air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
  colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")
}

if(met_first){
  biobank=readRDS(paste(path, "Results/matched_bb_first.rds", sep=""))
  metab = readRDS(paste(path, "Results/matched_metab_first.rds", sep=""))
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Univariate_obj1/Air_met_first/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Univariate_obj1/Air_met_first/"
  air = select(biobank, c("24003-0.0", "24005-0.0":"24006-0.0"))
  colnames(air) <- c("NO2","PM10", "PM2.5")
}


## 24003: NO2(2010)
## 24004: NOx(2010)
## 24005: PM10(2010)
## 24006: PM2.5(2010) 
## 24007: PM2.5 absorbance(2010) 
## 24008: PM2.5-10 (2010) 
## 24014: Close to major road 

metab_eid = rownames(metab)
metab=filter(metab, rownames(metab) %in% biobank$eid) 

metab_clus_eid = rownames(metab_cluster)
metab_cluster=filter(metab_cluster, rownames(metab_cluster) %in% biobank$eid) 

metab_glm = metab
metab=scale(metab)
metab_glm$MajorRoad = air$MajorRoad 
metab_glm$MajorRoad = as.factor(metab_glm$MajorRoad)


metab_clus_glm = metab_cluster
metab_cluster=scale(metab_cluster)
metab_clus_glm$MajorRoad = air$MajorRoad 
metab_clus_glm$MajorRoad = as.factor(metab_clus_glm$MajorRoad)

air = scale(air[,1:6])

# Exploring pairwise correlations between individual metabolites  -----------------------------------------
niter=5000
pvals_perm=matrix(NA, nrow=niter, ncol=ncol(metab_cluster))
for (i in 1:niter){
  Y_perm=sample(metab_clus_glm$MajorRoad) # Permutation of the outcome
  for (k in 1:ncol(metab_cluster)){
    X=metab_cluster[,k]
    model=glm(Y_perm~X, family = "binomial")
    model0=glm(Y_perm~1, family = "binomial")
    pvals_perm[i,k]=anova(model, model0, test = "Chisq")$`Pr(>Chi)`[2]
  }
}
Beta = Pvalues = matrix(NA, nrow = ncol(air),ncol = ncol(metab))
print(ncol(air) * ncol(metab)) # 384

for (i in 1:ncol(air)) {
  print(i)
  for (j in 1:ncol(metab)) {
    model1 = lm(air[, i] ~ metab[, j])
    Beta[i, j] = coefficients(model1)["metab[, j]"]
    Pvalues[i, j] = summary(model1)$coefficients["metab[, j]",
                                                 "Pr(>|t|)"]
  }
}

rownames(Pvalues) = rownames(Beta) = colnames(air)
colnames(Pvalues) = colnames(Beta) = colnames(metab)

saveRDS(Pvalues, paste(path_to_results, "pvalues_uni.rds", sep=""))
saveRDS(Beta, paste(path_to_results, "betas_uni.rds", sep =""))



