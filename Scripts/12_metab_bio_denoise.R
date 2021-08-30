### Summmer Project -- Metabolite denoise 
### 13th June 2021 - Alicia 

## Aim : Account for confounding effects from Age, Sex, Education, Smoking, Alcohol intake

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))


# Load packages 
library(stats)
library(tidyverse)
library(dplyr)
library(lme4)
library(nloptr)
library(ggeffects)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

# -------------------------------------------------------------------------
group1_med = F
met_base = T

# -------------------------------------------------------------------------

if (group1_med){
  metab_no_bio=readRDS(paste(path, "Results_report/cluster_manual/matched_group1_med_base.rds", sep=""))
  head(metab_no_bio)
  colnames(metab_no_bio)
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/cluster_manual/"
}

if (met_base){
  metab_no_bio  = readRDS(paste(path, "Results_report/updates/cluster_hc_ward_metab_no_bio.rds", sep =""))%>% as.data.frame()
  bb_no_bio = readRDS(paste(path, "Results_report/updates/matched_biobank.rds", sep =""))
  biomarker = readRDS(paste(path, "Results_report/updates/imputed_biomarker.rds",sep =""))
  path_to_results ="/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/"
}



# Denoising biomarkers and metab_no_bio ----------------------------------------------------
### extract potential confounders
confound <- bb_no_bio %>%
  dplyr::select("21022-0.0","31-0.0", "6138-0.0", "20116-0.0","1558-0.0","21001-0.0", "eid")
confound$eid = as.character(confound$eid)

### sort biomarkers and metab_no_bio in order
biomarker = biomarker[order(as.numeric(rownames(biomarker))), ]
metab_no_bio = metab_no_bio[order(as.numeric(row.names(metab_no_bio))), ] 

## merging the biobank confounders and biomarker and and metab_no_bio
biomarker$eid = rownames(biomarker)
biomarker$eid = as.character(biomarker$eid)
metab_no_bio$eid = rownames(metab_no_bio)
metab_no_bio$eid = as.character(metab_no_bio$eid)

## matching the participants name
confound_bio= confound %>% filter(confound$eid %in% biomarker$eid)
confound_metab = confound %>% filter(confound$eid %in% rownames(metab_no_bio))

## merging
bio_con = left_join(confound_bio,biomarker, by = "eid")
metab_bio_con = left_join(confound_metab,metab_no_bio, by = "eid")

## changing type of columns
bio_con$'21022-0.0' = as.numeric(bio_con$'21022-0.0')
metab_bio_con$'21022-0.0' = as.numeric(metab_bio_con$'21022-0.0')

bio_con$'31-0.0' = as.factor(bio_con$'31-0.0')
metab_bio_con$'31-0.0' = as.factor(metab_bio_con$'31-0.0')

bio_con$'20116-0.0' = as.factor(bio_con$'20116-0.0')
metab_bio_con$'20116-0.0' = as.factor(metab_bio_con$'20116-0.0')

bio_con$'1558-0.0' = as.factor(bio_con$'1558-0.0')
metab_bio_con$'1558-0.0' = as.factor(metab_bio_con$'1558-0.0')

bio_con$'21001-0.0' = as.numeric(bio_con$'21001-0.0')
metab_bio_con$'21001-0.0' = as.numeric(metab_bio_con$'21001-0.0')

## sub all spaces of metabolites colnames to _
# metab_con_complete = metab_con_complete %>% rename_all(function(x) gsub(" ", "_", x))

## Set denoise data
bio_con_denoise = bio_con[,8:35]
clus_con_denoise = metab_bio_con[,8:57]

## Denoise metab individual
for (i in 8:ncol(bio_con)){
  model = lm(bio_con[,i]~., data=bio_con[,1:6])
  bio_con_denoise[,i-1] = model$residuals
}

for (i in 8:ncol(metab_bio_con)){
  model = lm(metab_bio_con[,i]~., data =metab_bio_con[,1:6])
  clus_con_denoise[,i-1] = model$residuals
}

# --------------- Save data ----------------------------------------------------------

bio_con_denoise = bio_con_denoise[, 1:28]
bio_con_denoise$eid = bio_con$eid

clus_con_denoise = clus_con_denoise[, 1:50]
clus_con_denoise$eid = metab_bio_con$eid

# Combining biomarkers and metabolites
combine = left_join(bio_con_denoise, clus_con_denoise, by ="eid")
rownames(combine) = combine$eid

rownames(bio_con_denoise) = bio_con_denoise$eid
rownames(clus_con_denoise) = clus_con_denoise$eid


# Remove eid
combine = combine %>% select(!matches("eid"))
bio_con_denoise = bio_con_denoise %>% select(!matches("eid"))
clus_con_denoise = clus_con_denoise %>% select(!matches("eid"))

if (group1_med){saveRDS(combine, paste(path,"Results_report/cluster_manual/combined_group1_med_base.rds", sep=""))}
if (group1_cen){saveRDS(combine, paste(path,"Results_report/cluster_manual/combined_group1_con_base.rds", sep=""))}
if (group2_med){saveRDS(combine, paste(path,"Results_report/cluster_manual/combined_group2_med_base.rds", sep=""))}
if (group2_cen){saveRDS(combine, paste(path,"Results_report/cluster_manual/combined_group2_con_base.rds", sep=""))}
if (met_base){saveRDS(combine, paste(path,"Results_report/updates/combined_biometab.rds", sep=""))}

saveRDS(bio_con_denoise,paste(path,"Results_report/updates/denoised_biomarkers.rds", sep =""))

if (group1_med){saveRDS(clus_con_denoise, paste(path,"Results_report/cluster_manual/denoised_group1_med_base.rds", sep=""))}
if (group1_cen){saveRDS(clus_con_denoise, paste(path,"Results_report/cluster_manual/denoised_group1_con_base.rds", sep=""))}
if (group2_med){saveRDS(clus_con_denoise, paste(path,"Results_report/cluster_manual/denoised_group2_med_base.rds", sep=""))}
if (group2_cen){saveRDS(clus_con_denoise, paste(path,"Results_report/cluster_manual/denoised_group2_con_base.rds", sep=""))}
if (met_base){saveRDS(clus_con_denoise, paste(path,"Results_report/updates/denoised_clus_no_bio.rds", sep=""))}

# ----------------------------Metab and bio correlation---------------------------------------------
combine = readRDS(paste(path, "Results_report/updates/combined_biometab.rds", sep =""))
metab_clus = readRDS(paste(path, "Results_report/updates/denoised_clus_no_bio.rds", sep =""))

mycor = cor(combine)
pdf(paste(path, "Figure_report/Preliminary/heatmap_denoised_biomarkers.pdf",sep=""))
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)
dev.off()

metab = metab%>% filter(rownames(metab) %in% rownames(metab_no_bio))
df= cbind(biomarker[-29], metab)
colnames(df) <- chartr(".", " ", colnames(df))
mycor = cor(df)
pdf(paste(path, "Figure_report/report_no_bio/heatmap_imputed_biomarkers.pdf",sep=""))
pheatmap(mycor, show_rownames = F, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100))
dev.off()


# # Denoising only metab (complete) ----------------------------------------------------
# 
# ### extract potential confounders
# confound <- bb %>%
#   dplyr::select("21022-0.0","31-0.0", "6138-0.0", "20116-0.0","1558-0.0","21001-0.0","eid")
# confound$eid = as.character(confound$eid)
# 
# ### sort metabolites in order
# metab = metab[order(as.numeric(row.names(metab))), ]
# metab$eid = rownames(metab)
# 
# metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ]
# metab_clus$eid = rownames(metab_clus)
# 
# ## merging the biobank confounders and metab
# metab_con = left_join(confound,metab, by = "eid")
# metab_clus_con = left_join(confound,metab_clus, by = "eid")
# 
# ## taking only complete cases
# metab_con_complete <- metab_con[complete.cases(metab_con),] 
# metab_clus_con_complete <- metab_clus_con[complete.cases(metab_clus_con),] 
# 
# ## changing type of columns
# metab_con_complete$'21022-0.0' = as.numeric(metab_con_complete$'21022-0.0')
# metab_clus_con_complete$'21022-0.0'  = as.numeric(metab_clus_con_complete$'21022-0.0')
# 
# metab_con_complete$'31-0.0' = as.factor(metab_con_complete$'31-0.0')
# metab_clus_con_complete$'31-0.0'  = as.factor(metab_clus_con_complete$'31-0.0')
# 
# metab_con_complete$'20116-0.0' = as.factor(metab_con_complete$'20116-0.0')
# metab_clus_con_complete$'20116-0.0'  = as.factor(metab_clus_con_complete$'20116-0.0')
# 
# metab_con_complete$'1558-0.0' = as.factor(metab_con_complete$'1558-0.0')
# metab_clus_con_complete$'1558-0.0'  = as.factor(metab_clus_con_complete$'1558-0.0')
# 
# metab_con_complete$'21001-0.0' = as.numeric(metab_con_complete$'21001-0.0')
# metab_clus_con_complete$'21001-0.0'  = as.numeric(metab_clus_con_complete$'21001-0.0')
# 
# ## sub all spaces of metabolites colnames to _
# # metab_con_complete = metab_con_complete %>% rename_all(function(x) gsub(" ", "_", x))
# 
# ## Set denoise data
# metab_con_denoise = metab_con_complete[,8:175]
# metab_clus_con_denoise = metab_clus_con_complete[,8:56]
# 
# ## Denoise metab individual
# for (i in 8:ncol(metab_con_complete)){
#   model = lm(metab_con_complete[,i]~., data =metab_con_complete[,1:6])
#   metab_con_denoise[,i-1] = model$residuals
# }
# 
# ## Denoise metab clus
# for (i in 8:ncol(metab_clus_con_complete)){
#   model = lm(metab_clus_con_complete[,i]~., data =metab_clus_con_complete[,1:6])
#   metab_clus_con_denoise[,i-1] = model$residuals
# }
# 
# ## Save data
# rownames(metab_con_denoise) = metab_con_complete[,7]
# rownames(metab_clus_con_denoise) = metab_clus_con_complete[,7]
# 
# metab_con_denoise = metab_con_denoise[,1:168]
# metab_clus_con_denoise = metab_clus_con_denoise[,1:49]
# 
# saveRDS(metab_con_denoise,paste(path,"Results_report/denoised_metab.rds", sep =""))
# saveRDS(metab_clus_con_denoise,paste(path,"Results_report/denoised_clus.rds", sep =""))
# 
# 
# 
