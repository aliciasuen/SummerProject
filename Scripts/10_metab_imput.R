### Summmer Project -- Metabolomics Data Imputaion
### 31st May 2021 - Alicia edited on 11th June

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))


# Load packages
library(openxlsx)
library(data.table)
library("plyr") 
library(dplyr)
library(imputeLCMD)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
path_to_results =  "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/updates/"

# Load datasets
metab_base = readRDS(paste(path, "Results/updates/matched_metab_base.rds", sep=""))
biomarker = readRDS(paste(path, "Results/updates/matched_biomarker.rds", sep =""))
rownames(biomarker) = biomarker$eid
biomarker = biomarker[,-1]
metab_train70 = readRDS(paste(path, "Results/updates/split/train70_metab.rds", sep =""))
metab_train70 = metab_train70[,-161]
metab_test30 = readRDS(paste(path, "Results/updates/split/test30_metab.rds", sep =""))
metab_test30 = metab_test30[,-161]
bio_train70 = readRDS(paste(path, "Results/updates/split/train70_biomarker.rds", sep =""))
bio_train70 = bio_train70[,-1]
bio_test30 = readRDS(paste(path, "Results/updates/split/test30_biomarker.rds", sep =""))
bio_test30 = bio_test30[,-1]


### Imputation using impute.QRILC
set.seed(123)
metab_base_imp=impute.QRILC(t(metab_base))
metab_base_imp=as.data.frame(t(metab_base_imp[[1]]))

set.seed(123)
bio_imp=impute.QRILC(t(biomarker))
bio_imp=as.data.frame(t(bio_imp[[1]]))

set.seed(123)
metab_train70_imp=impute.QRILC(t(metab_train70))
metab_train70_imp=as.data.frame(t(metab_train70_imp[[1]]))

set.seed(123)
metab_test30_imp=impute.QRILC(t(metab_test30))
metab_test30_imp=as.data.frame(t(metab_test30_imp[[1]]))

set.seed(123)
bio_train70_imp=impute.QRILC(t(bio_train70))
bio_train70_imp=as.data.frame(t(bio_train70_imp[[1]]))

set.seed(123)
bio_test30_imp=impute.QRILC(t(bio_test30))
bio_test30_imp=as.data.frame(t(bio_test30_imp[[1]]))

## Save
saveRDS(metab_base_imp, paste(path_to_results,"imputed_metab_base.rds",sep = ""))
saveRDS(bio_imp, paste(path_to_results,"imputed_biomarker.rds",sep = ""))
saveRDS(metab_train70_imp, paste(path_to_results,"split/imputed_metab_train70.rds",sep = ""))
saveRDS(metab_test30_imp, paste(path_to_results,"split/imputed_metab_test30.rds",sep = ""))
saveRDS(bio_train70_imp, paste(path_to_results,"split/imputed_biomarker_train70.rds",sep = ""))
saveRDS(bio_test30_imp, paste(path_to_results,"split/imputed_biomarker_test30.rds",sep = ""))





# 
# pdf(paste(path,"Figures/Preliminary/heatmap_metab.pdf", sep=""))
# pheatmap(cor(metab, method = "pearson"), border = NA, show_rownames = T, show_colnames = F,
#          breaks = seq(-1, 1, length.out = 100))
# dev.off()
# pdf(paste(path,"Figures/Preliminary/heatmap_metab.pdf", sep=""), width = 10, height = 7)
# pheatmap(cor(metab, method = "pearson"), border = NA, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
#          breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
# dev.off()
# 

