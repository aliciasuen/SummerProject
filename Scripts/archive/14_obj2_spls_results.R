### Summmer Project -- Objective 1 : Multivariate - sPLS 
### 23rd June 2021 - Alicia 


# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))

LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}
#if (!requireNamespace("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")
library(BiocManager)
# BiocManager::install("mixOmics")
LoadPackages(c("pheatmap","corpcor","abind","parallel",
               "RColorBrewer","ppcor","mvtnorm",
               "pROC","stabs","huge","pulsar",
               "QUIC","igraph","glasso","glassoFast",
               "colorspace","glmnet", "tidyverse","sgPLS","mixOmics","dplyr", "focus"))


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

## -----------------------------------------------------------------------------------------------------------------------
met_no_bio = T
met_base = F
met_first= F
## -----------------------------------------------------------------------------------------------------------------------
if(met_no_bio){
  biobank=readRDS(paste(path, "Results_report/updates/matched_biobank.rds", sep=""))
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_biomarkers.rds", sep="")) %>% as.matrix()
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  combine = readRDS(paste(path, "Results_report/updates/combined_biometab.rds", sep=""))
  combine = combine[order(as.numeric(row.names(combine))),] %>% as.matrix()
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj2/Air_clus_bio/sPLS/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj2/Air_clus_bio/sPLS/"
  air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
  colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")
}

if(met_base){
  biobank=readRDS(paste(path, "Results_report/matched_bb_base.rds", sep=""))
  metab = readRDS(paste(path, "Results_report/denoised_metab.rds", sep=""))
  metab = metab[order(as.numeric(row.names(metab))), ] %>% as.matrix()
  metab_clus = readRDS(paste(path, "Results_report/denoised_clus.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj1/Air_met_base/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj1/Air_met_base/"
  air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
  colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")
}

if(met_first){
  biobank=readRDS(paste(path, "Results_report/matched_bb_first.rds", sep=""))
  metab = readRDS(paste(path, "Results_report/matched_metab_first.rds", sep=""))
  metab = metab %>% select(!matches("eid_52569")) %>% data.frame()
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj1/Air_met_first/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj1/Air_met_first/"
  air = select(biobank, "24003-0.0":"24008-0.0")
  colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10")
}

# source(paste(path,"Functions/penalisation_functions.R", sep=""))
# source(paste(path,"Functions/pls_functions.R", sep=""))

air = air[,1:6]


# ----------------------Clustered metabolites--------------------------------------------------

out = VariableSelection(xdata = metab_clus, ydata = air, Lambda=1:ncol(metab_clus), PFER_thr =20 , PFER_method = "MB",
                          implementation=SparsePLS, K=100)

print(Argmax(out))

# Save plots
pdf(paste(path_to_save,"CalibrationPlot/cali_spls_metab_clus_base.pdf",sep = ""), height=5, width=12)
CalibrationPlot(out)
dev.off()

# Calibrated selection proportions 
selprop=SelectionProportions(out)

# Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})

saveRDS(out, paste(path_to_Results_report, "out_spls_metab_clus_base.rds", sep=""))
saveRDS(selprop, paste(path_to_Results_report, "selprop_spls_metab_clus_base.rds", sep=""))
saveRDS(average_load.1, paste(path_to_Results_report, "load_metab_clus_base.rds", sep =""))

# --------------------Biomarkers-----------------------------------------------------

out = VariableSelection(xdata = biomarker, ydata = air, Lambda=1:ncol(biomarker),
                        implementation=SparsePLS, K=100)
print(Argmax(out))
# Save plots
pdf(paste(path_to_save,"CalibrationPlot/cali_spls_bio.pdf",sep = ""), height=5, width=12)
CalibrationPlot(out)
dev.off()

# Calibrated selection proportions
selprop=SelectionProportions(out)

# Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})

saveRDS(out, paste(path_to_Results_report, "out_spls_bio.rds", sep=""))
saveRDS(selprop, paste(path_to_Results_report, "selprop_spls_bio.rds", sep=""))
saveRDS(average_load.1, paste(path_to_Results_report, "load_bio.rds", sep =""))


# --------------------Combine-----------------------------------------------------

out = VariableSelection(xdata = combine, ydata = air, Lambda=1:ncol(combine),
                        implementation=SparsePLS, K=100)
print(Argmax(out))
# Save plots
pdf(paste(path_to_save,"CalibrationPlot/cali_spls_combine.pdf",sep = ""), height=5, width=12)
CalibrationPlot(out)
dev.off()

# Calibrated selection proportions
selprop=SelectionProportions(out)

# Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})

saveRDS(out, paste(path_to_Results_report, "out_spls_combine.rds", sep=""))
saveRDS(selprop, paste(path_to_Results_report, "selprop_spls_combine.rds", sep=""))
saveRDS(average_load.1, paste(path_to_Results_report, "load_combine.rds", sep =""))




# # --------------------Individual Metabolites-----------------------------------------------------
# 
# out = VariableSelection(xdata = metab, ydata = air, Lambda=1:ncol(metab),
#                           implementation=SparsePLS, K=100)
# print(Argmax(out))
# # Save plots
# pdf(paste(path_to_save,"CalibrationPlot/cali_spls_metab_base.pdf",sep = ""), height=5, width=12)
# CalibrationPlot(out)
# dev.off()
# 
# # Calibrated selection proportions 
# selprop=SelectionProportions(out)
# 
# # Extracting ID of calibrated lambda
# hat_lambda_id.1=ArgmaxId(out)[1]
# 
# # Computing average beta coefficients from models with calibrated lambda
# average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# 
# saveRDS(out, paste(path_to_Results_report, "out_spls_metab_base.rds", sep=""))
# saveRDS(selprop, paste(path_to_Results_report, "selprop_spls_metab_base.rds", sep=""))
# saveRDS(average_load.1, paste(path_to_Results_report, "load_metab_base.rds", sep =""))

