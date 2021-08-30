### Summmer Project -- Objective 2 : Multi-response sPLS (using BiSelection) - sparsity on both X and Y
### 27thh July 2021 - Alicia 


## Looking at metabolites as Y (outcome) and air exposure as X (predictors)

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
group1_med = F
group1_cen = F
group2_med = F
group2_cen = F
met_no_bio = T
met_base = F
met_first= F
## -----------------------------------------------------------------------------------------------------------------------
if (group1_med){
  metab_clus=readRDS(paste(path, "Results/cluster_manual/denoised_group1_med_base.rds", sep=""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  combine = readRDS(paste(path, "Results/cluster_manual/combined_group1_med_base.rds", sep =""))
  combine = combine[order(as.numeric(row.names(combine))),] %>% as.matrix()
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/cluster_manual/group1_med/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/cluster_manual/group1_med/"
}

if (group1_cen){
  metab_clus=readRDS(paste(path, "Results/cluster_manual/denoised_group1_con_base.rds", sep=""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  combine = readRDS(paste(path, "Results/cluster_manual/combined_group1_con_base.rds", sep =""))
  combine = combine[order(as.numeric(row.names(combine))),] %>% as.matrix()
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/cluster_manual/group1_cen/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/cluster_manual/group1_cen/"
}

if (group2_med){
  metab_clus=readRDS(paste(path, "Results/cluster_manual/denoised_group2_med_base.rds", sep=""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  combine = readRDS(paste(path, "Results/cluster_manual/combined_group2_med_base.rds", sep =""))
  combine = combine[order(as.numeric(row.names(combine))),] %>% as.matrix()
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/cluster_manual/group2_med/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/cluster_manual/group2_med/"
}

if (group2_cen){
  metab_clus=readRDS(paste(path, "Results/cluster_manual/denoised_group2_con_base.rds", sep=""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  combine = readRDS(paste(path, "Results/cluster_manual/combined_group2_con_base.rds", sep =""))
  combine = combine[order(as.numeric(row.names(combine))),] %>% as.matrix()
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/cluster_manual/group2_cen/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/cluster_manual/group2_cen/"
}

if(met_no_bio){
  biobank=readRDS(paste(path, "Results/biobank_final.rds", sep=""))
  biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep="")) %>% as.matrix()
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  combine = readRDS(paste(path, "Results/combined_biometab.rds", sep=""))
  combine = combine[order(as.numeric(row.names(combine))),] %>% as.matrix()
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/Air_clus_bio/sensitivity_XY_reverse/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/Air_clus_bio/sensitivity_XY_reverse/"
  air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
  colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")
}

if(met_base){
  biobank=readRDS(paste(path, "Results/matched_bb_base.rds", sep=""))
  metab = readRDS(paste(path, "Results/denoised_metab.rds", sep=""))
  metab = metab[order(as.numeric(row.names(metab))), ] %>% as.matrix()
  metab_clus = readRDS(paste(path, "Results/denoised_clus.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_met_base/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/Air_met_base/"
  air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
  colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")
}

if(met_first){
  biobank=readRDS(paste(path, "Results/matched_bb_first.rds", sep=""))
  metab = readRDS(paste(path, "Results/matched_metab_first.rds", sep=""))
  metab = metab %>% select(!matches("eid_52569")) %>% data.frame()
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_met_first/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/Air_met_first/"
  air = select(biobank, "24003-0.0":"24008-0.0")
  colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10")
}

# source(paste(path,"Functions/penalisation_functions.R", sep=""))
# source(paste(path,"Functions/pls_functions.R", sep=""))

#### Loading biomarker and biobank datasets ####
biobank=readRDS(paste(path, "Results/biobank_final.rds", sep=""))
biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep="")) %>% as.matrix()
air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")
air = air[,1:6]


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
# saveRDS(out, paste(path_to_results, "out_spls_metab_base.rds", sep=""))
# saveRDS(selprop, paste(path_to_results, "selprop_spls_metab_base.rds", sep=""))
# saveRDS(average_load.1, paste(path_to_results, "load_metab_base.rds", sep =""))

# # ----------------------Clustered metabolites--------------------------------------------------
# ## Sparsity only on X ##
# out = BiSelection(xdata = air, ydata = metab_clus, LambdaX=1:ncol(air),
#                           implementation=SparsePLS, K=100)
# 
# # Calibrated selection proportions
# selprop=out$selpropX
# 
# # # Extracting ID of calibrated lambda
# # hat_lambda_id.1=ArgmaxId(out)[1]
# #
# # # Computing average beta coefficients from models with calibrated lambda
# # average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# 
# saveRDS(out, paste(path_to_results, "sparsityX/out_spls_metab_clus_base.rds", sep=""))
# saveRDS(selprop, paste(path_to_results, "sparsityX/selprop_spls_metab_clus_base.rds", sep=""))
# # saveRDS(average_load.1, paste(path_to_results, "load_metab_clus_base.rds", sep =""))
# 
# ## Sparsity only on X and Y ##
# out = BiSelection(xdata = air, ydata = metab_clus, LambdaX=1:ncol(air),  LambdaY=1:(ncol(metab_clus)-1),
#                         implementation=SparsePLS, K=10)
# 
# # Calibrated selection proportions
# selpropX=out$selpropX
# selpropY=out$selpropY
# 
# # # Extracting ID of calibrated lambda
# # hat_lambda_id.1=ArgmaxId(out)[1]
# #
# # # Computing average beta coefficients from models with calibrated lambda
# # average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# 
# saveRDS(out, paste(path_to_results, "sparsityXY/out_spls_metab_clus_base.rds", sep=""))
# saveRDS(selpropX, paste(path_to_results, "sparsityXY/selpropX_spls_metab_clus_base.rds", sep=""))
# saveRDS(selpropY, paste(path_to_results, "sparsityXY/selpropY_spls_metab_clus_base.rds", sep=""))
# 
# # saveRDS(average_load.1, paste(path_to_results, "load_metab_clus_base.rds", sep =""))
# 
# # --------------------Biomarkers-----------------------------------------------------
# 
# ## Sparsity on X ##
# out = BiSelection(xdata = air, ydata = biomarker, LambdaX=1:ncol(air),
#                         implementation=SparsePLS, K=100)
# 
# 
# # Calibrated selection proportions
# selprop=out$selpropX
# 
# # # Extracting ID of calibrated lambda
# # hat_lambda_id.1=ArgmaxId(out)[1]
# #
# # # Computing average beta coefficients from models with calibrated lambda
# # average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# 
# saveRDS(out, paste(path_to_results, "sparsityX/out_spls_bio.rds", sep=""))
# saveRDS(selprop, paste(path_to_results, "sparsityX/selprop_spls_bio.rds", sep=""))
# # saveRDS(average_load.1, paste(path_to_results, "load_bio.rds", sep =""))
# 
# ## Sparsity on X and Y  ##
# out = BiSelection(xdata = air, ydata = biomarker, LambdaX=1:ncol(air),LambdaY=1:(ncol(biomarker)-1),
#                   implementation=SparsePLS, K=100)
# 
# 
# # Calibrated selection proportions
# selpropX=out$selpropX
# selpropY=out$selpropY
# 
# # # Extracting ID of calibrated lambda
# # hat_lambda_id.1=ArgmaxId(out)[1]
# #
# # # Computing average beta coefficients from models with calibrated lambda
# # average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# 
# saveRDS(out, paste(path_to_results, "sparsityXY/out_spls_bio.rds", sep=""))
# saveRDS(selpropX, paste(path_to_results, "sparsityXY/selpropX_spls_bio.rds", sep=""))
# saveRDS(selpropY, paste(path_to_results, "sparsityXY/selpropY_spls_bio.rds", sep=""))
# 
# # saveRDS(average_load.1, paste(path_to_results, "load_bio.rds", sep =""))
# # --------------------Combine-----------------------------------------------------
# 
# ## Sparsity on X ##
# out = BiSelection(xdata = air, ydata = combine, LambdaX=1:ncol(air),
#                         implementation=SparsePLS, K=100)
# 
# 
# # Calibrated selection proportions
# selprop=out$selpropX
# 
# # # Extracting ID of calibrated lambda
# # hat_lambda_id.1=ArgmaxId(out)[1]
# #
# # # Computing average beta coefficients from models with calibrated lambda
# # average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# 
# saveRDS(out, paste(path_to_results, "sparsityX/out_spls_combine.rds", sep=""))
# saveRDS(selprop, paste(path_to_results, "sparsityX/selprop_spls_combine.rds", sep=""))
# # saveRDS(average_load.1, paste(path_to_results, "load_combine.rds", sep =""))
# 
# ## Sparsity on X and Y ##
# out = BiSelection(xdata = air, ydata = combine, LambdaX=1:ncol(air),LambdaY=1:(ncol(combine)-1),
#                         implementation=SparsePLS, K=100)
# 
# 
# # Calibrated selection proportions
# selpropX=out$selpropX
# selpropY=out$selpropY
# 
# # # Extracting ID of calibrated lambda
# # hat_lambda_id.1=ArgmaxId(out)[1]
# #
# # # Computing average beta coefficients from models with calibrated lambda
# # average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# 
# saveRDS(out, paste(path_to_results, "sparsityXY/out_spls_combine.rds", sep=""))
# saveRDS(selpropX, paste(path_to_results, "sparsityXY/selpropX_spls_combine.rds", sep=""))
# saveRDS(selpropY, paste(path_to_results, "sparsityXY/selpropY_spls_combine.rds", sep=""))
# 
# # saveRDS(average_load.1, paste(path_to_results, "load_combine.rds", sep =""))
# 
# 
# 
# # ------------------Checking number of components 3 for sparsity on Y-------------------------------------------------------
# #### Clustered metabolites ####
# ## Sparsity only on X and Y ##
# out = BiSelection(xdata = air, ydata = metab_clus, LambdaX=1:ncol(air),  LambdaY=1:(ncol(metab_clus)-1),ncomp = 3,
#                         implementation=SparsePLS, K=100)
# 
# # Calibrated selection proportions
# selpropX_1=out$selpropX[1,]
# selpropX_2=out$selpropX[2,]
# selpropX_3=out$selpropX[3,]
# selpropY_1=out$selpropY[1,]
# selpropY_2=out$selpropY[2,]
# selpropY_3=out$selpropY[3,]
# 
# # # Extracting ID of calibrated lambda
# # hat_lambda_id.1=ArgmaxId(out)[1]
# #
# # # Computing average beta coefficients from models with calibrated lambda
# # average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# 
# saveRDS(out, paste(path_to_results, "sparsityXY_ncomp3/out_spls_metab_clus_base.rds", sep=""))
# saveRDS(selpropX_1, paste(path_to_results, "sparsityXY_ncomp3/selpropX_1_spls_metab_clus_base.rds", sep=""))
# saveRDS(selpropX_2, paste(path_to_results, "sparsityXY_ncomp3/selpropX_2_spls_metab_clus_base.rds", sep=""))
# saveRDS(selpropX_3, paste(path_to_results, "sparsityXY_ncomp3/selpropX_3_spls_metab_clus_base.rds", sep=""))
# 
# saveRDS(selpropY_1, paste(path_to_results, "sparsityXY_ncomp3/selpropY_1_spls_metab_clus_base.rds", sep=""))
# saveRDS(selpropY_2, paste(path_to_results, "sparsityXY_ncomp3/selpropY_2_spls_metab_clus_base.rds", sep=""))
# saveRDS(selpropY_3, paste(path_to_results, "sparsityXY_ncomp3/selpropY_3_spls_metab_clus_base.rds", sep=""))
# 
# # saveRDS(average_load.1, paste(path_to_results, "load_metab_clus_base.rds", sep =""))
# 
# #### Biomarkers  ####
# out = BiSelection(xdata = air, ydata = biomarker, LambdaX=1:ncol(air),LambdaY=1:(ncol(biomarker)-1),ncomp=3,
#                   implementation=SparsePLS, K=100)
# 
# 
# # Calibrated selection proportions
# selpropX_1=out$selpropX[1,]
# selpropX_2=out$selpropX[2,]
# selpropX_3=out$selpropX[3,]
# selpropY_1=out$selpropY[1,]
# selpropY_2=out$selpropY[2,]
# selpropY_3=out$selpropY[3,]
# 
# # # Extracting ID of calibrated lambda
# # hat_lambda_id.1=ArgmaxId(out)[1]
# #
# # # Computing average beta coefficients from models with calibrated lambda
# # average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# 
# saveRDS(out, paste(path_to_results, "sparsityXY_ncomp3/out_spls_bio_base.rds", sep=""))
# saveRDS(selpropX_1, paste(path_to_results, "sparsityXY_ncomp3/selpropX_1_spls_bio_base.rds", sep=""))
# saveRDS(selpropX_2, paste(path_to_results, "sparsityXY_ncomp3/selpropX_2_spls_bio_base.rds", sep=""))
# saveRDS(selpropX_3, paste(path_to_results, "sparsityXY_ncomp3/selpropX_3_spls_bio_base.rds", sep=""))
# 
# saveRDS(selpropY_1, paste(path_to_results, "sparsityXY_ncomp3/selpropY_1_spls_bio_base.rds", sep=""))
# saveRDS(selpropY_2, paste(path_to_results, "sparsityXY_ncomp3/selpropY_2_spls_bio_base.rds", sep=""))
# saveRDS(selpropY_3, paste(path_to_results, "sparsityXY_ncomp3/selpropY_3_spls_bio_base.rds", sep=""))
# 
# # saveRDS(average_load.1, paste(path_to_results, "load_bio.rds", sep =""))

#### Combined  ####
out = BiSelection(xdata = air, ydata = combine, LambdaX=1:ncol(air),LambdaY=1:(ncol(combine)-1),ncomp = 3,
                        implementation=SparsePLS, K=100, verbose = FALSE)


# Calibrated selection proportions
selpropX_1=out$selpropX[1,]
selpropX_2=out$selpropX[2,]
selpropX_3=out$selpropX[3,]
selpropY_1=out$selpropY[1,]
selpropY_2=out$selpropY[2,]
selpropY_3=out$selpropY[3,]

# # Extracting ID of calibrated lambda
# hat_lambda_id.1=ArgmaxId(out)[1]
#
# # Computing average beta coefficients from models with calibrated lambda
# average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})

saveRDS(out, paste(path_to_results, "sparsityXY_ncomp3/out_spls_combine_base.rds", sep=""))
saveRDS(selpropX_1, paste(path_to_results, "sparsityXY_ncomp3/selpropX_1_spls_combine_base.rds", sep=""))
saveRDS(selpropX_2, paste(path_to_results, "sparsityXY_ncomp3/selpropX_2_spls_combine_base.rds", sep=""))
saveRDS(selpropX_3, paste(path_to_results, "sparsityXY_ncomp3/selpropX_3_spls_combine_base.rds", sep=""))

saveRDS(selpropY_1, paste(path_to_results, "sparsityXY_ncomp3/selpropY_1_spls_combine_base.rds", sep=""))
saveRDS(selpropY_2, paste(path_to_results, "sparsityXY_ncomp3/selpropY_2_spls_combine_base.rds", sep=""))
saveRDS(selpropY_3, paste(path_to_results, "sparsityXY_ncomp3/selpropY_3_spls_combine_base.rds", sep=""))

# saveRDS(average_load.1, paste(path_to_results, "load_combine.rds", sep =""))
