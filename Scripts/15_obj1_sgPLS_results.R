### Summmer Project -- Objective 1 : Multivariate - sgPLS stability selection on metabolites and air pollutants
### 21st July 2021 - Alicia 

## Q: Which metabolites are statistically correlated with which exposures in the data?
## --> Multivariate LASSO regression on each exposure 

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))

# devtools::install("../Functions/focus-master")

LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}
LoadPackages(c("pheatmap","corpcor","abind","parallel",
               "RColorBrewer","ppcor","mvtnorm",
               "pROC","stabs","huge","pulsar",
               "QUIC","igraph","glasso","glassoFast",
               "colorspace","glmnet","dplyr","tidyverse","devtools", "focus"))


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

## -----------------------------------------------------------------------------------------------------------------------
group1 = T
group2 = F

## -----------------------------------------------------------------------------------------------------------------------
if (group1){
  annot=read_csv(paste(path, "Dictionaries/cluster_manually_update.csv", sep=""))
  annot = annot[,1:2]
  colnames(annot) = c("features","Grouping")
  annot$Grouping = factor(annot$Grouping) 
  annot$Grouping = as.numeric(annot$Grouping)
  annot = annot %>% arrange(Grouping)
  grouping = count(annot)
  grouping_num = grouping$n
  metab = readRDS(paste(path, "Results/denoised_metab_no_bio.rds", sep=""))
  ## reordering the colnames ##
  index <- match(annot$features, colnames(metab))
  metab_reordered  <- metab[ , index] 
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/sgPLS/group1/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/sgPLS/group1/"
}

if (group2){
  annot=read_csv(paste(path, "Dictionaries/cluster_manually.csv", sep="")) %>% as.data.frame()
  annot = annot[,c(1,3)]
  colnames(annot) = c("features","Grouping_2")
  annot$Grouping_2 = factor(annot$Grouping_2) 
  annot$Grouping_2 = as.numeric(annot$Grouping_2)
  annot = annot %>% arrange(Grouping_2)
  grouping = count(annot$Grouping_2)
  grouping_num = grouping$freq
  metab = readRDS(paste(path, "Results/denoised_metab.rds", sep=""))
  ## reordering the colnames ##
  index <- match(annot$features, colnames(metab))
  metab_reordered  <- metab[ , index] 
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/sgPLS/group2/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/sgPLS/group2/"
}

source(paste(path,"Functions/penalisation_functions.R", sep=""))
# myfunctions=list.files("/rds/general/project/hda_students_data/live/Group2/project/data_analysis/cox/functions/R")
# for (k in 1:length(myfunctions)){
#   source(paste0("/rds/general/project/hda_students_data/live/Group2/project/data_analysis/cox/functions/R/",myfunctions[k]))  
# }

#### Loading biomarker and biobank datasets ####
biobank=readRDS(paste(path, "Results/matched_bb_no_bio_base.rds", sep=""))
metab_reordered = metab_reordered %>% filter(rownames(metab_reordered) %in% biobank$eid)
metab_reordered = metab_reordered %>% select(!matches("Glycoprotein Acetyls"))
metab_reordered = as.matrix(metab_reordered)
biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep="")) %>% as.matrix()
air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")


# # ------------------Clustered Metabolites Stability sgPLS (group1)-------------------------------------------------------
NO2 = as.matrix(air$NO2)
# PM2.5 = as.matrix(air$PM2.5)
# PM10 = as.matrix(air$PM10)
# NOx = as.matrix(air$NOx)
# PM_abs = as.matrix(air$PM2.5_absorbance)
# PM2.5_10 = as.matrix(air$`PM2.5-PM10`)
# air$MajorRoad = ifelse(air$MajorRoad == "No", 0,
#                        ifelse(air$MajorRoad == "Yes", 1, NA ))
# MajorRoad = as.matrix(air$MajorRoad)

## NO2 ##

out = VariableSelection(xdata = metab_reordered, ydata = NO2,
                  LambdaX=1:ncol(metab_reordered),
                  group_x = c(10,2,7,4,7,9,2,4,4,7,4,7,7,7,4,3,7,7,7,4,4,7,7,7,4,4,7,7,7),
                  alpha.x = seq(0.1, 0.9, by = 0.1),
                  implementation=SparseGroupPLS, K=100,family = "gaussian")

selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_metab_clus_NO2.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_NO2.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/clus_NO2.pdf",sep =""))
CalibrationPlot(out)
dev.off()
# 
# ## PM2.5 ##
# out = VariableSelection(xdata = metab_reordered, ydata = PM2.5, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,7,4,7,9,2,4,4,7,4,7,7,7,4,3,7,7,7,4,4,7,7,7,4,4,7,7,7),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_PM2.5.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_PM2.5.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_PM2.5.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM10 ##
# out = VariableSelection(xdata = metab_reordered, ydata = PM10, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,7,4,7,9,2,4,4,7,4,7,7,7,4,3,7,7,7,4,4,7,7,7,4,4,7,7,7),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_PM10.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_PM10.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_PM10.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## NOx ##
# out = VariableSelection(xdata = metab_reordered, ydata = NOx, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,7,4,7,9,2,4,4,7,4,7,7,7,4,3,7,7,7,4,4,7,7,7,4,4,7,7,7),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_NOx.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_NOx.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_NOx.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM_abs ##
# out = VariableSelection(xdata = metab_reordered, ydata = PM_abs, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,7,4,7,9,2,4,4,7,4,7,7,7,4,3,7,7,7,4,4,7,7,7,4,4,7,7,7),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_PM_abs.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_PM_abs.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_PM_abs.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM2.5_10 ##
# out = VariableSelection(xdata = metab_reordered, ydata = PM2.5_10, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,7,4,7,9,2,4,4,7,4,7,7,7,4,3,7,7,7,4,4,7,7,7,4,4,7,7,7),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_PM2.5_10.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_PM2.5_10.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_PM2.5_10.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## Major Road ##
# out = VariableSelection(xdata = metab_reordered, ydata = MajorRoad, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,7,4,7,9,2,4,4,7,4,7,7,7,4,3,7,7,7,4,4,7,7,7,4,4,7,7,7),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "binomial")
# selprop=SelectionProportions(out)
# saveRDS(out, paste(path_to_results, "out_metab_clus_road.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_road.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_road.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()


# ------------------Clustered Metabolites (group1) Betas parameters extraction-------------------------------------------------------
NO2 <- readRDS(paste(path_to_results, "out_metab_clus_NO2.rds",sep = ""))
# PM10 <- readRDS(paste(path_to_results, "out_metab_clus_PM10.rds",sep = ""))
# PM2.5 <- readRDS(paste(path_to_results, "out_metab_clus_PM2.5.rds",sep = ""))
# NOx <- readRDS(paste(path_to_results, "out_metab_clus_NOx.rds",sep = ""))
# PM_abs <- readRDS(paste(path_to_results, "out_metab_clus_PM_abs.rds",sep = ""))
# PM2.5_10 <- readRDS(paste(path_to_results, "out_metab_clus_PM2.5_10.rds",sep = ""))
# MajorRoad <- readRDS(paste(path_to_results, "out_metab_clus_road.rds",sep = ""))

# Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(NO2)[1]
# hat_lambda_id.2=ArgmaxId(PM10)[1]
# hat_lambda_id.3=ArgmaxId(PM2.5)[1]
# hat_lambda_id.4=ArgmaxId(NOx)[1]
# hat_lambda_id.5=ArgmaxId(PM_abs)[1]
# hat_lambda_id.6=ArgmaxId(PM2.5_10)[1]
# hat_lambda_id.7=ArgmaxId(MajorRoad)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.2=apply(PM10$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.3=apply(PM2.5$Beta[hat_lambda_id.3,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.4=apply(NOx$Beta[hat_lambda_id.4,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.5=apply(PM_abs$Beta[hat_lambda_id.5,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.6=apply(PM2.5_10$Beta[hat_lambda_id.6,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.7=apply(MajorRoad$Beta[hat_lambda_id.7,,],1,FUN=function(x){mean(x[x!=0])})

# Save
saveRDS(average_load.1,paste(path_to_results,"betas_stability_clus_NO2.rds", sep =""))
# saveRDS(average_load.2,paste(path_to_results,"betas_stability_clus_PM10.rds", sep =""))
# saveRDS(average_load.3,paste(path_to_results,"betas_stability_clus_PM2.5.rds", sep =""))
# saveRDS(average_load.4,paste(path_to_results,"betas_stability_clus_NOx.rds", sep =""))
# saveRDS(average_load.5,paste(path_to_results,"betas_stability_clus_PM_abs.rds", sep =""))
# saveRDS(average_load.6,paste(path_to_results,"betas_stability_clus_PM2.5_10.rds", sep =""))
# saveRDS(average_load.7,paste(path_to_results,"betas_stability_clus_road.rds", sep =""))

group1 = T
group2 = F

# # # ------------------Clustered Metabolites Stability sgPLS (group2)-------------------------------------------------------
NO2 = as.matrix(air$NO2)
# PM2.5 = as.matrix(air$PM2.5)
# PM10 = as.matrix(air$PM10)
# NOx = as.matrix(air$NOx)
# PM_abs = as.matrix(air$PM2.5_absorbance)
# PM2.5_10 = as.matrix(air$`PM2.5-PM10`)
# air$MajorRoad = ifelse(air$MajorRoad == "No", 0,
#                        ifelse(air$MajorRoad == "Yes", 1, NA ))
# MajorRoad = as.matrix(air$MajorRoad)
# 
## NO2 ##

out = VariableSelection(xdata = metab_reordered, ydata = NO2,
                        LambdaX=1:ncol(metab_reordered),
                        group_x = c(10,2,21,18,9,2,18,4,1,18,3,4,17,17,18,5),
                        alpha.x = seq(0.1, 0.9, by = 0.1),
                        implementation=SparseGroupPLS, K=100,family = "gaussian")

selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_metab_clus_NO2.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_NO2.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/clus_NO2.pdf",sep =""))
CalibrationPlot(out)
dev.off()
# 
# ## PM2.5 ##
# out = VariableSelection(xdata = metab_reordered, ydata = PM2.5, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,20,17,9,2,17,4,4,18,3,4,17,17,18,5),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_PM2.5.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_PM2.5.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_PM2.5.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM10 ##
# out = VariableSelection(xdata = metab_reordered, ydata = PM10, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,20,17,9,2,17,4,4,18,3,4,17,17,18,5),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_PM10.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_PM10.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_PM10.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## NOx ##
# out = VariableSelection(xdata = metab_reordered, ydata = NOx, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,20,17,9,2,17,4,4,18,3,4,17,17,18,5),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_NOx.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_NOx.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_NOx.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM_abs ##
# out = VariableSelection(xdata = metab_reordered, ydata = PM_abs, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,20,17,9,2,17,4,4,18,3,4,17,17,18,5),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_PM_abs.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_PM_abs.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_PM_abs.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM2.5_10 ##
# out = VariableSelection(xdata = metab_reordered, ydata = PM2.5_10, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,20,17,9,2,17,4,4,18,3,4,17,17,18,5),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=100,family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_PM2.5_10.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_PM2.5_10.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_PM2.5_10.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## Major Road ##
# out = VariableSelection(xdata = metab_reordered, ydata = road, 
#                         LambdaX=1:ncol(metab_reordered),
#                         group_x = c(10,2,20,17,9,2,17,4,4,18,3,4,17,17,18,5),
#                         alpha.x = seq(0.1, 0.9, by = 0.1),
#                         implementation=SparseGroupPLS, K=10,family = "binomial")
# selprop=SelectionProportions(out)
# saveRDS(out, paste(path_to_results, "out_metab_clus_road.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_road.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_road.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# 
# # ------------------Clustered Metabolites (group2) Betas parameters extraction-------------------------------------------------------
NO2 <- readRDS(paste(path_to_results, "out_metab_clus_NO2.rds",sep = ""))
# PM10 <- readRDS(paste(path_to_results, "out_metab_clus_PM10.rds",sep = ""))
# PM2.5 <- readRDS(paste(path_to_results, "out_metab_clus_PM2.5.rds",sep = ""))
# NOx <- readRDS(paste(path_to_results, "out_metab_clus_NOx.rds",sep = ""))
# PM_abs <- readRDS(paste(path_to_results, "out_metab_clus_PM_abs.rds",sep = ""))
# PM2.5_10 <- readRDS(paste(path_to_results, "out_metab_clus_PM2.5_10.rds",sep = ""))
# MajorRoad <- readRDS(paste(path_to_results, "out_metab_clus_road.rds",sep = ""))
# 
# # Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(NO2)[1]
# hat_lambda_id.2=ArgmaxId(PM10)[1]
# hat_lambda_id.3=ArgmaxId(PM2.5)[1]
# hat_lambda_id.4=ArgmaxId(NOx)[1]
# hat_lambda_id.5=ArgmaxId(PM_abs)[1]
# hat_lambda_id.6=ArgmaxId(PM2.5_10)[1]
# hat_lambda_id.7=ArgmaxId(MajorRoad)[1]
# 
# # Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.2=apply(PM10$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.3=apply(PM2.5$Beta[hat_lambda_id.3,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.4=apply(NOx$Beta[hat_lambda_id.4,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.5=apply(PM_abs$Beta[hat_lambda_id.5,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.6=apply(PM2.5_10$Beta[hat_lambda_id.6,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.7=apply(MajorRoad$Beta[hat_lambda_id.7,,],1,FUN=function(x){mean(x[x!=0])})
# 
# # Save
saveRDS(average_load.1,paste(path_to_results,"betas_stability_clus_NO2.rds", sep =""))
# saveRDS(average_load.2,paste(path_to_results,"betas_stability_clus_PM10.rds", sep =""))
# saveRDS(average_load.3,paste(path_to_results,"betas_stability_clus_PM2.5.rds", sep =""))
# saveRDS(average_load.4,paste(path_to_results,"betas_stability_clus_NOx.rds", sep =""))
# saveRDS(average_load.5,paste(path_to_results,"betas_stability_clus_PM_abs.rds", sep =""))
# saveRDS(average_load.6,paste(path_to_results,"betas_stability_clus_PM2.5_10.rds", sep =""))
# saveRDS(average_load.7,paste(path_to_results,"betas_stability_clus_road.rds", sep =""))
# 
