### Summmer Project -- Objective 1 : Multivariate - sPLS stability selection on metabolites and air pollutants
### 31st May 2021 edited on 16th Jun- Alicia 

## Q: Which metabolites are statistically correlated with which exposures in the data?
## --> Multivariate LASSO regression on each exposure 

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))

LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}
LoadPackages(c("pheatmap","corpcor","abind","parallel",
               "RColorBrewer","ppcor","mvtnorm",
               "pROC","stabs","huge","pulsar",
               "QUIC","igraph","glasso","glassoFast",
               "colorspace","glmnet","dplyr", "devtools", "focus"))


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

## -----------------------------------------------------------------------------------------------------------------------
met_no_bio = T
met_base = F
met_first= F
## -----------------------------------------------------------------------------------------------------------------------

if(met_no_bio){
  biobank=readRDS(paste(path, "Results/biobank_final.rds", sep=""))
  biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep="")) %>% as.matrix()
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  combine = readRDS(paste(path, "Results/combined_biometab.rds", sep=""))
  combine = combine[order(as.numeric(row.names(combine))),] %>% as.matrix()
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_clus_bio/sPLS/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/Air_clus_bio/sPLS/"
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
  metab = metab %>% select(!matches("eid_52569"))
  metab = metab[order(as.numeric(row.names(metab))), ] %>% as.matrix()
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_met_first/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/Air_met_first/"
  air = select(biobank, "24003-0.0":"24008-0.0")
  colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10")
}

source(paste(path,"Functions/penalisation_functions.R", sep=""))
# myfunctions=list.files("/rds/general/project/hda_students_data/live/Group2/project/data_analysis/cox/functions/R")
# for (k in 1:length(myfunctions)){
#   source(paste0("/rds/general/project/hda_students_data/live/Group2/project/data_analysis/cox/functions/R/",myfunctions[k]))  
# }


# # ------------------Clustered Metabolites Stability sPLS-------------------------------------------------------
# NO2 = as.matrix(air$NO2)
# PM2.5 = as.matrix(air$PM2.5)
# PM10 = as.matrix(air$PM10)
# NOx = as.matrix(air$NOx)
# PM_abs = as.matrix(air$PM2.5_absorbance)
# PM2.5_10 = as.matrix(air$`PM2.5-PM10`)
# air$MajorRoad = ifelse(air$MajorRoad == "No", 0,
#                        ifelse(air$MajorRoad == "Yes", 1, NA ))
# MajorRoad = as.matrix(air$MajorRoad)
# 
# ## NO2 ##
# 
# out = VariableSelection(xdata = metab_clus, ydata = NO2, Lambda=1:ncol(metab_clus),
#                         implementation=SparsePLS, K=100)
# 
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_clus_NO2.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_NO2.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_NO2.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM2.5 ##
# out = VariableSelection(xdata = metab_clus, ydata = PM2.5, Lambda=1:ncol(metab_clus),
#                         implementation=SparsePLS, K=100)
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
# out = VariableSelection(xdata = metab_clus, ydata = PM10, Lambda=1:ncol(metab_clus),
#                         implementation=SparsePLS, K=100)
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
# out = VariableSelection(xdata = metab_clus, ydata = NOx, Lambda=1:ncol(metab_clus),
#                         implementation=SparsePLS, K=100)
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
# out = VariableSelection(xdata = metab_clus, ydata = PM_abs, Lambda=1:ncol(metab_clus),
#                         implementation=SparsePLS, K=100)
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
# out = VariableSelection(xdata = metab_clus, ydata = PM2.5_10, Lambda=1:ncol(metab_clus),
#                         implementation=SparsePLS, K=100)
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
# out = VariableSelection(xdata = metab_clus, ydata = MajorRoad, Lambda=1:ncol(metab_clus), family = "binomial",
#                         implementation=SparsePLS, K=100)
# selprop=SelectionProportions(out)
# saveRDS(out, paste(path_to_results, "out_metab_clus_road.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_road.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/clus_road.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# # ------------------Clustered Metabolites Betas parameters extraction-------------------------------------------------------
# NO2 <- readRDS(paste(path_to_results, "out_metab_clus_NO2.rds",sep = ""))
# PM10 <- readRDS(paste(path_to_results, "out_metab_clus_PM10.rds",sep = ""))
# PM2.5 <- readRDS(paste(path_to_results, "out_metab_clus_PM2.5.rds",sep = ""))
# NOx <- readRDS(paste(path_to_results, "out_metab_clus_NOx.rds",sep = ""))
# PM_abs <- readRDS(paste(path_to_results, "out_metab_clus_PM_abs.rds",sep = ""))
# PM2.5_10 <- readRDS(paste(path_to_results, "out_metab_clus_PM2.5_10.rds",sep = ""))
# MajorRoad <- readRDS(paste(path_to_results, "out_metab_clus_road.rds",sep = ""))
# 
# # Extracting ID of calibrated lambda
# hat_lambda_id.1=ArgmaxId(NO2)[1]
# hat_lambda_id.2=ArgmaxId(PM10)[1]
# hat_lambda_id.3=ArgmaxId(PM2.5)[1]
# hat_lambda_id.4=ArgmaxId(NOx)[1]
# hat_lambda_id.5=ArgmaxId(PM_abs)[1]
# hat_lambda_id.6=ArgmaxId(PM2.5_10)[1]
# hat_lambda_id.7=ArgmaxId(MajorRoad)[1]
# 
# # Computing average beta coefficients from models with calibrated lambda
# average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.2=apply(PM10$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.3=apply(PM2.5$Beta[hat_lambda_id.3,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.4=apply(NOx$Beta[hat_lambda_id.4,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.5=apply(PM_abs$Beta[hat_lambda_id.5,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.6=apply(PM2.5_10$Beta[hat_lambda_id.6,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.7=apply(MajorRoad$Beta[hat_lambda_id.7,,],1,FUN=function(x){mean(x[x!=0])})
# 
# # Save
# saveRDS(average_load.1,paste(path_to_results,"betas_stability_clus_NO2.rds", sep =""))
# saveRDS(average_load.2,paste(path_to_results,"betas_stability_clus_PM10.rds", sep =""))
# saveRDS(average_load.3,paste(path_to_results,"betas_stability_clus_PM2.5.rds", sep =""))
# saveRDS(average_load.4,paste(path_to_results,"betas_stability_clus_NOx.rds", sep =""))
# saveRDS(average_load.5,paste(path_to_results,"betas_stability_clus_PM_abs.rds", sep =""))
# saveRDS(average_load.6,paste(path_to_results,"betas_stability_clus_PM2.5_10.rds", sep =""))
# saveRDS(average_load.7,paste(path_to_results,"betas_stability_clus_road.rds", sep =""))
# 
# 
# # ------Biomarkers stability LASSO -------------------------------------------------------------------
# 
# NO2 = as.matrix(air$NO2)
# PM2.5 = as.matrix(air$PM2.5)
# PM10 = as.matrix(air$PM10)
# NOx = as.matrix(air$NOx)
# PM_abs = as.matrix(air$PM2.5_absorbance)
# PM2.5_10 = as.matrix(air$`PM2.5-PM10`)
# MajorRoad = as.matrix(air$MajorRoad)
# 
# ## NO2 ##
# out = VariableSelection(xdata = biomarker, ydata = NO2, Lambda=1:ncol(biomarker),
#                         implementation=SparsePLS, K=100)
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_bio_NO2.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_bio_NO2.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/bio_NO2.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM2.5 ##
# out = VariableSelection(xdata = biomarker, ydata = PM2.5, Lambda=1:ncol(biomarker),
#                         implementation=SparsePLS, K=100)
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_bio_PM2.5.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_bio_PM2.5.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/bio_PM2.5.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM10 ##
# out = VariableSelection(xdata = biomarker, ydata = PM10, Lambda=1:ncol(biomarker),
#                         implementation=SparsePLS, K=100)
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_bio_PM10.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_bio_PM10.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/bio_PM10.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## NOx ##
# out = VariableSelection(xdata = biomarker, ydata = NOx, Lambda=1:ncol(biomarker),
#                         implementation=SparsePLS, K=100)
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_bio_NOx.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_bio_NOx.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/bio_NOx.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM_abs ##
# out = VariableSelection(xdata = biomarker, ydata = PM_abs, Lambda=1:ncol(biomarker),
#                         implementation=SparsePLS, K=100)
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_bio_PM_abs.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_bio_PM_abs.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/bio_PM_abs.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM2.5_10 ##
# out = VariableSelection(xdata = biomarker, ydata = PM2.5_10, Lambda=1:ncol(biomarker),
#                         implementation=SparsePLS, K=100)
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_bio_PM2.5_10.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_bio_PM2.5_10.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/bio_PM2.5_10.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## Major Road ##
# out = VariableSelection(xdata = biomarker, ydata = MajorRoad, Lambda=1:ncol(biomarker), family = "binomial",
#                         implementation=SparsePLS, K=100)
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_bio_road.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_bio_road.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/bio_road.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# # ------------------Biomarkers Betas parameters extraction-------------------------------------------------------
# 
# NO2 <- readRDS(paste(path_to_results, "out_bio_NO2.rds",sep = ""))
# PM10 <- readRDS(paste(path_to_results, "out_bio_PM10.rds",sep = ""))
# PM2.5 <- readRDS(paste(path_to_results, "out_bio_PM2.5.rds",sep = ""))
# NOx <- readRDS(paste(path_to_results, "out_bio_NOx.rds",sep = ""))
# PM_abs <- readRDS(paste(path_to_results, "out_bio_PM_abs.rds",sep = ""))
# PM2.5_10 <- readRDS(paste(path_to_results, "out_bio_PM2.5_10.rds",sep = ""))
# MajorRoad <- readRDS(paste(path_to_results, "out_bio_road.rds",sep = ""))
# 
# # Extracting ID of calibrated lambda
# hat_lambda_id.1=ArgmaxId(NO2)[1]
# hat_lambda_id.2=ArgmaxId(PM10)[1]
# hat_lambda_id.3=ArgmaxId(PM2.5)[1]
# hat_lambda_id.4=ArgmaxId(NOx)[1]
# hat_lambda_id.5=ArgmaxId(PM_abs)[1]
# hat_lambda_id.6=ArgmaxId(PM2.5_10)[1]
# hat_lambda_id.7=ArgmaxId(MajorRoad)[1]
# 
# # Computing average beta coefficients from models with calibrated lambda
# average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.2=apply(PM10$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.3=apply(PM2.5$Beta[hat_lambda_id.3,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.4=apply(NOx$Beta[hat_lambda_id.4,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.5=apply(PM_abs$Beta[hat_lambda_id.5,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.6=apply(PM2.5_10$Beta[hat_lambda_id.6,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.7=apply(MajorRoad$Beta[hat_lambda_id.7,,],1,FUN=function(x){mean(x[x!=0])})
# 
# # Save
# saveRDS(average_load.1,paste(path_to_results,"betas_bio_NO2.rds", sep =""))
# saveRDS(average_load.2,paste(path_to_results,"betas_bio_PM10.rds", sep =""))
# saveRDS(average_load.3,paste(path_to_results,"betas_bio_PM2.5.rds", sep =""))
# saveRDS(average_load.4,paste(path_to_results,"betas_bio_NOx.rds", sep =""))
# saveRDS(average_load.5,paste(path_to_results,"betas_bio_PM_abs.rds", sep =""))
# saveRDS(average_load.6,paste(path_to_results,"betas_bio_PM2.5_10.rds", sep =""))
# saveRDS(average_load.7,paste(path_to_results,"betas_bio_road.rds", sep =""))
# 
# 
# ------Combined stability LASSO -------------------------------------------------------------------

NO2 = as.matrix(air$NO2)
PM2.5 = as.matrix(air$PM2.5)
PM10 = as.matrix(air$PM10)
NOx = as.matrix(air$NOx)
PM_abs = as.matrix(air$PM2.5_absorbance)
PM2.5_10 = as.matrix(air$`PM2.5-PM10`)
MajorRoad = as.matrix(air$MajorRoad)

## NO2 ##
out = VariableSelection(xdata = combine, ydata = NO2, Lambda=1:ncol(combine),
                        implementation=SparsePLS, K=100)
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_NO2.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_NO2.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/combine_NO2.pdf",sep =""))
CalibrationPlot(out)
dev.off()

## PM2.5 ##
out = VariableSelection(xdata = combine, ydata = PM2.5, Lambda=1:ncol(combine),
                        implementation=SparsePLS, K=100)
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_PM2.5.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_PM2.5.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/combine_PM2.5.pdf",sep =""))
CalibrationPlot(out)
dev.off()

## PM10 ##
out = VariableSelection(xdata = combine, ydata = PM2.5, Lambda=1:ncol(combine),
                        implementation=SparsePLS, K=100)
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_PM10.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_PM10.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/combine_PM10.pdf",sep =""))
CalibrationPlot(out)
dev.off()

## NOx ##
out = VariableSelection(xdata = combine, ydata = NOx, Lambda=1:ncol(combine),
                        implementation=SparsePLS, K=100)
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_NOx.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_NOx.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/combine_NOx.pdf",sep =""))
CalibrationPlot(out)
dev.off()

## PM_abs ##
out = VariableSelection(xdata = combine, ydata = PM_abs, Lambda=1:ncol(combine),
                        implementation=SparsePLS, K=100)
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_PM_abs.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_PM_abs.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/combine_PM_abs.pdf",sep =""))
CalibrationPlot(out)
dev.off()

## PM2.5_10 ##
out = VariableSelection(xdata = combine, ydata = PM2.5_10, Lambda=1:ncol(combine),
                        implementation=SparsePLS, K=100)
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_PM2.5_10.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_PM2.5_10.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/combine_PM2.5_10.pdf",sep =""))
CalibrationPlot(out)
dev.off()

## Major Road ##
out = VariableSelection(xdata = combine, ydata = MajorRoad, Lambda=1:ncol(combine), family = "binomial",
                        implementation=SparsePLS, K=100)
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_road.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_road.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/combine_road.pdf",sep =""))
CalibrationPlot(out)
dev.off()

# ------------------Biomarkers Betas parameters extraction-------------------------------------------------------

NO2 <- readRDS(paste(path_to_results, "out_combine_NO2.rds",sep = ""))
PM10 <- readRDS(paste(path_to_results, "out_combine_PM10.rds",sep = ""))
PM2.5 <- readRDS(paste(path_to_results, "out_combine_PM2.5.rds",sep = ""))
NOx <- readRDS(paste(path_to_results, "out_combine_NOx.rds",sep = ""))
PM_abs <- readRDS(paste(path_to_results, "out_combine_PM_abs.rds",sep = ""))
PM2.5_10 <- readRDS(paste(path_to_results, "out_combine_PM2.5_10.rds",sep = ""))
MajorRoad <- readRDS(paste(path_to_results, "out_combine_road.rds",sep = ""))

# Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(NO2)[1]
hat_lambda_id.2=ArgmaxId(PM10)[1]
hat_lambda_id.3=ArgmaxId(PM2.5)[1]
hat_lambda_id.4=ArgmaxId(NOx)[1]
hat_lambda_id.5=ArgmaxId(PM_abs)[1]
hat_lambda_id.6=ArgmaxId(PM2.5_10)[1]
hat_lambda_id.7=ArgmaxId(MajorRoad)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
average_load.2=apply(PM10$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
average_load.3=apply(PM2.5$Beta[hat_lambda_id.3,,],1,FUN=function(x){mean(x[x!=0])})
average_load.4=apply(NOx$Beta[hat_lambda_id.4,,],1,FUN=function(x){mean(x[x!=0])})
average_load.5=apply(PM_abs$Beta[hat_lambda_id.5,,],1,FUN=function(x){mean(x[x!=0])})
average_load.6=apply(PM2.5_10$Beta[hat_lambda_id.6,,],1,FUN=function(x){mean(x[x!=0])})
average_load.7=apply(MajorRoad$Beta[hat_lambda_id.7,,],1,FUN=function(x){mean(x[x!=0])})

# Save
saveRDS(average_load.1,paste(path_to_results,"betas_combine_NO2.rds", sep =""))
saveRDS(average_load.2,paste(path_to_results,"betas_combine_PM10.rds", sep =""))
saveRDS(average_load.3,paste(path_to_results,"betas_combine_PM2.5.rds", sep =""))
saveRDS(average_load.4,paste(path_to_results,"betas_combine_NOx.rds", sep =""))
saveRDS(average_load.5,paste(path_to_results,"betas_combine_PM_abs.rds", sep =""))
saveRDS(average_load.6,paste(path_to_results,"betas_combine_PM2.5_10.rds", sep =""))
saveRDS(average_load.7,paste(path_to_results,"betas_combine_road.rds", sep =""))


# # --------------------------Individual Metabolites Stability LASSO-----------------------------------------------
# NO2 = as.matrix(air$NO2)
# PM2.5 = as.matrix(air$PM2.5)
# PM10 = as.matrix(air$PM10)
# NOx = as.matrix(air$NOx)
# PM_abs = as.matrix(air$PM2.5_absorbance)
# PM2.5_10 = as.matrix(air$`PM2.5-PM10`)
# air$MajorRoad = ifelse(air$MajorRoad == "No", 0,
#                        ifelse(air$MajorRoad == "Yes", 1, NA ))
# MajorRoad = as.matrix(air$MajorRoad)
# 
# ## NO2 ##
# out = VariableSelection(xdata = metab, ydata = NO2,K=100, tau = 0.5, verbose=FALSE,
#                            family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_NO2.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_NO2.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/NO2.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM2.5 ##
# out = VariableSelection(xdata = metab, ydata = PM2.5,K=100, tau = 0.5, verbose=FALSE,
#                           family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_PM2.5.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_PM2.5.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/PM2.5.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM10 ##
# out = VariableSelection(xdata = metab, ydata = PM10,K=100, tau = 0.5, verbose=FALSE,
#                           family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_PM10.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_PM10.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/PM10.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## NOx ##
# out = VariableSelection(xdata = metab, ydata = NOx,K=100, tau = 0.5, verbose=FALSE,
#                           family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_NOx.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_NOx.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/NOx.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM_abs ##
# out = VariableSelection(xdata = metab, ydata = PM_abs,K=100, tau = 0.5, verbose=FALSE,
#                           family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_PM_abs.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_PM_abs.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/PM_abs.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## PM2.5_10 ##
# out = VariableSelection(xdata = metab, ydata = PM2.5_10,K=100, tau = 0.5, verbose=FALSE,
#                           family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_PM2.5_10.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_PM2.5_10.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/PM2.5_10.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# ## Major Road ##
# out = VariableSelection(xdata = metab, ydata = MajorRoad,K=100, tau = 0.5, verbose=FALSE,
#                           family = "gaussian")
# selprop=SelectionProportions(out)
# 
# saveRDS(out, paste(path_to_results, "out_metab_road.rds", sep =""))
# saveRDS(selprop, paste(path_to_results, "selprop_metab_road.rds", sep =""))
# 
# pdf(paste(path_to_save,"CalibrationPlot/road.pdf",sep =""))
# CalibrationPlot(out)
# dev.off()
# 
# # Multivariate responses ##
# # all = as.matrix(cbind(NO2, PM2.5, PM10))
# # 
# # out = VariableSelection(xdata = metab, ydata = all,K=100, Lambda = 1:ncol(metab),
# #                           family = "mgaussian")
# # selprop=SelectionProportions(out, plot=FALSE)
# # 
# # saveRDS(out, paste(path_to_results, "out_metab_multi.rds", sep =""))
# # saveRDS(selprop, paste(path_to_results, "selprop_metab_multi.rds", sep =""))
# 
# 
# 
# # ------------------Individual Metabolites Betas parameters extraction-------------------------------------------------------
# NO2 <- readRDS(paste(path_to_results, "out_metab_NO2.rds",sep = ""))
# PM10 <- readRDS(paste(path_to_results, "out_metab_PM10.rds",sep = ""))
# PM2.5 <- readRDS(paste(path_to_results, "out_metab_PM2.5.rds",sep = ""))
# NOx <- readRDS(paste(path_to_results, "out_metab_NOx.rds",sep = ""))
# PM_abs <- readRDS(paste(path_to_results, "out_metab_PM_abs.rds",sep = ""))
# PM2.5_10 <- readRDS(paste(path_to_results, "out_metab_PM2.5_10.rds",sep = ""))
# MajorRoad <- readRDS(paste(path_to_results, "out_metab_road.rds",sep = ""))
# 
# # multi <- readRDS(paste(path_to_results, "out_metab_multi.rds",sep = ""))
# 
# # Extracting ID of calibrated lambda
# hat_lambda_id.1=ArgmaxId(NO2)[1]
# hat_lambda_id.2=ArgmaxId(PM10)[1]
# hat_lambda_id.3=ArgmaxId(PM2.5)[1]
# hat_lambda_id.4=ArgmaxId(NOx)[1]
# hat_lambda_id.5=ArgmaxId(PM_abs)[1]
# hat_lambda_id.6=ArgmaxId(PM2.5_10)[1]
# hat_lambda_id.7=ArgmaxId(MajorRoad)[1]
# # hat_lambda_id.4=ArgmaxId(multi)[1]
# 
# # Computing average beta coefficients from models with calibrated lambda
# average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.2=apply(PM10$Beta[hat_lambda_id.2,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.3=apply(PM2.5$Beta[hat_lambda_id.3,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.4=apply(NOx$Beta[hat_lambda_id.4,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.5=apply(PM_abs$Beta[hat_lambda_id.5,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.6=apply(PM2.5_10$Beta[hat_lambda_id.6,,],1,FUN=function(x){mean(x[x!=0])})
# average_load.7=apply(MajorRoad$Beta[hat_lambda_id.7,,],1,FUN=function(x){mean(x[x!=0])})
# # average_load.4=apply(multi$Beta[hat_lambda_id.4,,],1,FUN=function(x){mean(x[x!=0])})
# 
# # Save
# saveRDS(average_load.1,paste(path_to_results,"betas_stability_NO2.rds", sep =""))
# saveRDS(average_load.2,paste(path_to_results,"betas_stability_PM10.rds", sep =""))
# saveRDS(average_load.3,paste(path_to_results,"betas_stability_PM2.5.rds", sep =""))
# saveRDS(average_load.4,paste(path_to_results,"betas_stability_NOx.rds", sep =""))
# saveRDS(average_load.5,paste(path_to_results,"betas_stability_PM_abs.rds", sep =""))
# saveRDS(average_load.6,paste(path_to_results,"betas_stability_PM2.5_10.rds", sep =""))
# saveRDS(average_load.7,paste(path_to_results,"betas_stability_road.rds", sep =""))






