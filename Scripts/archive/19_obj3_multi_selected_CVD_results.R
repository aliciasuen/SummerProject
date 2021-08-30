### Summmer Project -- Objective3 : sPLS-DA on selected metabolites and CVD outcome 
### 16th July  2021 - Alicia 


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


lasso = F
spls = T

if(lasso){
  biobank=readRDS(paste(path, "Results_report/updates/matched_biobank.rds", sep=""))
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_selected_biomarker.rds", sep="")) %>% as.matrix()
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_selected_metab.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  combine = readRDS(paste(path, "Results_report/updates/denoised_selected_combined.rds", sep=""))
  combine = combine[order(as.numeric(row.names(combine))),] %>% as.matrix()
  cvd = readRDS(paste(path, "Results_report/updates/matched_cvd.rds",sep =""))
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj3/selected_lasso/"
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj3/selected_lasso/"
}
if(spls){
  biobank=readRDS(paste(path, "Results_report/updates/matched_biobank.rds", sep=""))
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_selected_biomarker.rds", sep="")) %>% as.matrix()
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_selected_metab.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] %>% as.matrix()
  combine = readRDS(paste(path, "Results_report/updates/denoised_selected_combined.rds", sep=""))
  combine = combine[order(as.numeric(row.names(combine))),] %>% as.matrix()
  cvd = readRDS(paste(path, "Results_report/updates/matched_cvd.rds",sep =""))
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj3/selected_spls/"
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj3/selected_spls/"
}

# ------------------Clustered Metabolites Stability LASSO-------------------------------------------------------

Y = as.factor(cvd$cvd_status)

out = VariableSelection(xdata = metab_clus, ydata = Y,K=100, tau = 0.5, verbose=FALSE, PFER_thr =20 , PFER_method = "MB",
                        family = "binomial")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_metab_clus.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_metab_clus.rds", sep =""))

pdf(paste(path_to_figure,"CalibrationPlot/clus_heatmap.pdf",sep =""))
CalibrationPlot(out)
dev.off()

# ------------------Clustered Metabolites Betas parameters extraction-------------------------------------------------------
out <- readRDS(paste(path_to_results, "out_metab_clus.rds",sep = ""))

# Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})

# Save
saveRDS(average_load.1,paste(path_to_results,"betas_clus.rds", sep =""))


# ------Biomarkers stability LASSO -------------------------------------------------------------------

out = VariableSelection(xdata = biomarker, ydata = Y,K=100, tau = 0.5, verbose=FALSE,PFER_thr =20 , PFER_method = "MB",
                        family = "binomial")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_bio.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_bio.rds", sep =""))

pdf(paste(path_to_figure,"CalibrationPlot/bio_heatmap.pdf",sep =""))
CalibrationPlot(out)
dev.off()

# ------------------Biomarkers Betas parameters extraction-------------------------------------------------------

out <- readRDS(paste(path_to_results, "out_bio.rds",sep = ""))

# Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})

# Save
saveRDS(average_load.1,paste(path_to_results,"betas_bio.rds", sep =""))


# ------Combined stability LASSO -------------------------------------------------------------------

out = VariableSelection(xdata = combine, ydata = Y,K=100, tau = 0.5, verbose=FALSE,PFER_thr =20 , PFER_method = "MB",
                        family = "binomial")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine.rds", sep =""))

pdf(paste(path_to_figure,"CalibrationPlot/combine_heatmap.pdf",sep =""))
CalibrationPlot(out)
dev.off()

# ------------------Combined Betas parameters extraction-------------------------------------------------------
out <- readRDS(paste(path_to_results, "out_combine.rds",sep = ""))

# # Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(out)[1]

# # Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})

# # Save
saveRDS(average_load.1,paste(path_to_results,"betas_combine.rds", sep =""))



