### Summmer Project -- Linear regression - air pollution exposure-related biomarkers with CVD
### 25th Aug 2021

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
path_to_data = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/updates/"
path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj1/Air_clus_bio/lasso/pfer/"


metab = readRDS(paste(path_to_data, "denoised_clus_no_bio.rds",sep=""))
biomarker = readRDS(paste(path_to_data, "denoised_biomarkers.rds",sep=""))
combined = readRDS(paste(path_to_data, "combined_biometab.rds",sep=""))
cvd = readRDS(paste(path_to_data, "matched_cvd.rds",sep=""))


# NO2 ---------------------------------------------------------------------
# Load lasso results
out_NO2 = readRDS(paste(path_to_results,"out_combine_NO2.rds", sep = ""))
## Selected Variables ##
select_NO2 = SelectedVariables(out_NO2)
# Extract names of selected
myNO2 = names(select_NO2[select_NO2==1])
writeLines(myNO2, paste(path, "Dictionaries/lasso_selected/lasso_NO2_selected.txt", sep ="")) 

combined_selected =  combined[,colnames(combined) %in% myNO2]
colnames(combined_selected)

# Linear regression on CVD ------------------------------------------------
path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj4/"
lm1 <- lm(cvd$cvd_status ~ ., data = combined_selected)
summary(lm1)

score_lm = fitted.values(lm1) 

saveRDS(score_lm, paste(path_to_save, "linear_cvd_no2_selected_score.rds", sep =""))

# PM2.5 ---------------------------------------------------------------------
# Load lasso results
out_pm2.5 = readRDS(paste(path_to_results,"out_combine_PM2.5.rds", sep = ""))
## Selected Variables ##
select_pm2.5 = SelectedVariables(out_pm2.5)
# Extract names of selected
mypm2.5 = names(select_pm2.5[select_pm2.5==1])
writeLines(mypm2.5, paste(path, "Dictionaries/lasso_selected/lasso_PM2.5_selected.txt", sep ="")) 

combined_selected =  combined[,colnames(combined) %in% mypm2.5]
colnames(combined_selected)

# Linear regression on CVD ------------------------------------------------
path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj4/"
lm1 <- lm(cvd$cvd_status ~ ., data = combined_selected)
summary(lm1)

score_lm = fitted.values(lm1) 

saveRDS(score_lm, paste(path_to_save, "linear_cvd_pm2.5_selected_score.rds", sep =""))
