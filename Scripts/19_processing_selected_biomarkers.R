### Summmer Project -- Processing - air pollution exposure-related biomarkers
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


# Combined ----------------------------------------------------------------
# Load data 
metab = readRDS(paste(path_to_data, "denoised_clus_no_bio.rds",sep=""))
biomarker = readRDS(paste(path_to_data, "denoised_biomarkers.rds",sep=""))
combined = readRDS(paste(path_to_data, "combined_biometab.rds",sep=""))

# Load lasso results
out_NO2 = readRDS(paste(path_to_results,"out_combine_NO2.rds", sep = ""))
out_PM10 = readRDS(paste(path_to_results,"out_combine_PM10.rds", sep = ""))
out_NOx = readRDS(paste(path_to_results,"out_combine_NOx.rds", sep = ""))
out_PM2.5 = readRDS(paste(path_to_results,"out_combine_PM10.rds", sep = ""))
out_PM2.5_10 = readRDS(paste(path_to_results,"out_combine_PM2.5_10.rds", sep = ""))
out_abs = readRDS(paste(path_to_results,"out_combine_PM_abs.rds", sep = ""))
out_road = readRDS(paste(path_to_results,"out_combine_road.rds", sep = ""))


## Selected Variables ##
select_NO2 = SelectedVariables(out_NO2)
select_PM10 = SelectedVariables(out_PM10)
select_NOx = SelectedVariables(out_NOx)
select_PM2.5 = SelectedVariables(out_PM2.5)
select_PM2.5_10 = SelectedVariables(out_PM2.5_10)
select_abs = SelectedVariables(out_abs)
select_road  = SelectedVariables(out_road)

# Extract names of selected
myNO2 = names(select_NO2[select_NO2==1])
myPM10 = names(select_NO2[select_PM10==1])
myNOx = names(select_NO2[select_NOx==1])
myPM2.5 = names(select_NO2[select_PM2.5==1])
myPM2.510 = names(select_NO2[select_PM2.5_10==1])
myabs = names(select_NO2[select_abs==1])
myroad = names(select_NO2[select_road==1])

writeLines(myNO2, paste(path, "Dictionaries/lasso_selected/lasso_NO2_selected.txt", sep ="")) 
writeLines(myPM10, paste(path, "Dictionaries/lasso_selected/lasso_PM110_selected.txt", sep ="")) 
writeLines(myNOx, paste(path, "Dictionaries/lasso_selected/lasso_NOx_selected.txt", sep ="")) 
writeLines(myPM2.5, paste(path, "Dictionaries/lasso_selected/lasso_PM2.5_selected.txt", sep ="")) 
writeLines(myPM2.510, paste(path, "Dictionaries/lasso_selected/lasso_PM2.510_selected.txt", sep ="")) 
writeLines(myabs, paste(path, "Dictionaries/lasso_selected/lasso_abs_selected.txt", sep ="")) 
writeLines(myroad, paste(path, "Dictionaries/lasso_selected/lasso_road_selected.txt", sep ="")) 

myall = c(myNO2,myPM10,myNOx,myPM2.5,myPM2.510,myabs,myroad)
myall = unique(myall) # 31 selected

# Subset data using extracted names

combined_selected =  combined[,colnames(combined) %in% myall]

# save
saveRDS(combined_selected, paste(path_to_data, "denoised_selected_combined.rds", sep = ""))


# Clusters ----------------------------------------------------------------
# Load data 
metab = readRDS(paste(path_to_data, "denoised_clus_no_bio.rds",sep=""))
biomarker = readRDS(paste(path_to_data, "denoised_biomarkers.rds",sep=""))
combined = readRDS(paste(path_to_data, "combined_biometab.rds",sep=""))

# Load lasso results
out_NO2 = readRDS(paste(path_to_results,"out_metab_clus_NO2.rds", sep = ""))
out_PM10 = readRDS(paste(path_to_results,"out_metab_clus_PM10.rds", sep = ""))
out_NOx = readRDS(paste(path_to_results,"out_metab_clus_NOx.rds", sep = ""))
out_PM2.5 = readRDS(paste(path_to_results,"out_metab_clus_PM10.rds", sep = ""))
out_PM2.5_10 = readRDS(paste(path_to_results,"out_metab_clus_PM2.5_10.rds", sep = ""))
out_abs = readRDS(paste(path_to_results,"out_metab_clus_PM_abs.rds", sep = ""))
out_road = readRDS(paste(path_to_results,"out_metab_clus_road.rds", sep = ""))


## Selected Variables ##
select_NO2 = SelectedVariables(out_NO2)
select_PM10 = SelectedVariables(out_PM10)
select_NOx = SelectedVariables(out_NOx)
select_PM2.5 = SelectedVariables(out_PM2.5)
select_PM2.5_10 = SelectedVariables(out_PM2.5_10)
select_abs = SelectedVariables(out_abs)
select_road  = SelectedVariables(out_road)

# Extract names of selected
myNO2 = names(select_NO2[select_NO2==1])
myPM10 = names(select_NO2[select_PM10==1])
myNOx = names(select_NO2[select_NOx==1])
myPM2.5 = names(select_NO2[select_PM2.5==1])
myPM2.510 = names(select_NO2[select_PM2.5_10==1])
myabs = names(select_NO2[select_abs==1])
myroad = names(select_NO2[select_road==1])

writeLines(myNO2, paste(path, "Dictionaries/lasso_selected/cluster/lasso_NO2_selected.txt", sep ="")) 
writeLines(myPM10, paste(path, "Dictionaries/lasso_selected/cluster/lasso_PM110_selected.txt", sep ="")) 
writeLines(myNOx, paste(path, "Dictionaries/lasso_selected/cluster/lasso_NOx_selected.txt", sep ="")) 
writeLines(myPM2.5, paste(path, "Dictionaries/lasso_selected/cluster/lasso_PM2.5_selected.txt", sep ="")) 
writeLines(myPM2.510, paste(path, "Dictionaries/lasso_selected/cluster/lasso_PM2.510_selected.txt", sep ="")) 
writeLines(myabs, paste(path, "Dictionaries/lasso_selected/cluster/lasso_abs_selected.txt", sep ="")) 
writeLines(myroad, paste(path, "Dictionaries/lasso_selected/cluster/lasso_road_selected.txt", sep ="")) 

myall = c(myNO2,myPM10,myNOx,myPM2.5,myPM2.510,myabs,myroad)
myall = unique(myall) # 31 selected

# Subset data using extracted names

metab_selected =  metab[,colnames(metab) %in% myall]

# save
saveRDS(metab_selected, paste(path_to_data, "denoised_selected_metab.rds", sep = ""))


# Biomarker ----------------------------------------------------------------
# Load data 
metab = readRDS(paste(path_to_data, "denoised_clus_no_bio.rds",sep=""))
biomarker = readRDS(paste(path_to_data, "denoised_biomarkers.rds",sep=""))
combined = readRDS(paste(path_to_data, "combined_biometab.rds",sep=""))

# Load lasso results
out_NO2 = readRDS(paste(path_to_results,"out_bio_NO2.rds", sep = ""))
out_PM10 = readRDS(paste(path_to_results,"out_bio_PM10.rds", sep = ""))
out_NOx = readRDS(paste(path_to_results,"out_bio_NOx.rds", sep = ""))
out_PM2.5 = readRDS(paste(path_to_results,"out_bio_PM10.rds", sep = ""))
out_PM2.5_10 = readRDS(paste(path_to_results,"out_bio_PM2.5_10.rds", sep = ""))
out_abs = readRDS(paste(path_to_results,"out_bio_PM_abs.rds", sep = ""))
out_road = readRDS(paste(path_to_results,"out_bio_road.rds", sep = ""))


## Selected Variables ##
select_NO2 = SelectedVariables(out_NO2)
select_PM10 = SelectedVariables(out_PM10)
select_NOx = SelectedVariables(out_NOx)
select_PM2.5 = SelectedVariables(out_PM2.5)
select_PM2.5_10 = SelectedVariables(out_PM2.5_10)
select_abs = SelectedVariables(out_abs)
select_road  = SelectedVariables(out_road)

# Extract names of selected
myNO2 = names(select_NO2[select_NO2==1])
myPM10 = names(select_NO2[select_PM10==1])
myNOx = names(select_NO2[select_NOx==1])
myPM2.5 = names(select_NO2[select_PM2.5==1])
myPM2.510 = names(select_NO2[select_PM2.5_10==1])
myabs = names(select_NO2[select_abs==1])
myroad = names(select_NO2[select_road==1])

writeLines(myNO2, paste(path, "Dictionaries/lasso_selected/biomarker/lasso_NO2_selected.txt", sep ="")) 
writeLines(myPM10, paste(path, "Dictionaries/lasso_selected/biomarker/lasso_PM110_selected.txt", sep ="")) 
writeLines(myNOx, paste(path, "Dictionaries/lasso_selected/biomarker/lasso_NOx_selected.txt", sep ="")) 
writeLines(myPM2.5, paste(path, "Dictionaries/lasso_selected/biomarker/lasso_PM2.5_selected.txt", sep ="")) 
writeLines(myPM2.510, paste(path, "Dictionaries/lasso_selected/biomarker/lasso_PM2.510_selected.txt", sep ="")) 
writeLines(myabs, paste(path, "Dictionaries/lasso_selected/biomarker/lasso_abs_selected.txt", sep ="")) 
writeLines(myroad, paste(path, "Dictionaries/lasso_selected/biomarker/lasso_road_selected.txt", sep ="")) 

myall = c(myNO2,myPM10,myNOx,myPM2.5,myPM2.510,myabs,myroad)
myall = unique(myall) # 17 selected

# Subset data using extracted names

bio_selected =  biomarker[,colnames(biomarker) %in% myall]

# save
saveRDS(bio_selected, paste(path_to_data, "denoised_selected_biomarker.rds", sep = ""))



