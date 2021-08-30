### Summmer Project -- Objective 1 : Multivariate - LASSO stability selection on metabolites and air pollutants
### 6th July  - Alicia 


# Load packages
library(tidyverse)
library(colorspace)
library(plotrix)
library(maptools)
library(ggplot2)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

## -----------------------------------------------------------------------------------------------------------------------
group1_med = F
met_no_bio = T

## -----------------------------------------------------------------------------------------------------------------------
if(group1_med){
  biomarker = readRDS(paste(path, "Results_report/denoised_biomarkers.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results_report/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj1/cluster_manual/group1_med/lasso/"
  path_to_multi_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj1/cluster_manual/group1_med/"
  path_to_biomarker_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj1/Air_clus_bio/lasso/"
  ## sel prop ##
  bio_NO2_sp = readRDS(paste(path_to_biomarker_Results_report, "selprop_bio_NO2.rds",sep=""))
  bio_PM10_sp = readRDS(paste(path_to_biomarker_Results_report, "selprop_bio_PM10.rds",sep=""))
  bio_PM2.5_sp = readRDS(paste(path_to_biomarker_Results_report, "selprop_bio_PM2.5.rds",sep=""))
  bio_NOx_sp = readRDS(paste(path_to_biomarker_Results_report, "selprop_bio_NOx.rds",sep=""))
  bio_PM2.5_10_sp = readRDS(paste(path_to_biomarker_Results_report, "selprop_bio_PM2.5_10.rds",sep=""))
  bio_abs_sp = readRDS(paste(path_to_biomarker_Results_report, "selprop_bio_PM_abs.rds",sep=""))
  bio_road_sp = readRDS(paste(path_to_biomarker_Results_report, "selprop_bio_road.rds",sep=""))
  clus_NO2_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_NO2.rds",sep=""))
  clus_PM10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_PM10.rds",sep=""))
  clus_PM2.5_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_PM2.5.rds",sep=""))
  clus_NOx_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_NOx.rds",sep=""))
  clus_PM2.5_10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_PM2.5_10.rds",sep=""))
  clus_abs_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_PM_abs.rds",sep=""))
  clus_road_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_road.rds",sep=""))
  combine_NO2_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_NO2.rds",sep=""))
  combine_PM10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_PM10.rds",sep=""))
  combine_PM2.5_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_PM2.5.rds",sep=""))
  combine_NOx_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_NOx.rds",sep=""))
  combine_PM2.5_10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_PM2.5_10.rds",sep=""))
  combine_abs_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_PM_abs.rds",sep=""))
  combine_road_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_road.rds",sep=""))
  ## out ## 
  out_bio_NO2 = readRDS(paste(path_to_biomarker_Results_report, "out_bio_NO2.rds",sep=""))
  out_bio_PM10 = readRDS(paste(path_to_biomarker_Results_report, "out_bio_PM10.rds",sep=""))
  out_bio_PM2.5 = readRDS(paste(path_to_biomarker_Results_report, "out_bio_PM2.5.rds",sep=""))
  out_bio_NOx = readRDS(paste(path_to_biomarker_Results_report, "out_bio_NOx.rds",sep=""))
  out_bio_PM2.5_10 = readRDS(paste(path_to_biomarker_Results_report, "out_bio_PM2.5_10.rds",sep=""))
  out_bio_abs = readRDS(paste(path_to_biomarker_Results_report, "out_bio_PM_abs.rds",sep=""))
  out_bio_road = readRDS(paste(path_to_biomarker_Results_report, "out_bio_road.rds",sep=""))
  out_clus_NO2 = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_NO2.rds",sep=""))
  out_clus_PM10 = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_PM10.rds",sep=""))
  out_clus_PM2.5 = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_PM2.5.rds",sep=""))
  out_clus_NOx = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_NOx.rds",sep=""))
  out_clus_PM2.5_10 = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_PM2.5_10.rds",sep=""))
  out_clus_abs = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_PM_abs.rds",sep=""))
  out_clus_road = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_road.rds",sep=""))
  out_combine_NO2 = readRDS(paste(path_to_multi_Results_report, "out_combine_NO2.rds",sep=""))
  out_combine_PM10 = readRDS(paste(path_to_multi_Results_report, "out_combine_PM10.rds",sep=""))
  out_combine__PM2.5 = readRDS(paste(path_to_multi_Results_report, "out_combine_PM2.5.rds",sep=""))
  out_combine_NOx = readRDS(paste(path_to_multi_Results_report, "out_combine_NOx.rds",sep=""))
  out_combine_PM2.5_10 = readRDS(paste(path_to_multi_Results_report, "out_combine_PM2.5_10.rds",sep=""))
  out_combine_abs = readRDS(paste(path_to_multi_Results_report, "out_combine_PM_abs.rds",sep=""))
  out_combine_road = readRDS(paste(path_to_multi_Results_report, "out_combine_road.rds",sep=""))
  ## betas ## 
  bio_NO2_betas  = readRDS(paste(path_to_biomarker_Results_report, "betas_bio_NO2.rds",sep=""))
  bio_PM10_betas  = readRDS(paste(path_to_biomarker_Results_report, "betas_bio_PM10.rds",sep=""))
  bio_PM2.5_betas  = readRDS(paste(path_to_biomarker_Results_report, "betas_bio_PM2.5.rds",sep=""))
  bio_NOx_betas  = readRDS(paste(path_to_biomarker_Results_report, "betas_bio_NOx.rds",sep=""))
  bio_PM2.5_10_betas  = readRDS(paste(path_to_biomarker_Results_report, "betas_bio_PM2.5_10.rds",sep=""))
  bio_abs_betas  = readRDS(paste(path_to_biomarker_Results_report, "betas_bio_PM_abs.rds",sep=""))
  bio_road_betas  = readRDS(paste(path_to_biomarker_Results_report, "betas_bio_road.rds",sep=""))
  clus_NO2_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_NO2.rds",sep=""))
  clus_PM10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_PM10.rds",sep=""))
  clus_PM2.5_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_PM2.5.rds",sep=""))
  clus_NOx_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_NOx.rds",sep=""))
  clus_PM2.5_10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_PM2.5_10.rds",sep=""))
  clus_abs_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_PM_abs.rds",sep=""))
  clus_road_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_road.rds",sep=""))
  combine_NO2_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_NO2.rds",sep=""))
  combine_PM10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_PM10.rds",sep=""))
  combine_PM2.5_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_PM2.5.rds",sep=""))
  combine_NOx_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_NOx.rds",sep=""))
  combine_PM2.5_10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_PM2.5_10.rds",sep=""))
  combine_abs_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_PM_abs.rds",sep=""))
  combine_road_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_road.rds",sep=""))
  
}

if(met_no_bio){
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_biomarkers.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj1/Air_clus_bio/lasso/pfer/"
  path_to_multi_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj1/Air_clus_bio/lasso/pfer/"
  ## sel prop ##
  bio_NO2_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio_NO2.rds",sep=""))
  bio_PM10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio_PM10.rds",sep=""))
  bio_PM2.5_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio_PM2.5.rds",sep=""))
  bio_NOx_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio_NOx.rds",sep=""))
  bio_PM2.5_10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio_PM2.5_10.rds",sep=""))
  bio_abs_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio_PM_abs.rds",sep=""))
  bio_road_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio_road.rds",sep=""))
  clus_NO2_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_NO2.rds",sep=""))
  clus_PM10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_PM10.rds",sep=""))
  clus_PM2.5_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_PM2.5.rds",sep=""))
  clus_NOx_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_NOx.rds",sep=""))
  clus_PM2.5_10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_PM2.5_10.rds",sep=""))
  clus_abs_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_PM_abs.rds",sep=""))
  clus_road_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_road.rds",sep=""))
  combine_NO2_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_NO2.rds",sep=""))
  combine_PM10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_PM10.rds",sep=""))
  combine_PM2.5_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_PM2.5.rds",sep=""))
  combine_NOx_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_NOx.rds",sep=""))
  combine_PM2.5_10_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_PM2.5_10.rds",sep=""))
  combine_abs_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_PM_abs.rds",sep=""))
  combine_road_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_road.rds",sep=""))
  ## out ## 
  out_bio_NO2 = readRDS(paste(path_to_multi_Results_report, "out_bio_NO2.rds",sep=""))
  out_bio_PM10 = readRDS(paste(path_to_multi_Results_report, "out_bio_PM10.rds",sep=""))
  out_bio_PM2.5 = readRDS(paste(path_to_multi_Results_report, "out_bio_PM2.5.rds",sep=""))
  out_bio_NOx = readRDS(paste(path_to_multi_Results_report, "out_bio_NOx.rds",sep=""))
  out_bio_PM2.5_10 = readRDS(paste(path_to_multi_Results_report, "out_bio_PM2.5_10.rds",sep=""))
  out_bio_abs = readRDS(paste(path_to_multi_Results_report, "out_bio_PM_abs.rds",sep=""))
  out_bio_road = readRDS(paste(path_to_multi_Results_report, "out_bio_road.rds",sep=""))
  out_clus_NO2 = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_NO2.rds",sep=""))
  out_clus_PM10 = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_PM10.rds",sep=""))
  out_clus_PM2.5 = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_PM2.5.rds",sep=""))
  out_clus_NOx = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_NOx.rds",sep=""))
  out_clus_PM2.5_10 = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_PM2.5_10.rds",sep=""))
  out_clus_abs = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_PM_abs.rds",sep=""))
  out_clus_road = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_road.rds",sep=""))
  out_combine_NO2 = readRDS(paste(path_to_multi_Results_report, "out_combine_NO2.rds",sep=""))
  out_combine_PM10 = readRDS(paste(path_to_multi_Results_report, "out_combine_PM10.rds",sep=""))
  out_combine__PM2.5 = readRDS(paste(path_to_multi_Results_report, "out_combine_PM2.5.rds",sep=""))
  out_combine_NOx = readRDS(paste(path_to_multi_Results_report, "out_combine_NOx.rds",sep=""))
  out_combine_PM2.5_10 = readRDS(paste(path_to_multi_Results_report, "out_combine_PM2.5_10.rds",sep=""))
  out_combine_abs = readRDS(paste(path_to_multi_Results_report, "out_combine_PM_abs.rds",sep=""))
  out_combine_road = readRDS(paste(path_to_multi_Results_report, "out_combine_road.rds",sep=""))
  ## betas ## 
  bio_NO2_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio_NO2.rds",sep=""))
  bio_PM10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio_PM10.rds",sep=""))
  bio_PM2.5_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio_PM2.5.rds",sep=""))
  bio_NOx_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio_NOx.rds",sep=""))
  bio_PM2.5_10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio_PM2.5_10.rds",sep=""))
  bio_abs_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio_PM_abs.rds",sep=""))
  bio_road_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio_road.rds",sep=""))
  clus_NO2_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_NO2.rds",sep=""))
  clus_PM10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_PM10.rds",sep=""))
  clus_PM2.5_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_PM2.5.rds",sep=""))
  clus_NOx_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_NOx.rds",sep=""))
  clus_PM2.5_10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_PM2.5_10.rds",sep=""))
  clus_abs_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_PM_abs.rds",sep=""))
  clus_road_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_road.rds",sep=""))
  combine_NO2_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_NO2.rds",sep=""))
  combine_PM10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_PM10.rds",sep=""))
  combine_PM2.5_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_PM2.5.rds",sep=""))
  combine_NOx_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_NOx.rds",sep=""))
  combine_PM2.5_10_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_PM2.5_10.rds",sep=""))
  combine_abs_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_PM_abs.rds",sep=""))
  combine_road_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_road.rds",sep=""))
  
}

source(paste(path,"Functions/penalisation_functions.R", sep=""))


# ------------------Clustered Metabolites LASSO selection prop v.s. beta  -------------------------------------------------------

## NO2 ##
tmp = rep(c(0.005,-0.005),length.out = length(combine_NO2_betas))
names(tmp)=names(sort(combine_NO2_betas))
x_nudge = tmp[names(combine_NO2_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(combine_NO2_betas))
names(tmp)=names(sort(combine_NO2_betas))
y_nudge = tmp[names(combine_NO2_betas)]

pdf(paste(path_to_figure, "lasso_volcano_NO2_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(combine_NO2_betas, combine_NO2_sp, las=1, pch=19,
     col=ifelse(abs(combine_NO2_sp) < Argmax(out_combine_NO2)[2],"grey","lightgoldenrod"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgoldenrod",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(combine_NO2_betas+x_nudge, combine_NO2_sp+y_nudge,
     labels = ifelse(abs(combine_NO2_sp) < Argmax(out_combine_NO2)[2],"",names(combine_NO2_sp)),
     cex = 0.4)
dev.off()
sum(clus_NO2_sp>= Argmax(out_clus_NO2)[2])

## PM10 ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_PM10_betas))
names(tmp)=names(sort(clus_PM10_betas))
x_nudge = tmp[names(clus_PM10_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_PM10_sp))
names(tmp)=names(sort(clus_PM10_sp))
y_nudge = tmp[names(clus_PM10_sp)]

pdf(paste(path_to_figure, "lasso_volcano_PM10_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_PM10_betas, clus_PM10_sp, las=1, pch=19,
     col=ifelse(abs(clus_PM10_sp) < 0.9,"grey","skyblue"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "skyblue",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(clus_PM10_betas+x_nudge, clus_PM10_sp+y_nudge,
     labels = ifelse(abs(clus_PM10_sp) < 0.9,"",names(clus_PM10_sp)),
     cex = 0.6)
dev.off()
sum(clus_PM10_sp>=0.9)

## PM2.5 ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_PM2.5_betas))
names(tmp)=names(sort(clus_PM2.5_betas))
x_nudge = tmp[names(clus_PM2.5_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_PM2.5_sp))
names(tmp)=names(sort(clus_PM2.5_sp))
y_nudge = tmp[names(clus_PM2.5_sp)]

pdf(paste(path_to_figure, "lasso_volcano_PM2.5_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_PM2.5_betas, clus_PM2.5_sp, las=1, pch=19,
     col=ifelse(abs(clus_PM2.5_sp) < 0.9,"grey","lightgreen"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgreen",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(clus_PM2.5_betas+x_nudge, clus_PM2.5_sp+y_nudge,
     labels = ifelse(abs(clus_PM2.5_sp) < 0.9,"",names(clus_PM2.5_sp)),
     cex = 0.4)
dev.off()

## NOx ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_NOx_betas))
names(tmp)=names(sort(clus_NOx_betas))
x_nudge = tmp[names(clus_NOx_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_NOx_sp))
names(tmp)=names(sort(clus_NOx_sp))
y_nudge = tmp[names(clus_NOx_sp)]

pdf(paste(path_to_figure, "lasso_volcano_PM2.5_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_NOx_betas, clus_NOx_sp, las=1, pch=19,
     col=ifelse(abs(clus_NOx_sp) < 0.9,"grey","lightgreen"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgreen",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(clus_NOx_betas+x_nudge, clus_NOx_sp+y_nudge,
     labels = ifelse(abs(clus_NOx_sp) < 0.9,"",names(clus_NOx_sp)),
     cex = 0.4)
dev.off()

## major road ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_road_betas))
names(tmp)=names(sort(clus_road_betas))
x_nudge = tmp[names(clus_road_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_road_sp))
names(tmp)=names(sort(clus_road_sp))
y_nudge = tmp[names(clus_road_sp)]

pdf(paste(path_to_figure, "lasso_volcano_road_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_road_betas, clus_road_sp, las=1, pch=19,
     col=ifelse(abs(clus_road_sp) < 0.9,"grey","lightgreen"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgreen",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(clus_road_betas+x_nudge, clus_road_sp+y_nudge,
     labels = ifelse(abs(clus_road_sp) < 0.9,"",names(clus_road_sp)),
     cex = 0.4)
dev.off()
sum(clus_road_sp>=0.9)

# ------------------Biomarkers LASSO selection prop v.s. beta  -------------------------------------------------------

## NO2 ##
tmp = rep(c(0.005,-0.005),length.out = length(bio_NO2_betas))
names(tmp)=names(sort(bio_NO2_betas))
x_nudge = tmp[names(bio_NO2_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(bio_NO2_sp))
names(tmp)=names(sort(bio_NO2_sp))
y_nudge = tmp[names(bio_NO2_sp)]

pdf(paste(path_to_figure, "lasso_volcano_NO2_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(bio_NO2_betas, bio_NO2_sp, las=1, pch=19,
     col=ifelse(abs(bio_NO2_sp) < Argmax(out_bio_NO2),"grey","lightgoldenrod"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgoldenrod",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(bio_NO2_betas+x_nudge, bio_NO2_sp+y_nudge,
     labels = ifelse(abs(bio_NO2_sp) < 0.9,"",names(bio_NO2_sp)),
     cex = 0.4)
dev.off()
sum(bio_NO2_sp>=0.9)

## PM10 ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_PM10_betas))
names(tmp)=names(sort(clus_PM10_betas))
x_nudge = tmp[names(clus_PM10_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_PM10_sp))
names(tmp)=names(sort(clus_PM10_sp))
y_nudge = tmp[names(clus_PM10_sp)]

pdf(paste(path_to_figure, "lasso_volcano_PM10_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_PM10_betas, clus_PM10_sp, las=1, pch=19,
     col=ifelse(abs(clus_PM10_sp) < 0.9,"grey","skyblue"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "skyblue",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(clus_PM10_betas+x_nudge, clus_PM10_sp+y_nudge,
     labels = ifelse(abs(clus_PM10_sp) < 0.9,"",names(clus_PM10_sp)),
     cex = 0.4)
dev.off()
sum(clus_PM10_sp>=0.9)

## PM2.5 ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_PM2.5_betas))
names(tmp)=names(sort(clus_PM2.5_betas))
x_nudge = tmp[names(clus_PM2.5_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_PM2.5_sp))
names(tmp)=names(sort(clus_PM2.5_sp))
y_nudge = tmp[names(clus_PM2.5_sp)]

pdf(paste(path_to_figure, "lasso_volcano_PM2.5_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_PM2.5_betas, clus_PM2.5_sp, las=1, pch=19,
     col=ifelse(abs(clus_PM2.5_sp) < 0.9,"grey","lightgreen"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgreen",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(clus_PM2.5_betas+x_nudge, clus_PM2.5_sp+y_nudge,
     labels = ifelse(abs(clus_PM2.5_sp) < 0.9,"",names(clus_PM2.5_sp)),
     cex = 0.4)
dev.off()

## NOx ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_NOx_betas))
names(tmp)=names(sort(clus_NOx_betas))
x_nudge = tmp[names(clus_NOx_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_NOx_sp))
names(tmp)=names(sort(clus_NOx_sp))
y_nudge = tmp[names(clus_NOx_sp)]

pdf(paste(path_to_figure, "lasso_volcano_PM2.5_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_NOx_betas, clus_NOx_sp, las=1, pch=19,
     col=ifelse(abs(clus_NOx_sp) < 0.9,"grey","lightgreen"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgreen",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(clus_NOx_betas+x_nudge, clus_NOx_sp+y_nudge,
     labels = ifelse(abs(clus_NOx_sp) < 0.9,"",names(clus_NOx_sp)),
     cex = 0.4)
dev.off()

## major road ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_road_betas))
names(tmp)=names(sort(clus_road_betas))
x_nudge = tmp[names(clus_road_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_road_sp))
names(tmp)=names(sort(clus_road_sp))
y_nudge = tmp[names(clus_road_sp)]

pdf(paste(path_to_figure, "lasso_volcano_road_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_road_betas, clus_road_sp, las=1, pch=19,
     col=ifelse(abs(clus_road_sp) < 0.9,"grey","lightgreen"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgreen",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(clus_road_betas+x_nudge, clus_road_sp+y_nudge,
     labels = ifelse(abs(clus_road_sp) < 0.9,"",names(clus_road_sp)),
     cex = 0.4)
dev.off()
sum(clus_road_sp>=0.9)

# ---------------------Correlation pattern----------------------------------------------------
sp_NO2 = clus_NO2_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_NO2)[2]) 
sp_NO2_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_NO2)))
mycor = cor(sp_NO2_df)
pdf(paste(path_to_figure, "corr_0.9_NO2.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_NO2_sp>=Argmax(out_clus_NO2)[2]) 

sp = clus_PM10_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_PM10)[2]) 
sp_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_0.9_PM10.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_PM10_sp>=Argmax(out_clus_PM10)[2]) 


sp = clus_PM2.5_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_PM2.5)[2]) 
sp_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_0.9_PM2.5.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F, 
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_PM2.5_sp>=Argmax(out_clus_PM2.5)[2]) 

sp = clus_abs_sp %>% data.frame() %>% filter (. >= 0.9) 
sp_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_0.9_abs.pdf", sep =""),width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()

sp = clus_road_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_road)[2]) 
sp_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_0.9_road.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_road_sp>=Argmax(out_clus_road)[2]) 

sum(combine_road_sp>=Argmax(out_combine_road)[2]) 
sum(bio_road_sp>=Argmax(out_bio_road)[2])

# -----------------------Clustered Metabolites Sel prop plots--------------------------------------------------
## Visualisation of selection proportions by stability selection models 

## NO2 ##
clus_NO2_sp=sort(clus_NO2_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_NO2.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(15,5,1,1))
plot(clus_NO2_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(clus_NO2_sp>=Argmax(out_clus_NO2)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of clustered metabolites with NO2", cex.main=0.8)
abline(h=Argmax(out_clus_NO2)[2], lty=2, col="darkred")
for (i in 1:length(clus_NO2_sp)){
  axis(side=1, at=i, labels=names(clus_NO2_sp)[i], las=2, cex.axis=0.4,
       col=ifelse(clus_NO2_sp[i]>=Argmax(out_clus_NO2)[2], yes="red", no="grey"),
       col.axis=ifelse(clus_NO2_sp[i]>=Argmax(out_clus_NO2)[2], yes="red", no="grey"))
}
dev.off()

## PM10 ##
clus_PM10_sp=sort(clus_PM10_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_PM10.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(15,5,1,1))
plot(clus_PM10_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(clus_PM10_sp>=Argmax(out_clus_PM10)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of clustered metabolites with PM10",cex.main=0.8)
abline(h=Argmax(out_clus_PM10)[2], lty=2, col="darkred")
for (i in 1:length(clus_PM10_sp)){
  axis(side=1, at=i, labels=names(clus_PM10_sp)[i], las=2, cex.axis=0.4,
       col=ifelse(clus_PM10_sp[i]>=Argmax(out_clus_PM10)[2], yes="red", no="grey"),
       col.axis=ifelse(clus_PM10_sp[i]>=Argmax(out_clus_PM10)[2], yes="red", no="grey"))
}
dev.off()


## PM2.5 ##
clus_PM2.5_sp=sort(clus_PM2.5_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_PM2.5.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(15,5,1,1))
plot(clus_PM2.5_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(clus_PM2.5_sp>=Argmax(out_clus_PM2.5)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of clustered metabolites with PM2.5",cex.main=0.8)
abline(h=Argmax(out_clus_PM2.5)[2], lty=2, col="darkred")
for (i in 1:length(clus_PM2.5_sp)){
  axis(side=1, at=i, labels=names(clus_PM2.5_sp)[i], las=2, cex.axis=0.4,
       col=ifelse(clus_PM2.5_sp[i]>=Argmax(out_clus_PM2.5)[2], yes="red", no="grey"),
       col.axis=ifelse(clus_PM2.5_sp[i]>=Argmax(out_clus_PM2.5)[2], yes="red", no="grey"))
}
dev.off()

## Road ##
clus_road_sp=sort(clus_road_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_road.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(15,5,1,1))
plot(clus_road_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(clus_road_sp>=Argmax(out_clus_road)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions",
     main = "Selection proportion of clustered metabolites with closesness to major road",cex.main=0.8)
abline(h=Argmax(out_clus_road)[2], lty=2, col="darkred")
for (i in 1:length(clus_road_sp)){
  axis(side=1, at=i, labels=names(clus_road_sp)[i], las=2, cex.axis=0.4,
       col=ifelse(clus_road_sp[i]>=Argmax(out_clus_road)[2], yes="red", no="grey"),
       col.axis=ifelse(clus_road_sp[i]>=Argmax(out_clus_road)[2], yes="red", no="grey"))
}
dev.off()


# Biomarkers Sel prop plots -----------------------------------------------

## NO2 ##
bio_NO2_sp=sort(bio_NO2_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_NO2_bio.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(bio_NO2_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(bio_NO2_sp>=Argmax(out_bio_NO2)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of biomarkers with NO2", cex.main=0.8)
abline(h=Argmax(out_bio_NO2)[2], lty=2, col="darkred")
for (i in 1:length(bio_NO2_sp)){
  axis(side=1, at=i, labels=names(bio_NO2_sp)[i], las=2, cex.axis=0.5,
       col=ifelse(bio_NO2_sp[i]>=Argmax(out_bio_NO2)[2], yes="red", no="grey"),
       col.axis=ifelse(bio_NO2_sp[i]>=Argmax(out_bio_NO2)[2], yes="red", no="grey"))
}
dev.off()

## PM10 ##
bio_PM10_sp=sort(bio_PM10_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_PM10_bio.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(bio_PM10_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(bio_PM10_sp>=Argmax(out_bio_PM10)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of biomarkers with PM10",cex.main=0.8)
abline(h=Argmax(out_bio_PM10)[2], lty=2, col="darkred")
for (i in 1:length(bio_PM10_sp)){
  axis(side=1, at=i, labels=names(bio_PM10_sp)[i], las=2, cex.axis=0.5,
       col=ifelse(bio_PM10_sp[i]>=Argmax(out_bio_PM10)[2], yes="red", no="grey"),
       col.axis=ifelse(bio_PM10_sp[i]>=Argmax(out_bio_PM10)[2], yes="red", no="grey"))
}
dev.off()


## PM2.5 ##
bio_PM2.5_sp=sort(bio_PM2.5_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_PM2.5_bio.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(bio_PM2.5_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(bio_PM2.5_sp>=Argmax(out_bio_PM2.5)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of biomarkers with PM2.5",cex.main=0.8)
abline(h=Argmax(out_bio_PM2.5)[2], lty=2, col="darkred")
for (i in 1:length(bio_PM2.5_sp)){
  axis(side=1, at=i, labels=names(bio_PM2.5_sp)[i], las=2, cex.axis=0.5,
       col=ifelse(bio_PM2.5_sp[i]>=Argmax(out_bio_PM2.5)[2], yes="red", no="grey"),
       col.axis=ifelse(bio_PM2.5_sp[i]>=Argmax(out_bio_PM2.5)[2], yes="red", no="grey"))
}
dev.off()

## Road ##
bio_road_sp=sort(bio_road_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_road_bio.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(bio_road_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(bio_road_sp>=Argmax(out_bio_road)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions",
     main = "Selection proportion of biomarkers with closesness to major road",cex.main=0.8)
abline(h=Argmax(out_bio_road)[2], lty=2, col="darkred")
for (i in 1:length(bio_road_sp)){
  axis(side=1, at=i, labels=names(bio_road_sp)[i], las=2, cex.axis=0.5,
       col=ifelse(bio_road_sp[i]>=Argmax(out_bio_road)[2], yes="red", no="grey"),
       col.axis=ifelse(bio_road_sp[i]>=Argmax(out_bio_road)[2], yes="red", no="grey"))
}
dev.off()


# Combined sel prop plots -------------------------------------------------

## NO2 ##
combine_NO2_sp=sort(combine_NO2_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_NO2_combine.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(15,5,1,1))
plot(combine_NO2_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(combine_NO2_sp>=Argmax(out_combine_NO2)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of clustered metabolites and biomarkers with NO2", cex.main=0.6)
abline(h=Argmax(out_combine_NO2)[2], lty=2, col="darkred")
for (i in 1:length(combine_NO2_sp)){
  axis(side=1, at=i, labels=names(combine_NO2_sp)[i], las=2, cex.axis=0.4,
       col=ifelse(combine_NO2_sp[i]>=Argmax(out_combine_NO2)[2], yes="red", no="grey"),
       col.axis=ifelse(combine_NO2_sp[i]>=Argmax(out_combine_NO2)[2], yes="red", no="grey"))
}
dev.off()

## PM10 ##
combine_PM10_sp=sort(combine_PM10_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_PM10_combine.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(15,5,1,1))
plot(combine_PM10_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(combine_PM10_sp>=Argmax(out_combine_PM10)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of clustered metabolites and biomarkers with PM10",cex.main=0.6)
abline(h=Argmax(out_combine_PM10)[2], lty=2, col="darkred")
for (i in 1:length(combine_PM10_sp)){
  axis(side=1, at=i, labels=names(combine_PM10_sp)[i], las=2, cex.axis=0.4,
       col=ifelse(combine_PM10_sp[i]>=Argmax(out_combine_PM10)[2], yes="red", no="grey"),
       col.axis=ifelse(combine_PM10_sp[i]>=Argmax(out_combine_PM10)[2], yes="red", no="grey"))
}
dev.off()


## PM2.5 ##
combine_PM2.5_sp=sort(combine_PM2.5_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_PM2.5_combine.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(15,5,1,1))
plot(combine_PM2.5_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(combine_PM2.5_sp>=Argmax(out_combine__PM2.5)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of clustered metabolites and biomarkers with PM2.5",cex.main=0.6)
abline(h=Argmax(out_combine__PM2.5)[2], lty=2, col="darkred")
for (i in 1:length(combine_PM2.5_sp)){
  axis(side=1, at=i, labels=names(combine_PM2.5_sp)[i], las=2, cex.axis=0.4,
       col=ifelse(combine_PM2.5_sp[i]>=Argmax(out_combine__PM2.5)[2], yes="red", no="grey"),
       col.axis=ifelse(combine_PM2.5_sp[i]>=Argmax(out_combine__PM2.5)[2], yes="red", no="grey"))
}
dev.off()

## Road ##
combine_road_sp=sort(combine_road_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_road_combine.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(15,5,1,1))
plot(combine_road_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(combine_road_sp>=Argmax(out_combine_road)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions",
     main = "Selection proportion of clustered metabolites and biomarkers with closesness to major road",cex.main=0.5)
abline(h=Argmax(out_combine_road)[2], lty=2, col="darkred")
for (i in 1:length(combine_road_sp)){
  axis(side=1, at=i, labels=names(combine_road_sp)[i], las=2, cex.axis=0.4,
       col=ifelse(combine_road_sp[i]>=Argmax(out_combine_road)[2], yes="red", no="grey"),
       col.axis=ifelse(combine_road_sp[i]>=Argmax(out_combine_road)[2], yes="red", no="grey"))
}
dev.off()


# --------# making the legend for combined #-----------------------------------------------------------------
combine = readRDS(paste(path, "Results_report/updates/combined_biometab.rds", sep=""))
combine = combine[order(as.numeric(row.names(combine))),] 
combine_col = colnames(combine) %>% as.data.frame()
biomarker = combine_col[1:28,] %>% as.data.frame()
biomarker$dataset<-rep("biomarker",times=nrow(biomarker))
colnames(biomarker)[1] = "features"
metab = combine_col[29:78,] %>% as.data.frame()
metab$dataset<-rep("metab",times=nrow(metab))
colnames(metab)[1] = "features"
combine_col<-rbind(biomarker,metab)
combine_col$features <- chartr(".", " ", combine_col$features)
combine_col$number = rownames(combine_col)
bio_col = combine_col[1:28,]
metab_col = combine_col[29:78,]





# ---------------ggplot combining all ----------------------------------------------------------

## NO2 ## 
clus_NO2_sp = as.data.frame(clus_NO2_sp)
bio_NO2_sp = as.data.frame(bio_NO2_sp)
combine_NO2_sp = as.data.frame(combine_NO2_sp)

colnames(clus_NO2_sp)[1] = "sp"
colnames(bio_NO2_sp)[1] = "sp"
colnames(combine_NO2_sp)[1] = "sp"

clus_NO2_sp$type<-rep("Metabolites",times=nrow(clus_NO2_sp))
bio_NO2_sp$type<-rep("Biomarkers",times=nrow(bio_NO2_sp))
combine_NO2_sp$type<-rep("Combined",times=nrow(combine_NO2_sp))

clus_NO2_sp$encoding<-rownames(clus_NO2_sp)
bio_NO2_sp$encoding<-rownames(bio_NO2_sp)
combine_NO2_sp$encoding<-rownames(combine_NO2_sp)

selection<-rbind(clus_NO2_sp,bio_NO2_sp,combine_NO2_sp)
selection$encoding <- chartr(".", " ", selection$encoding)
mygrep = selection %>% group_by(encoding) %>% filter(sp>0.9) %>% .$encoding
selection = selection[which(selection$encoding %in% mygrep),]

#optimal selection proportion for each
clus_NO2_op_sp<-Argmax(out_clus_NO2)[2]
bio_NO2_op_sp<-Argmax(out_bio_NO2)[2]
combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]

# manual colour 
mycolour = c("brown","green4", "blue")
selection$text_colour = ifelse(selection$type =="Metabolites", 'blue','brown')
text_colour =selection$text_colour[order(selection$sp,decreasing = T)]


ggplot(selection,aes(x=reorder(encoding,sp),
                          y=sp,fill=type)) +
  geom_bar(stat="identity", position="dodge", alpha=0.6, size=0.15, width = 0.7) +
  theme_bw()+
  #ggtitle(paste("Selection proportions"))+
  ylab("Selection proportion")+
  xlab("")+
  # facet_grid(str_wrap(group, width = 10)~., scales = "free", space = "free",  drop = T)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=13, colour = ifelse(selection$type == 'Metabolites', 'Blue', 'Brown')),
        strip.text.y.left = element_text(size=10,angle = 0),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill="#ededed"),
        panel.spacing = unit(0, "lines"))+
  theme(
    legend.title = element_text(size=10),
    legend.text = element_text(size =10)
  )+
  scale_fill_manual(name = "", 
                    labels = str_wrap(
                      c("Biomarkers alone", 
                        "Combined",
                        "Clustered Metabolites alone"), width = 15),values = mycolour)+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(hjust = 0.5, size=14),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=14))+
  theme(panel.grid.minor = element_line(size = 0), panel.grid.major = element_line(size = 0))+
  geom_tile(aes(x = factor(encoding), y = 0, height = Inf, width = 0.75), data = selection, alpha = 0.05) + 
  coord_flip(ylim=c(0,1))+ 
  geom_hline(yintercept = c(clus_NO2_op_sp,bio_NO2_op_sp,combine_NO2_op_sp), linetype="dashed", color = c("skyblue","green","red"))

axis_text_color(p1)


ggsave(height=18, width=16,path = path_to_figure, filename="all_sp_NO2.pdf")


## PM10 ## 
clus_PM10_sp = as.data.frame(clus_PM10_sp)
bio_PM10_sp = as.data.frame(bio_PM10_sp)
combine_PM10_sp = as.data.frame(combine_PM10_sp)

colnames(clus_PM10_sp)[1] = "sp"
colnames(bio_PM10_sp)[1] = "sp"
colnames(combine_PM10_sp)[1] = "sp"

clus_PM10_sp$type<-rep("Metabolites",times=nrow(clus_PM10_sp))
bio_PM10_sp$type<-rep("Biomarkers",times=nrow(bio_PM10_sp))
combine_PM10_sp$type<-rep("Combined",times=nrow(combine_PM10_sp))

clus_PM10_sp$encoding<-rownames(clus_PM10_sp)
bio_PM10_sp$encoding<-rownames(bio_PM10_sp)
combine_PM10_sp$encoding<-rownames(combine_PM10_sp)

selection<-rbind(clus_PM10_sp,bio_PM10_sp,combine_PM10_sp)
selection$encoding <- chartr(".", " ", selection$encoding)
mygrep = selection %>% group_by(encoding) %>% filter(sp>0.9) %>% .$encoding
selection = selection[which(selection$encoding %in% mygrep),]

#optimal selection proportion for each

clus_PM10_op_sp<-Argmax(out_clus_PM10)[2]
bio_PM10_op_sp<-Argmax(out_bio_PM10)[2]
combine_PM10_op_sp<-Argmax(out_combine_PM10)[2]

threshold<-max(clus_PM10_op_sp,bio_PM10_op_sp,combine_PM10_op_sp)


ggplot(data=selection,aes(x=reorder(encoding,sp),
                          y=sp,fill=type)) +
  geom_bar(stat="identity", position="dodge", alpha=0.6, size=0.15, width = 0.7) +
  theme_bw()+
  #ggtitle(paste("Selection proportions"))+
  ylab("Selection proportion")+
  xlab("")+
  # facet_grid(str_wrap(group, width = 10)~., scales = "free", space = "free",  drop = T)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=14),
        strip.text.y.left = element_text(size=14,angle = 0),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill="#ededed"),
        panel.spacing = unit(0, "lines"))+
  theme(
    legend.title = element_text(size=10),
    legend.text = element_text(size =10)
  )+
  scale_fill_discrete(name = "", 
                      labels = str_wrap(
                        c("Biomarkers alone", 
                          "Combined",
                          "Clustered Metabolites alone"), width = 15))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(hjust = 0.5, size=14),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=14))+
  theme(panel.grid.minor = element_line(size = 0), panel.grid.major = element_line(size = 0))+
  geom_tile(aes(x = factor(encoding), y = 0, height = Inf, width = 0.75), data = selection, alpha = 0.05) + 
  coord_flip(ylim=c(0,1))+ 
  geom_hline(yintercept = c(clus_PM10_op_sp,bio_PM10_op_sp,combine_PM10_op_sp), linetype="dashed", color = c("skyblue","green","red"))+
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp_PM10.pdf")


## PM2.5 ## 
clus_PM2.5_sp = as.data.frame(clus_PM2.5_sp)
bio_PM2.5_sp = as.data.frame(bio_PM2.5_sp)
combine_PM2.5_sp = as.data.frame(combine_PM2.5_sp)

colnames(clus_PM2.5_sp)[1] = "sp"
colnames(bio_PM2.5_sp)[1] = "sp"
colnames(combine_PM2.5_sp)[1] = "sp"

clus_PM2.5_sp$type<-rep("Metabolites",times=nrow(clus_PM2.5_sp))
bio_PM2.5_sp$type<-rep("Biomarkers",times=nrow(bio_PM2.5_sp))
combine_PM2.5_sp$type<-rep("Combined",times=nrow(combine_PM2.5_sp))

clus_PM2.5_sp$encoding<-rownames(clus_PM2.5_sp)
bio_PM2.5_sp$encoding<-rownames(bio_PM2.5_sp)
combine_PM2.5_sp$encoding<-rownames(combine_PM2.5_sp)

selection<-rbind(clus_PM2.5_sp,bio_PM2.5_sp,combine_PM2.5_sp)
selection$encoding <- chartr(".", " ", selection$encoding)
mygrep = selection %>% group_by(encoding) %>% filter(sp>0.9) %>% .$encoding
selection = selection[which(selection$encoding %in% mygrep),]


#optimal selection proportion for each

clus_PM2.5_op_sp<-Argmax(out_clus_PM2.5)[2]
bio_PM2.5_op_sp<-Argmax(out_bio_PM2.5)[2]
combine_PM2.5_op_sp<-Argmax(out_combine__PM2.5)[2]


ggplot(data=selection,aes(x=reorder(encoding,sp),
                          y=sp,fill=type)) +
  geom_bar(stat="identity", position="dodge", alpha=0.6, size=0.15, width = 0.7) +
  theme_bw()+
  #ggtitle(paste("Selection proportions"))+
  ylab("Selection proportion")+
  xlab("")+
  # facet_grid(str_wrap(group, width = 10)~., scales = "free", space = "free",  drop = T)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=14),
        strip.text.y.left = element_text(size=10,angle = 0),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill="#ededed"),
        panel.spacing = unit(0, "lines"))+
  theme(
    legend.title = element_text(size=10),
    legend.text = element_text(size =10)
  )+
  scale_fill_discrete(name = "", 
                      labels = str_wrap(
                        c("Biomarkers alone", 
                          "Combined",
                          "Clustered Metabolites alone"), width = 15))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(hjust = 0.5, size=14),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=14))+
  theme(panel.grid.minor = element_line(size = 0), panel.grid.major = element_line(size = 0))+
  geom_tile(aes(x = factor(encoding), y = 0, height = Inf, width = 0.75), data = selection, alpha = 0.05) + 
  coord_flip(ylim=c(0,1))+ 
  geom_hline(yintercept = c(clus_PM2.5_op_sp,bio_PM2.5_op_sp,combine_PM2.5_op_sp), linetype="dashed", color = c("skyblue","green","red")) +
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp_PM2.5.pdf")

## PM2.5_absorbance ## 
clus_abs_sp = as.data.frame(clus_abs_sp)
bio_abs_sp = as.data.frame(bio_abs_sp)
combine_abs_sp = as.data.frame(combine_abs_sp)

colnames(clus_abs_sp)[1] = "sp"
colnames(bio_abs_sp)[1] = "sp"
colnames(combine_abs_sp)[1] = "sp"

clus_abs_sp$type<-rep("Metabolites",times=nrow(clus_abs_sp))
bio_abs_sp$type<-rep("Biomarkers",times=nrow(bio_abs_sp))
combine_abs_sp$type<-rep("Combined",times=nrow(combine_abs_sp))

clus_abs_sp$encoding<-rownames(clus_abs_sp)
bio_abs_sp$encoding<-rownames(bio_abs_sp)
combine_abs_sp$encoding<-rownames(combine_abs_sp)

selection<-rbind(clus_abs_sp,bio_abs_sp,combine_abs_sp)
selection$encoding <- chartr(".", " ", selection$encoding)
mygrep = selection %>% group_by(encoding) %>% filter(sp>0.9) %>% .$encoding
selection = selection[which(selection$encoding %in% mygrep),]


#optimal selection proportion for each

clus_abs_op_sp<-Argmax(out_clus_abs)[2]
bio_abs_op_sp<-Argmax(out_bio_abs)[2]
combine_abs_op_sp<-Argmax(out_combine_abs)[2]

threshold<-max(clus_PM2.5_op_sp,bio_PM2.5_op_sp,combine_PM2.5_op_sp)


ggplot(data=selection,aes(x=reorder(encoding,sp),
                          y=sp,fill=type)) +
  geom_bar(stat="identity", position="dodge", alpha=0.6, size=0.15, width = 0.7) +
  theme_bw()+
  #ggtitle(paste("Selection proportions"))+
  ylab("Selection proportion")+
  xlab("")+
  # facet_grid(str_wrap(group, width = 10)~., scales = "free", space = "free",  drop = T)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=14),
        strip.text.y.left = element_text(size=10,angle = 0),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill="#ededed"),
        panel.spacing = unit(0, "lines"))+
  theme(
    legend.title = element_text(size=10),
    legend.text = element_text(size =10)
  )+
  scale_fill_discrete(name = "", 
                      labels = str_wrap(
                        c("Biomarkers alone", 
                          "Combined",
                          "Clustered Metabolites alone"), width = 15))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(hjust = 0.5, size=14),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=14))+
  theme(panel.grid.minor = element_line(size = 0), panel.grid.major = element_line(size = 0))+
  geom_tile(aes(x = factor(encoding), y = 0, height = Inf, width = 0.75), data = selection, alpha = 0.05) + 
  coord_flip(ylim=c(0,1))+ 
  geom_hline(yintercept = c(clus_abs_op_sp,bio_abs_op_sp,combine_abs_op_sp), linetype="dashed", color = c("skyblue","green","red")) +
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp_abs.pdf")


## Major road ## 
clus_road_sp = as.data.frame(clus_road_sp)
bio_road_sp = as.data.frame(bio_road_sp)
combine_road_sp = as.data.frame(combine_road_sp)

colnames(clus_road_sp)[1] = "sp"
colnames(bio_road_sp)[1] = "sp"
colnames(combine_road_sp)[1] = "sp"

clus_road_sp$type<-rep("Metabolites",times=nrow(clus_road_sp))
bio_road_sp$type<-rep("Biomarkers",times=nrow(bio_road_sp))
combine_road_sp$type<-rep("Combined",times=nrow(combine_road_sp))

clus_road_sp$encoding<-rownames(clus_road_sp)
bio_road_sp$encoding<-rownames(bio_road_sp)
combine_road_sp$encoding<-rownames(combine_road_sp)

selection<-rbind(clus_road_sp,bio_road_sp,combine_road_sp)
selection$encoding <- chartr(".", " ", selection$encoding)
mygrep = selection %>% group_by(encoding) %>% filter(sp>0.9) %>% .$encoding
selection = selection[which(selection$encoding %in% mygrep),]


#optimal selection proportion for each

clus_road_op_sp<-Argmax(out_clus_road)[2]
bio_road_op_sp<-Argmax(out_bio_road)[2]
combine_road_op_sp<-Argmax(out_combine_road)[2]

threshold<-max(clus_road_op_sp,bio_road_op_sp,combine_road_op_sp)

ggplot(data=selection,aes(x=reorder(encoding,sp),
                          y=sp,fill=type)) +
  geom_bar(stat="identity", position="dodge", alpha=0.6, size=0.15, width = 0.7) +
  theme_bw()+
  #ggtitle(paste("Selection proportions"))+
  ylab("Selection proportion")+
  xlab("")+
  # facet_grid(str_wrap(group, width = 10)~., scales = "free", space = "free",  drop = T)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=13),
        strip.text.y.left = element_text(size=13,angle = 0),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill="#ededed"),
        panel.spacing = unit(0, "lines"))+
  theme(
    legend.title = element_text(size=10),
    legend.text = element_text(size = 10)
  )+
  scale_fill_discrete(name = "", 
                      labels = str_wrap(
                        c("Biomarkers alone", 
                          "Combined",
                          "Clustered Metabolites alone"), width = 15))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(hjust = 0.5, size=13),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=13))+
  theme(panel.grid.minor = element_line(size = 0), panel.grid.major = element_line(size = 0))+
  geom_tile(aes(x = factor(encoding), y = 0, height = Inf, width = 0.75), data = selection, alpha = 0.05) + 
  coord_flip(ylim=c(0,1))+ 
  geom_hline(yintercept = c(clus_road_op_sp,bio_road_op_sp,combine_road_op_sp), linetype="dashed", color = c("skyblue","green","red")) +
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp_road.pdf")




knitr::purl("13_obj1_lasso_visual.Rmd")



# Calibration plot  -------------------------------------------------------

pdf(paste(path_to_figure,"CalibrationPlot/combine_NO2.pdf",sep =""))
par(mar = c(7, 5, 7, 6))
CalibrationPlot(out_combine_NO2)
dev.off()
Argmax(out_combine_NO2)
Argmax(out_combine_NOx)
pdf(paste(path_to_figure,"CalibrationPlot/combine_PM10.pdf",sep =""))
par(mar = c(7, 5, 7, 6))
CalibrationPlot(out_combine_PM10)
dev.off()
Argmax(out_combine_PM10)

pdf(paste(path_to_figure,"CalibrationPlot/combine_PM2.5.pdf",sep =""))
par(mar = c(7, 5, 7, 6))
CalibrationPlot(out_combine__PM2.5)
dev.off()
Argmax(out_combine__PM2.5)

pdf(paste(path_to_figure,"CalibrationPlot/combine_abs.pdf",sep =""))
par(mar = c(7, 5, 7, 6))
CalibrationPlot(out_combine_abs)
dev.off()
Argmax(out_combine_abs)
Argmax(out_combine_PM2.5_10)

pdf(paste(path_to_figure,"CalibrationPlot/combine_road.pdf",sep =""))
par(mar = c(7, 5, 7, 6))
CalibrationPlot(out_combine_road)
dev.off()
Argmax(out_combine_road)


# Stability coefficients --------------------------------------------------
# Stability selection lasso coefficients 
metab_NO2_betas<-metab_NO2_betas[!is.na(metab_NO2_betas)]
pdf(paste(path_to_figure, "lasso_coef_NO2.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(metab_NO2_betas[metab_NO2_betas !=0], type = "h", col = "navy", lwd = 3,
     xaxt = "n", xlab = "", ylab = expression(beta))
axis(side = 1, at = 1:sum(metab_NO2_betas != 0),labels = names(metab_NO2_betas)[metab_NO2_betas !=
                                                                                  0], las = 2,cex.axis = 0.5)
abline(h = 0, lty = 2)
dev.off()

## PM10
pdf(paste(path_to_figure, "lasso_sp_PM10.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(metab_PM10_sp, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(metab_PM10_sp>=0.9, yes="red", no="grey"), cex.lab=1.5)
abline(h=0.9, lty=2, col="darkred")
for (i in 1:length(metab_PM10_sp)){
  axis(side=1, at=i, labels =names(metab_PM10_sp)[i], las=2,cex.axis = 0.3,
       col=ifelse(metab_PM10_sp[i]>=0.9, yes="red", no="grey"),
       col.axis=ifelse(metab_PM10_sp[i]>=0.9, yes="red", no="grey"))
}
dev.off()

# Stability selection lasso coefficients 
metab_PM10_betas<-metab_PM10_betas[!is.na(metab_PM10_betas)]
pdf(paste(path_to_figure, "lasso_coef_PM10.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(metab_PM10_betas[metab_PM10_betas !=0], type = "h", col = "navy", lwd = 3,
     xaxt = "n", xlab = "", ylab = expression(beta))
axis(side = 1, at = 1:sum(metab_PM10_betas != 0),labels = names(metab_PM10_betas)[metab_PM10_betas !=
                                                                                    0], las = 2,cex.axis = 0.5)
abline(h = 0, lty = 2)
dev.off()


# ------------------Individual Metabolites LASSO selection prop v.s. beta  -------------------------------------------------------

## NO2 ##
tmp = rep(c(0.005,-0.005),length.out = length(metab_NO2_betas))
names(tmp)=names(sort(metab_NO2_betas))
x_nudge = tmp[names(metab_NO2_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(metab_NO2_sp))
names(tmp)=names(sort(metab_NO2_sp))
y_nudge = tmp[names(metab_NO2_sp)]

pdf(paste(path_to_figure, "lasso_volcano_NO2.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(metab_NO2_betas, metab_NO2_sp, las=1, pch=19,
     col=ifelse(abs(metab_NO2_sp) < 0.9,"grey","lightgoldenrod"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgoldenrod",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
spread.labels(metab_NO2_betas+x_nudge, metab_NO2_sp+y_nudge,
              labels = ifelse(abs(metab_NO2_sp) < 1,"",names(metab_NO2_sp)),
              cex = 0.4)
text(metab_NO2_betas+x_nudge, metab_NO2_sp+y_nudge,
     labels = ifelse(abs(metab_NO2_sp) < 1,"",names(metab_NO2_sp)),
     cex = 0.4)
dev.off()

## PM10 ##
tmp = rep(c(0.005,-0.005),length.out = length(metab_PM10_betas))
names(tmp)=names(sort(metab_PM10_betas))
x_nudge = tmp[names(metab_PM10_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(metab_PM10_sp))
names(tmp)=names(sort(metab_PM10_sp))
y_nudge = tmp[names(metab_PM10_sp)]

pdf(paste(path_to_figure, "lasso_volcano_PM10.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(metab_PM10_betas, metab_PM10_sp, las=1, pch=19,
     col=ifelse(abs(metab_PM10_sp) < 0.9,"grey","skyblue"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "skyblue",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
spread.labels(metab_PM10_betas+x_nudge, metab_PM10_sp+y_nudge,
              labels = ifelse(abs(metab_PM10_sp) < 1,"",names(metab_PM10_sp)),
              cex = 0.4)
text(metab_PM10_betas+x_nudge, metab_PM10_sp+y_nudge,
     labels = ifelse(abs(metab_PM10_sp) < 1,"",names(metab_PM10_sp)),
     cex = 0.4)
dev.off()

## PM2.5 ##
tmp = rep(c(0.005,-0.005),length.out = length(metab_PM2.5_betas))
names(tmp)=names(sort(metab_PM2.5_betas))
x_nudge = tmp[names(metab_PM2.5_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(metab_PM2.5_sp))
names(tmp)=names(sort(metab_PM2.5_sp))
y_nudge = tmp[names(metab_PM2.5_sp)]

pdf(paste(path_to_figure, "lasso_volcano_PM2.5.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(metab_PM2.5_betas, metab_PM2.5_sp, las=1, pch=19,
     col=ifelse(abs(metab_PM2.5_sp) < 0.9,"grey","lightgreen"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgreen",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(metab_PM2.5_betas+x_nudge, metab_PM2.5_sp+y_nudge,
     labels = ifelse(abs(metab_PM2.5_sp) < 0.9,"",names(metab_PM2.5_sp)),
     cex = 0.4)
dev.off()

## NOx ##
tmp = rep(c(0.005,-0.005),length.out = length(metab_NOx_betas))
names(tmp)=names(sort(metab_NOx_betas))
x_nudge = tmp[names(metab_NOx_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(metab_NOx_sp))
names(tmp)=names(sort(metab_NOx_sp))
y_nudge = tmp[names(metab_NOx_sp)]

pdf(paste(path_to_figure, "lasso_volcano_PM2.5.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(metab_NOx_betas, metab_NOx_sp, las=1, pch=19,
     col=ifelse(abs(metab_NOx_sp) < 0.9,"grey","lightgreen"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgreen",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(metab_NOx_betas+x_nudge, metab_NOx_sp+y_nudge,
     labels = ifelse(abs(metab_NOx_sp) < 0.9,"",names(metab_NOx_sp)),
     cex = 0.4)
dev.off()

## major road ##
tmp = rep(c(0.005,-0.005),length.out = length(metab_road_betas))
names(tmp)=names(sort(metab_road_betas))
x_nudge = tmp[names(metab_road_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(metab_road_sp))
names(tmp)=names(sort(metab_road_sp))
y_nudge = tmp[names(metab_road_sp)]

pdf(paste(path_to_figure, "lasso_volcano_road.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(metab_road_betas, metab_road_sp, las=1, pch=19,
     col=ifelse(abs(metab_road_sp) < 0.9,"grey","lightgreen"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgreen",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(metab_road_betas+x_nudge, metab_road_sp+y_nudge,
     labels = ifelse(abs(metab_road_sp) < 0.9,"",names(metab_road_sp)),
     cex = 0.4)
dev.off()
sum(metab_road_sp>=0.9)



