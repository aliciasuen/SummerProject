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
met_no_bio = T
met_base = F
met_first= F

## -----------------------------------------------------------------------------------------------------------------------
if(met_no_bio){
  biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/Air_clus_bio/sPLS/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_clus_bio/sPLS/"
  ## sel prop ##
  bio_NO2_sp = readRDS(paste(path_to_multi_results, "selprop_bio_NO2.rds",sep=""))
  bio_PM10_sp = readRDS(paste(path_to_multi_results, "selprop_bio_PM10.rds",sep=""))
  bio_PM2.5_sp = readRDS(paste(path_to_multi_results, "selprop_bio_PM2.5.rds",sep=""))
  bio_NOx_sp = readRDS(paste(path_to_multi_results, "selprop_bio_NOx.rds",sep=""))
  bio_PM2.5_10_sp = readRDS(paste(path_to_multi_results, "selprop_bio_PM2.5_10.rds",sep=""))
  bio_abs_sp = readRDS(paste(path_to_multi_results, "selprop_bio_PM_abs.rds",sep=""))
  bio_road_sp = readRDS(paste(path_to_multi_results, "selprop_bio_road.rds",sep=""))
  clus_NO2_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_NO2.rds",sep=""))
  clus_PM10_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_PM10.rds",sep=""))
  clus_PM2.5_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_PM2.5.rds",sep=""))
  clus_NOx_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_NOx.rds",sep=""))
  clus_PM2.5_10_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_PM2.5_10.rds",sep=""))
  clus_abs_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_PM_abs.rds",sep=""))
  clus_road_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_road.rds",sep=""))
  combine_NO2_sp = readRDS(paste(path_to_multi_results, "selprop_combine_NO2.rds",sep=""))
  combine_PM10_sp = readRDS(paste(path_to_multi_results, "selprop_combine_PM10.rds",sep=""))
  combine_PM2.5_sp = readRDS(paste(path_to_multi_results, "selprop_combine_PM2.5.rds",sep=""))
  combine_NOx_sp = readRDS(paste(path_to_multi_results, "selprop_combine_NOx.rds",sep=""))
  combine_PM2.5_10_sp = readRDS(paste(path_to_multi_results, "selprop_combine_PM2.5_10.rds",sep=""))
  combine_abs_sp = readRDS(paste(path_to_multi_results, "selprop_combine_PM_abs.rds",sep=""))
  combine_road_sp = readRDS(paste(path_to_multi_results, "selprop_combine_road.rds",sep=""))
  ## out ## 
  out_bio_NO2 = readRDS(paste(path_to_multi_results, "out_bio_NO2.rds",sep=""))
  out_bio_PM10 = readRDS(paste(path_to_multi_results, "out_bio_PM10.rds",sep=""))
  out_bio_PM2.5 = readRDS(paste(path_to_multi_results, "out_bio_PM2.5.rds",sep=""))
  out_bio_NOx = readRDS(paste(path_to_multi_results, "out_bio_NOx.rds",sep=""))
  out_bio_PM2.5_10 = readRDS(paste(path_to_multi_results, "out_bio_PM2.5_10.rds",sep=""))
  out_bio_abs = readRDS(paste(path_to_multi_results, "out_bio_PM_abs.rds",sep=""))
  out_bio_road = readRDS(paste(path_to_multi_results, "out_bio_road.rds",sep=""))
  out_clus_NO2 = readRDS(paste(path_to_multi_results, "out_metab_clus_NO2.rds",sep=""))
  out_clus_PM10 = readRDS(paste(path_to_multi_results, "out_metab_clus_PM10.rds",sep=""))
  out_clus_PM2.5 = readRDS(paste(path_to_multi_results, "out_metab_clus_PM2.5.rds",sep=""))
  out_clus_NOx = readRDS(paste(path_to_multi_results, "out_metab_clus_NOx.rds",sep=""))
  out_clus_PM2.5_10 = readRDS(paste(path_to_multi_results, "out_metab_clus_PM2.5_10.rds",sep=""))
  out_clus_abs = readRDS(paste(path_to_multi_results, "out_metab_clus_PM_abs.rds",sep=""))
  out_clus_road = readRDS(paste(path_to_multi_results, "out_metab_clus_road.rds",sep=""))
  out_combine_NO2 = readRDS(paste(path_to_multi_results, "out_combine_NO2.rds",sep=""))
  out_combine_PM10 = readRDS(paste(path_to_multi_results, "out_combine_PM10.rds",sep=""))
  out_combine__PM2.5 = readRDS(paste(path_to_multi_results, "out_combine_PM2.5.rds",sep=""))
  out_combine_NOx = readRDS(paste(path_to_multi_results, "out_combine_NOx.rds",sep=""))
  out_combine_PM2.5_10 = readRDS(paste(path_to_multi_results, "out_combine_PM2.5_10.rds",sep=""))
  out_combine_abs = readRDS(paste(path_to_multi_results, "out_combine_PM_abs.rds",sep=""))
  out_combine_road = readRDS(paste(path_to_multi_results, "out_combine_road.rds",sep=""))
  ## betas ## 
  bio_NO2_betas  = readRDS(paste(path_to_multi_results, "betas_bio_NO2.rds",sep=""))
  bio_PM10_betas  = readRDS(paste(path_to_multi_results, "betas_bio_PM10.rds",sep=""))
  bio_PM2.5_betas  = readRDS(paste(path_to_multi_results, "betas_bio_PM2.5.rds",sep=""))
  bio_NOx_betas  = readRDS(paste(path_to_multi_results, "betas_bio_NOx.rds",sep=""))
  bio_PM2.5_10_betas  = readRDS(paste(path_to_multi_results, "betas_bio_PM2.5_10.rds",sep=""))
  bio_abs_betas  = readRDS(paste(path_to_multi_results, "betas_bio_PM_abs.rds",sep=""))
  bio_road_betas  = readRDS(paste(path_to_multi_results, "betas_bio_road.rds",sep=""))
  clus_NO2_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_NO2.rds",sep=""))
  clus_PM10_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_PM10.rds",sep=""))
  clus_PM2.5_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_PM2.5.rds",sep=""))
  clus_NOx_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_NOx.rds",sep=""))
  clus_PM2.5_10_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_PM2.5_10.rds",sep=""))
  clus_abs_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_PM_abs.rds",sep=""))
  clus_road_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_road.rds",sep=""))
  combine_NO2_betas  = readRDS(paste(path_to_multi_results, "betas_combine_NO2.rds",sep=""))
  combine_PM10_betas  = readRDS(paste(path_to_multi_results, "betas_combine_PM10.rds",sep=""))
  combine_PM2.5_betas  = readRDS(paste(path_to_multi_results, "betas_combine_PM2.5.rds",sep=""))
  combine_NOx_betas  = readRDS(paste(path_to_multi_results, "betas_combine_NOx.rds",sep=""))
  combine_PM2.5_10_betas  = readRDS(paste(path_to_multi_results, "betas_combine_PM2.5_10.rds",sep=""))
  combine_abs_betas  = readRDS(paste(path_to_multi_results, "betas_combine_PM_abs.rds",sep=""))
  combine_road_betas  = readRDS(paste(path_to_multi_results, "betas_combine_road.rds",sep=""))
  
}


if(met_base){
  path_to_uni_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Univariate_obj1/Air_met_base/"
  metab = readRDS(paste(path, "Results/denoised_metab.rds", sep=""))
  metab = metab[order(as.numeric(row.names(metab))), ] 
  metab_clus = readRDS(paste(path, "Results/denoised_clus.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  betas = readRDS(paste(path_to_uni_results, "betas_uni.rds", sep =""))
  pval = readRDS(paste(path_to_uni_results, "pvalues_uni.rds",sep ="")) 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/Air_met_base/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_met_base/"
  ## sel prop ##
  metab_NO2_sp = readRDS(paste(path_to_multi_results, "selprop_metab_NO2.rds",sep=""))
  metab_PM10_sp = readRDS(paste(path_to_multi_results, "selprop_metab_PM10.rds",sep=""))
  metab_PM2.5_sp = readRDS(paste(path_to_multi_results, "selprop_metab_PM2.5.rds",sep=""))
  metab_NOx_sp = readRDS(paste(path_to_multi_results, "selprop_metab_NOx.rds",sep=""))
  metab_PM2.5_10_sp = readRDS(paste(path_to_multi_results, "selprop_metab_PM2.5_10.rds",sep=""))
  metab_abs_sp = readRDS(paste(path_to_multi_results, "selprop_metab_PM_abs.rds",sep=""))
  metab_road_sp = readRDS(paste(path_to_multi_results, "selprop_metab_road.rds",sep=""))
  clus_NO2_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_NO2.rds",sep=""))
  clus_PM10_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_PM10.rds",sep=""))
  clus_PM2.5_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_PM2.5.rds",sep=""))
  clus_NOx_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_NOx.rds",sep=""))
  clus_PM2.5_10_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_PM2.5_10.rds",sep=""))
  clus_abs_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_PM_abs.rds",sep=""))
  clus_road_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_road.rds",sep=""))
  ## out ## 
  out_NO2 = readRDS(paste(path_to_multi_results, "out_metab_NO2.rds",sep=""))
  out_PM10 = readRDS(paste(path_to_multi_results, "out_metab_PM10.rds",sep=""))
  out_PM2.5 = readRDS(paste(path_to_multi_results, "out_metab_PM2.5.rds",sep=""))
  out_NOx = readRDS(paste(path_to_multi_results, "out_metab_NOx.rds",sep=""))
  out_PM2.5_10 = readRDS(paste(path_to_multi_results, "out_metab_PM2.5_10.rds",sep=""))
  out_abs = readRDS(paste(path_to_multi_results, "out_metab_PM_abs.rds",sep=""))
  out_road = readRDS(paste(path_to_multi_results, "out_metab_road.rds",sep=""))
  out_clus_NO2 = readRDS(paste(path_to_multi_results, "out_metab_clus_NO2.rds",sep=""))
  out_clus_PM10 = readRDS(paste(path_to_multi_results, "out_metab_clus_PM10.rds",sep=""))
  out_clus_PM2.5 = readRDS(paste(path_to_multi_results, "out_metab_clus_PM2.5.rds",sep=""))
  out_clus_NOx = readRDS(paste(path_to_multi_results, "out_metab_clus_NOx.rds",sep=""))
  out_clus_PM2.5_10 = readRDS(paste(path_to_multi_results, "out_metab_clus_PM2.5_10.rds",sep=""))
  out_clus_abs = readRDS(paste(path_to_multi_results, "out_metab_clus_PM_abs.rds",sep=""))
  out_clus_road = readRDS(paste(path_to_multi_results, "out_metab_clus_road.rds",sep=""))
  ## betas ## 
  metab_NO2_betas  = readRDS(paste(path_to_multi_results, "betas_stability_NO2.rds",sep=""))
  metab_PM10_betas  = readRDS(paste(path_to_multi_results, "betas_stability_PM10.rds",sep=""))
  metab_PM2.5_betas  = readRDS(paste(path_to_multi_results, "betas_stability_PM2.5.rds",sep=""))
  metab_NOx_betas  = readRDS(paste(path_to_multi_results, "betas_stability_NOx.rds",sep=""))
  metab_PM2.5_10_betas  = readRDS(paste(path_to_multi_results, "betas_stability_PM2.5_10.rds",sep=""))
  metab_abs_betas  = readRDS(paste(path_to_multi_results, "betas_stability_PM_abs.rds",sep=""))
  metab_road_betas  = readRDS(paste(path_to_multi_results, "betas_stability_road.rds",sep=""))
  clus_NO2_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_NO2.rds",sep=""))
  clus_PM10_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_PM10.rds",sep=""))
  clus_PM2.5_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_PM2.5.rds",sep=""))
  clus_NOx_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_NOx.rds",sep=""))
  clus_PM2.5_10_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_PM2.5_10.rds",sep=""))
  clus_abs_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_PM_abs.rds",sep=""))
  clus_road_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_road.rds",sep=""))
  
}

if(met_first){
  path_to_uni_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Univariate_obj1/Air_met_first/"
  betas = readRDS(paste(path_to_uni_results, "betas_uni.rds", sep =""))
  betas = betas[,-1]
  pval = readRDS(paste(path_to_uni_results, "pvalues_uni.rds",sep =""))
  pval = pval[, -1]
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Obj1/Air_met_first/"
}

source(paste(path,"Functions/penalisation_functions.R", sep=""))


# ------------------Clustered Metabolites LASSO selection prop v.s. beta  -------------------------------------------------------

## NO2 ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_NO2_betas))
names(tmp)=names(sort(clus_NO2_betas))
x_nudge = tmp[names(clus_NO2_betas)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_NO2_sp))
names(tmp)=names(sort(clus_NO2_sp))
y_nudge = tmp[names(clus_NO2_sp)]

pdf(paste(path_to_figure, "lasso_volcano_NO2_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_NO2_betas, clus_NO2_sp, las=1, pch=19,
     col=ifelse(abs(clus_NO2_sp) < Argmax(out_clus_NO2)[2],"grey","lightgoldenrod"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgoldenrod",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
spread.labels(clus_NO2_betas+x_nudge, clus_NO2_sp+y_nudge,
     labels = ifelse(abs(clus_NO2_sp) < Argmax(out_clus_NO2)[2],"",names(clus_NO2_sp)),
     cex = 0.4, ony = T)
text(clus_NO2_betas+x_nudge, clus_NO2_sp+y_nudge,
     labels = ifelse(abs(clus_NO2_sp) < Argmax(out_clus_NO2)[2],"",names(clus_NO2_sp)),
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
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_NO2_sp>=Argmax(out_clus_NO2)[2]) 

sp = clus_PM10_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_PM10)[2]) 
sp_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_0.9_PM10.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_PM10_sp>=Argmax(out_clus_PM10)[2]) 


sp = clus_PM2.5_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_PM2.5)[2]) 
sp_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_0.9_PM2.5.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_PM2.5_sp>=Argmax(out_clus_PM2.5)[2]) 

sp = metab_NOx_sp %>% data.frame() %>% filter (. >= 0.9) 
sp_df = metab %>% select(which(colnames(metab) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_0.9_NOx.pdf", sep =""))
pheatmap(mycor, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)
dev.off()

sp = clus_road_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_road)[2]) 
sp_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_0.9_road.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_road_sp>=Argmax(out_clus_road)[2]) 

sum(combine_road_sp>=Argmax(out_combine_road)[2]) 
sum(bio_road_sp>=Argmax(out_bio_road)[2])

# -----------------------Clustered Metabolites Sel prop plots--------------------------------------------------
## Visualisation of selection proportions by stability selection models 

## NO2 ##
clus_NO2_sp=sort(clus_NO2_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "spls_sp_NO2.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_PM10.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_PM2.5.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_road.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_NO2_bio.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_PM10_bio.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_PM2.5_bio.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_road_bio.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_NO2_combine.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_PM10_combine.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_PM2.5_combine.pdf", sep =""), width = 5.5, height = 5.5)
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
pdf(paste(path_to_figure, "spls_sp_road_combine.pdf", sep =""), width = 5.5, height = 5.5)
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
combine = readRDS(paste(path, "Results/combined_biometab.rds", sep=""))
combine = combine[order(as.numeric(row.names(combine))),] 
combine_col = colnames(combine) %>% as.data.frame()
biomarker = combine_col[1:28,] %>% as.data.frame()
biomarker$dataset<-rep("biomarker",times=nrow(biomarker))
colnames(biomarker)[1] = "features"
metab = combine_col[29:74,] %>% as.data.frame()
metab$dataset<-rep("metab",times=nrow(metab))
colnames(metab)[1] = "features"
combine_col<-rbind(biomarker,metab)
combine_col$features <- chartr(".", " ", combine_col$features)
combine_col$number = rownames(combine_col)
bio_col = combine_col[1:28,]
metab_col = combine_col[29:74,]






# -------------------------------------------------------------------------
spls = T

if(spls){
  biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/Air_clus_bio/sPLS/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_clus_bio/sPLS/"
  path_to_lasso_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_clus_bio/lasso/pfer/"
  ## sel prop ##
  bio_NO2_sp = readRDS(paste(path_to_multi_results, "selprop_bio_NO2.rds",sep=""))
  clus_NO2_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_NO2.rds",sep=""))
  combine_NO2_sp = readRDS(paste(path_to_multi_results, "selprop_combine_NO2.rds",sep=""))
  ## out ## 
  out_bio_NO2 = readRDS(paste(path_to_multi_results, "out_bio_NO2.rds",sep=""))
  out_clus_NO2 = readRDS(paste(path_to_multi_results, "out_metab_clus_NO2.rds",sep=""))
  out_combine_NO2 = readRDS(paste(path_to_multi_results, "out_combine_NO2.rds",sep=""))
  ## betas ## 
  bio_NO2_betas  = readRDS(paste(path_to_multi_results, "betas_bio_NO2.rds",sep=""))
  clus_NO2_betas  = readRDS(paste(path_to_multi_results, "betas_stability_clus_NO2.rds",sep=""))
  combine_NO2_betas  = readRDS(paste(path_to_multi_results, "betas_combine_NO2.rds",sep=""))  
  ## sel prop ##
  bio_NO2_sp_lasso = readRDS(paste(path_to_lasso_results, "selprop_bio_NO2.rds",sep=""))
  clus_NO2_sp_lasso = readRDS(paste(path_to_lasso_results, "selprop_metab_clus_NO2.rds",sep=""))
  combine_NO2_sp_lasso = readRDS(paste(path_to_lasso_results, "selprop_combine_NO2.rds",sep=""))
  ## out ## 
  out_bio_NO2_lasso = readRDS(paste(path_to_lasso_results, "out_bio_NO2.rds",sep=""))
  out_clus_NO2_lasso = readRDS(paste(path_to_lasso_results, "out_metab_clus_NO2.rds",sep=""))
  out_combine_NO2_lasso = readRDS(paste(path_to_lasso_results, "out_combine_NO2.rds",sep=""))
  ## betas ## 
  bio_NO2_betas_lasso  = readRDS(paste(path_to_lasso_results, "betas_bio_NO2.rds",sep=""))
  clus_NO2_betas_lasso  = readRDS(paste(path_to_lasso_results, "betas_stability_clus_NO2.rds",sep=""))
  combine_NO2_betas_lasso  = readRDS(paste(path_to_lasso_results, "betas_combine_NO2.rds",sep=""))
}










# Compared sPLS v.s. LASSO ------------------------------------------------

combine_NO2_sp = as.data.frame(combine_NO2_sp)
colnames(combine_NO2_sp)[1] = "sp"
combine_NO2_sp$encoding<-rownames(combine_NO2_sp)
combine_NO2_sp$type<-rep("sPLS",times=nrow(combine_NO2_sp))

combine_NO2_sp_lasso = as.data.frame(combine_NO2_sp_lasso)
colnames(combine_NO2_sp_lasso)[1] = "sp"
combine_NO2_sp_lasso$encoding<-rownames(combine_NO2_sp_lasso)
combine_NO2_sp_lasso$type<-rep("LASSO",times=nrow(combine_NO2_sp_lasso))

selection = cbind(combine_NO2_sp,combine_NO2_sp_lasso)
colnames(selection)[1] = "sp_spls"
colnames(selection)[4] = "sp_lasso"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,46))
combine_spls_op_sp<-Argmax(out_combine_NO2)[2]
combine_lasso_op_sp<-Argmax(out_combine_NO2_lasso)[2]
selection = selection %>% mutate(selection$newcolour,ifelse(sp_spls<combine_spls_op_sp & sp_lasso<combine_lasso_op_sp, "grey",mycolour_point))
colnames(selection)[6] = "newcolour"
# sel_lasso_1$var_cat=variable_cat
# sel_lasso_1$label=1:(nrow(sel_lasso_1))
# sel_lasso_1$mycolour_lab=darken(sel_lasso_1$mycolour_point, amount=0.5)



p1=ggplot(selection,
          aes(sp_lasso, sp_spls, label=ifelse((sp_lasso < combine_lasso_op_sp & sp_spls < combine_spls_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (LASSO)")+
  ylab("Selection Proportion (sPLS)")+
  ggtitle("NO2")+
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_spls_sp.pdf",sep =""), width = 5.5, height = 5.5)
p1
dev.off()



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

threshold<-max(clus_NO2_op_sp,bio_NO2_op_sp,combine_NO2_op_sp)


ggplot(data=selection,aes(x=reorder(encoding,sp),
                          y=sp,fill=type)) +
  geom_bar(stat="identity", position="dodge", alpha=0.6, size=0.15, width = 0.7) +
  theme_bw()+
  #ggtitle(paste("Selection proportions"))+
  ylab("Selection proportion")+
  xlab("")+
  # facet_grid(str_wrap(group, width = 10)~., scales = "free", space = "free",  drop = T)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=13),
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
  geom_hline(yintercept = c(clus_NO2_op_sp,bio_NO2_op_sp,combine_NO2_op_sp), linetype="dashed", color = c("skyblue","green","red")) +
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp_NO2.pdf")


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

selection <- filter(selection, sp>0.5)

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
  geom_hline(yintercept = c(clus_PM10_op_sp,bio_PM10_op_sp,combine_PM10_op_sp), linetype="dashed", color = c("skyblue","green","red")) +
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

selection <- filter(selection, sp>0.5)

#optimal selection proportion for each

clus_PM2.5_op_sp<-Argmax(out_clus_PM2.5)[2]
bio_PM2.5_op_sp<-Argmax(out_bio_PM2.5)[2]
combine_PM2.5_op_sp<-Argmax(out_combine__PM2.5)[2]

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
  geom_hline(yintercept = c(clus_PM2.5_op_sp,bio_PM2.5_op_sp,combine_PM2.5_op_sp), linetype="dashed", color = c("skyblue","green","red")) +
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp_PM2.5.pdf")


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

selection <- filter(selection, sp>0.5)

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

