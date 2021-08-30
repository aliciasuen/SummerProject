### Summmer Project -- Objective 1 : Multivariate - LASSO stability selection on metabolites and air pollutants
### 6th June 2021 - Alicia 


# Load packages
library(tidyverse)
library(colorspace)
library(plotrix)
library(ggplot2)
library(ggrepel)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

## -----------------------------------------------------------------------------------------------------------------------

lasso = T
univariate = F

## -----------------------------------------------------------------------------------------------------------------------
if(lasso){
  biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/report/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_clus_bio/lasso/"
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

if(spls){
  biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/report/"
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


source(paste(path,"Functions/penalisation_functions.R", sep=""))


# ------------------LASSO selection prop-------------------------------------------------------

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
# text(metab_NO2_betas+x_nudge, metab_NO2_sp+y_nudge,
#      labels = ifelse(abs(metab_NO2_sp) < 1,"",names(metab_NO2_sp)),
#      cex = 0.4)
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
#text(metab_PM10_betas+x_nudge, metab_PM10_sp+y_nudge,
     #labels = ifelse(abs(metab_PM10_sp) < 0.9,"",names(metab_PM10_sp)),
     #cex = 0.7)
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
#text(metab_PM2.5_betas+x_nudge, metab_PM2.5_sp+y_nudge,
     #labels = ifelse(abs(metab_PM2.5_sp) < 0.9,"",names(metab_PM2.5_sp)),
     #cex = 0.7)
dev.off()



# -----------------------Sel prop plots--------------------------------------------------
## Visualisation of selection proportions by stability selection models 
pdf(paste(path_to_figure, "lasso_sp_NO2.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(metab_NO2_sp, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(metab_NO2_sp>=0.9, yes="red", no="grey"), cex.lab=1.5)
abline(h=0.9, lty=2, col="darkred")
for (i in 1:length(metab_NO2_sp)){
  axis(side=1, at=i, labels =names(metab_NO2_sp)[i], las=2,cex.axis = 0.3,
       col=ifelse(metab_NO2_sp[i]>=0.9, yes="red", no="grey"),
       col.axis=ifelse(metab_NO2_sp[i]>=0.9, yes="red", no="grey"))
}
dev.off()

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


# ---------------------Uni-multi-volcano----------------------------------------------------


hat_params=GetArgmax(out_NO2)
print(hat_params)

air = rownames(pval)
features =colnames(pval)
tmp = as.vector(pval)
names(tmp) = paste0(rep(air, length.out = length(air)*length(features)),"_",
                    rep(features,each = length(air)))
logpval = -log10(tmp)

tmp = as.vector(betas)
names(tmp) = paste0(rep(air,length.out = length(air)*length(features)),"_",
                    rep(features,each = length(air)))
betas_list = tmp

list = c(names(metab_NO2)[metab_NO2>0.9],
         names(metab_PM10)[metab_PM10>0.9],
         names(metab_PM2.5)[metab_PM2.5>0.9])

mynode_colours=c(rep("lightgoldenrod",length(air)*length(metab_NO2)),
                 rep("lightcoral",length(air)*length(metab_PM10)),
                 rep("lightskyblue",length(air)*length(metab_PM2.5)))

bonf = logpval[logpval>-log10(0.05/ncol(pval))]
betas_list = betas_list[logpval>-log10(0.05/ncol(pval))]
mynode_colours = mynode_colours[logpval>-log10(0.05/ncol(pval))]

tmp = rep(c(0.1,-0.1),length.out = length(mynode_colours))
names(tmp)=names(sort(bonf))
x_nudge = tmp[names(bonf)]

pdf(paste(path_to_figure,"uni_lasso_volcano.pdf", sep=""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(betas_list, bonf, las=1, pch=19,
     col=ifelse(names(bonf)%in%list,mynode_colours,alpha("grey",0.1)),
     cex.lab=1, cex = 0.5,
     xlab=expression(beta), 
     ylab=expression(-log[10](p-value)))
text(betas_list+x_nudge, bonf,
     labels = ifelse(names(bonf)%in%list,names(bonf),""),
     cex = 0.5)
legend("top",pch=19,
       col = c("lightgoldenrod","lightcoral","lightskyblue","grey"),
       legend = c("NO2","PM10",
                  "PM2.5"), cex = 0.7)
dev.off()



# making the legend for combined ------------------------------------------
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

pdf(paste(path,"Figures/legend_bio.pdf", sep=""),width=2, height=4)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c("Biomarkers",paste0(bio_col$number,"-",bio_col$features)), text.col = "brown",
       bty='n', cex=0.6)
dev.off()

pdf(paste(path,"Figures/legend_metab_clus.pdf", sep=""),width=7, height=4)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c("Metabolites",paste0(metab_col$number,"-",metab_col$features)), text.col= "blue",
       bty='n', cex=0.6, ncol=2, text.width = 0.06)
dev.off()


# ------------lasso NO2 v.s.PM10-------------------------------------------------------------
combine_NO2_sp = as.data.frame(combine_NO2_sp)
colnames(combine_NO2_sp)[1] = "sp"
combine_NO2_sp$encoding<-rownames(combine_NO2_sp)
combine_NO2_sp$type<-rep("NO2",times=nrow(combine_NO2_sp))

combine_PM10_sp = as.data.frame(combine_PM10_sp)
colnames(combine_PM10_sp)[1] = "sp"
combine_PM10_sp$encoding<-rownames(combine_PM10_sp)
combine_PM10_sp$type<-rep("PM10",times=nrow(combine_PM10_sp))

selection = cbind(combine_NO2_sp,combine_PM10_sp)
colnames(selection)[1] = "sp_NO2"
colnames(selection)[4] = "sp_PM10"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)

# Reorder rows and transform to dataframe for ggplot

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,46))

# sel_lasso_1$var_cat=variable_cat
# sel_lasso_1$label=1:(nrow(sel_lasso_1))
# sel_lasso_1$mycolour_lab=darken(sel_lasso_1$mycolour_point, amount=0.5)

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_PM10_op_sp<-Argmax(out_combine_PM10)[2]

p1=ggplot(selection,
          aes(sp_NO2, sp_PM10, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM10 < combine_PM10_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_point(colour=selection$mycolour_point) +
  geom_label_repel(color=selection$mycolour_point,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (PM10)")+
  ggtitle("LASSO")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_no2pm10_sp.pdf",sep =""), width = 5.5, height = 5.5)
p1
dev.off()

# ------------lasso NO2 v.s. PM2.5-------------------------------------------------------------

combine_PM2.5_sp = as.data.frame(combine_PM2.5_sp)
colnames(combine_PM2.5_sp)[1] = "sp"
combine_PM2.5_sp$encoding<-rownames(combine_PM2.5_sp)
combine_PM2.5_sp$type<-rep("PM2.5",times=nrow(combine_PM2.5_sp))

selection = cbind(combine_NO2_sp,combine_PM2.5_sp)
colnames(selection)[1] = "sp_NO2"
colnames(selection)[4] = "sp_PM2.5"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)

# Reorder rows and transform to dataframe for ggplot

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,46))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_PM2.5_op_sp<-Argmax(out_combine_PM10)[2]


p2=ggplot(selection,
          aes(sp_NO2, sp_PM2.5, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM2.5 < combine_PM2.5_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_point(colour=selection$mycolour_point) +
  geom_label_repel(color=selection$mycolour_point,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (PM2.5)")+
  ggtitle("LASSO")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_no2pm2.5_sp.pdf",sep =""), width = 5.5, height = 5.5)
p2
dev.off()

# ------------lasso NO2 v.s. PM_abs-------------------------------------------------------------

combine_abs_sp = as.data.frame(combine_abs_sp)
colnames(combine_abs_sp)[1] = "sp"
combine_abs_sp$encoding<-rownames(combine_abs_sp)
combine_abs_sp$type<-rep("PM2.5",times=nrow(combine_abs_sp))

selection = cbind(combine_NO2_sp,combine_abs_sp)
colnames(selection)[1] = "sp_NO2"
colnames(selection)[4] = "sp_PM2.5_absorbance"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)

# Reorder rows and transform to dataframe for ggplot

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,46))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_abs_op_sp<-Argmax(out_combine_abs)[2]


p3=ggplot(selection,
          aes(sp_NO2, sp_PM2.5_absorbance, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM2.5_absorbance < combine_abs_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_point(colour=selection$mycolour_point) +
  geom_label_repel(color=selection$mycolour_point,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (PM2.5 absorbance)")+
  ggtitle("LASSO")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_no2abs_sp.pdf",sep =""), width = 5, height = 5)
p3
dev.off()

# ------------lasso NO2 v.s. majorroad -------------------------------------------------------------

combine_road_sp = as.data.frame(combine_road_sp)
colnames(combine_road_sp)[1] = "sp"
combine_road_sp$encoding<-rownames(combine_road_sp)
combine_road_sp$type<-rep("PM2.5",times=nrow(combine_road_sp))

selection = cbind(combine_NO2_sp,combine_road_sp)
colnames(selection)[1] = "sp_NO2"
colnames(selection)[4] = "sp_road"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)

# Reorder rows and transform to dataframe for ggplot

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,46))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_road_op_sp<-Argmax(out_combine_road)[2]


p4=ggplot(selection,
          aes(sp_NO2, sp_road, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_road < combine_road_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_point(colour=selection$mycolour_point) +
  geom_label_repel(color=selection$mycolour_point,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (Close to Major road)")+
  ggtitle("LASSO")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_no2road_sp.pdf",sep =""), width = 5, height = 5)
p4
dev.off()

# ------------spls NO2 v.s.PM10-------------------------------------------------------------
combine_NO2_sp = as.data.frame(combine_NO2_sp)
colnames(combine_NO2_sp)[1] = "sp"
combine_NO2_sp$encoding<-rownames(combine_NO2_sp)
combine_NO2_sp$type<-rep("NO2",times=nrow(combine_NO2_sp))

combine_PM10_sp = as.data.frame(combine_PM10_sp)
colnames(combine_PM10_sp)[1] = "sp"
combine_PM10_sp$encoding<-rownames(combine_PM10_sp)
combine_PM10_sp$type<-rep("PM10",times=nrow(combine_PM10_sp))

selection = cbind(combine_NO2_sp,combine_PM10_sp)
colnames(selection)[1] = "sp_NO2"
colnames(selection)[4] = "sp_PM10"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)

# Reorder rows and transform to dataframe for ggplot

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,47))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_PM10_op_sp<-Argmax(out_combine_PM10)[2]


p1=ggplot(selection,
          aes(sp_NO2, sp_PM10, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM10 < combine_PM10_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_point(colour=selection$mycolour_point) +
  geom_label_repel(color=selection$mycolour_point,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (PM10)")+
  ggtitle("sPLS")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_spls_no2pm10_sp.pdf",sep =""), width = 5, height = 5)
p1
dev.off()

# ------------spls NO2 v.s. PM2.5-------------------------------------------------------------

combine_PM2.5_sp = as.data.frame(combine_PM2.5_sp)
colnames(combine_PM2.5_sp)[1] = "sp"
combine_PM2.5_sp$encoding<-rownames(combine_PM2.5_sp)
combine_PM2.5_sp$type<-rep("PM2.5",times=nrow(combine_PM2.5_sp))

selection = cbind(combine_NO2_sp,combine_PM2.5_sp)
colnames(selection)[1] = "sp_NO2"
colnames(selection)[4] = "sp_PM2.5"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)

# Reorder rows and transform to dataframe for ggplot

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,47))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_PM2.5_op_sp<-Argmax(out_combine_PM10)[2]


p2=ggplot(selection,
          aes(sp_NO2, sp_PM2.5, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM2.5 < combine_PM2.5_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_point(colour=selection$mycolour_point) +
  geom_label_repel(color=selection$mycolour_point,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (PM2.5)")+
  ggtitle("sPLS")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_spls_no2pm2.5_sp.pdf",sep =""), width = 5, height = 5)
p2
dev.off()

# ------------spls NO2 v.s. PM_abs-------------------------------------------------------------

combine_abs_sp = as.data.frame(combine_abs_sp)
colnames(combine_abs_sp)[1] = "sp"
combine_abs_sp$encoding<-rownames(combine_abs_sp)
combine_abs_sp$type<-rep("PM2.5",times=nrow(combine_abs_sp))

selection = cbind(combine_NO2_sp,combine_abs_sp)
colnames(selection)[1] = "sp_NO2"
colnames(selection)[4] = "sp_PM2.5_absorbance"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)

# Reorder rows and transform to dataframe for ggplot

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,47))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_abs_op_sp<-Argmax(out_combine_abs)[2]


p3=ggplot(selection,
          aes(sp_NO2, sp_PM2.5_absorbance, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM2.5_absorbance < combine_abs_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_point(colour=selection$mycolour_point) +
  geom_label_repel(color=selection$mycolour_point,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (PM2.5 absorbance)")+
  ggtitle("sPLS")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_spls_no2abs_sp.pdf",sep =""), width = 5, height = 5)
p3
dev.off()

# ------------spls NO2 v.s. majorroad -------------------------------------------------------------

combine_road_sp = as.data.frame(combine_road_sp)
colnames(combine_road_sp)[1] = "sp"
combine_road_sp$encoding<-rownames(combine_road_sp)
combine_road_sp$type<-rep("PM2.5",times=nrow(combine_road_sp))

selection = cbind(combine_NO2_sp,combine_road_sp)
colnames(selection)[1] = "sp_NO2"
colnames(selection)[4] = "sp_road"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)

# Reorder rows and transform to dataframe for ggplot

## LASSO - base
xlim=c(0,1.05)
ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,47))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_road_op_sp<-Argmax(out_combine_road)[2]


p4=ggplot(selection,
          aes(sp_NO2, sp_road, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_road < combine_road_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_point(colour=selection$mycolour_point) +
  geom_label_repel(color=selection$mycolour_point,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (Close to Major road)")+
  ggtitle("sPLS")+
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_spls_no2road_sp.pdf",sep =""), width = 5, height = 5)
p4
dev.off()

