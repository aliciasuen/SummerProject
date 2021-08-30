### Summmer Project -- Objective 1 : Multivariate - LASSO stability selection on metabolites and air pollutants
### 16th June - Alicia 


# Load packages
library(tidyverse)
library(colorspace)
library(plotrix)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

## -----------------------------------------------------------------------------------------------------------------------
group1_med = F
group1_cen = F
group2_med = F
group2_cen = F
met_no_bio = F
ncomp2 = T
met_base = F
met_first= F

## -----------------------------------------------------------------------------------------------------------------------
if(group1_med){
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/cluster_manual/group1_med/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/cluster_manual/group1_med/"
  ## sel prop X ##
  bio_sp = readRDS(paste(path_to_multi_results, "sparsityX/selprop_spls_bio.rds",sep=""))
  clus_sp = readRDS(paste(path_to_multi_results, "sparsityX/selprop_spls_metab_clus_base.rds",sep=""))
  combine_sp = readRDS(paste(path_to_multi_results, "sparsityX/selprop_spls_combine.rds",sep=""))
  ## out X ## 
  bio_out = readRDS(paste(path_to_multi_results, "sparsityX/out_spls_bio.rds",sep=""))
  clus_out = readRDS(paste(path_to_multi_results, "sparsityX/out_spls_metab_clus_base.rds",sep=""))
  combine_out = readRDS(paste(path_to_multi_results, "sparsityX/out_spls_combine.rds",sep=""))
  # ## loadings ## 
  # bio_load = readRDS(paste(path_to_multi_results, "load_bio.rds",sep=""))
  # clus_load = readRDS(paste(path_to_multi_results, "load_metab_clus_base.rds",sep=""))
  # combine_load = readRDS(paste(path_to_multi_results, "load_combine.rds",sep=""))
  
  ## sel prop XY ##
  bio_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY/selpropX_spls_bio.rds",sep=""))
  clus_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY/selpropX_spls_metab_clus_base.rds",sep=""))
  combine_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY/selpropX_spls_combine.rds",sep=""))
  bio_sp_Y = readRDS(paste(path_to_multi_results, "sparsityXY/selpropY_spls_bio.rds",sep=""))
  clus_sp_Y = readRDS(paste(path_to_multi_results, "sparsityXY/selpropY_spls_metab_clus_base.rds",sep=""))
  combine_sp_Y = readRDS(paste(path_to_multi_results, "sparsityXY/selpropY_spls_combine.rds",sep=""))
  ## out XY ## 
  bio_out_XY = readRDS(paste(path_to_multi_results, "sparsityXY/out_spls_bio.rds",sep=""))
  clus_out_XY = readRDS(paste(path_to_multi_results, "sparsityXY/out_spls_metab_clus_base.rds",sep=""))
  combine_out_XY = readRDS(paste(path_to_multi_results, "sparsityXY/out_spls_combine.rds",sep=""))
  
}

if(group2_med){
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/cluster_manual/group2_med/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/cluster_manual/group2_med/"
  ## sel prop X ##
  bio_sp = readRDS(paste(path_to_multi_results, "sparsityX/selprop_spls_bio.rds",sep=""))
  clus_sp = readRDS(paste(path_to_multi_results, "sparsityX/selprop_spls_metab_clus_base.rds",sep=""))
  combine_sp = readRDS(paste(path_to_multi_results, "sparsityX/selprop_spls_combine.rds",sep=""))
  ## out X ## 
  bio_out = readRDS(paste(path_to_multi_results, "sparsityX/out_spls_bio.rds",sep=""))
  clus_out = readRDS(paste(path_to_multi_results, "sparsityX/out_spls_metab_clus_base.rds",sep=""))
  combine_out = readRDS(paste(path_to_multi_results, "sparsityX/out_spls_combine.rds",sep=""))
  # ## loadings ## 
  # bio_load = readRDS(paste(path_to_multi_results, "load_bio.rds",sep=""))
  # clus_load = readRDS(paste(path_to_multi_results, "load_metab_clus_base.rds",sep=""))
  # combine_load = readRDS(paste(path_to_multi_results, "load_combine.rds",sep=""))
  
  ## sel prop XY ##
  bio_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY/selpropX_spls_bio.rds",sep=""))
  clus_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY/selpropX_spls_metab_clus_base.rds",sep=""))
  combine_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY/selpropX_spls_combine.rds",sep=""))
  bio_sp_Y = readRDS(paste(path_to_multi_results, "sparsityXY/selpropY_spls_bio.rds",sep=""))
  clus_sp_Y = readRDS(paste(path_to_multi_results, "sparsityXY/selpropY_spls_metab_clus_base.rds",sep=""))
  combine_sp_Y = readRDS(paste(path_to_multi_results, "sparsityXY/selpropY_spls_combine.rds",sep=""))
  ## out XY ## 
  bio_out_XY = readRDS(paste(path_to_multi_results, "sparsityXY/out_spls_bio.rds",sep=""))
  clus_out_XY = readRDS(paste(path_to_multi_results, "sparsityXY/out_spls_metab_clus_base.rds",sep=""))
  combine_out_XY = readRDS(paste(path_to_multi_results, "sparsityXY/out_spls_combine.rds",sep=""))
  
}

if(met_no_bio){
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/Air_clus_bio/sPLS_BiSelection/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/Air_clus_bio/sPLS_BiSelection/"
  ## sel prop X ##
  bio_sp = readRDS(paste(path_to_multi_results, "sparsityX/selprop_spls_bio.rds",sep=""))
  clus_sp = readRDS(paste(path_to_multi_results, "sparsityX/selprop_spls_metab_clus_base.rds",sep=""))
  combine_sp = readRDS(paste(path_to_multi_results, "sparsityX/selprop_spls_combine.rds",sep=""))
  ## out X ## 
  bio_out = readRDS(paste(path_to_multi_results, "sparsityX/out_spls_bio.rds",sep=""))
  clus_out = readRDS(paste(path_to_multi_results, "sparsityX/out_spls_metab_clus_base.rds",sep=""))
  combine_out = readRDS(paste(path_to_multi_results, "sparsityX/out_spls_combine.rds",sep=""))
  # ## loadings ## 
  # bio_load = readRDS(paste(path_to_multi_results, "load_bio.rds",sep=""))
  # clus_load = readRDS(paste(path_to_multi_results, "load_metab_clus_base.rds",sep=""))
  # combine_load = readRDS(paste(path_to_multi_results, "load_combine.rds",sep=""))
  
  ## sel prop XY ##
  bio_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY/selpropX_spls_bio.rds",sep=""))
  clus_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY/selpropX_spls_metab_clus_base.rds",sep=""))
  combine_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY/selpropX_spls_combine.rds",sep=""))
  bio_sp_Y = readRDS(paste(path_to_multi_results, "sparsityXY/selpropY_spls_bio.rds",sep=""))
  clus_sp_Y = readRDS(paste(path_to_multi_results, "sparsityXY/selpropY_spls_metab_clus_base.rds",sep=""))
  combine_sp_Y = readRDS(paste(path_to_multi_results, "sparsityXY/selpropY_spls_combine.rds",sep=""))
  ## out XY ## 
  bio_out_XY = readRDS(paste(path_to_multi_results, "sparsityXY/out_spls_bio.rds",sep=""))
  clus_out_XY = readRDS(paste(path_to_multi_results, "sparsityXY/out_spls_metab_clus_base.rds",sep=""))
  combine_out_XY = readRDS(paste(path_to_multi_results, "sparsityXY/out_spls_combine.rds",sep=""))
  
}

if(met_base){
  path_to_uni_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Univariate_obj1/Air_met_base/"
  metab = readRDS(paste(path, "Results/denoised_metab.rds", sep=""))
  metab = metab[order(as.numeric(row.names(metab))), ] 
  metab_clus = readRDS(paste(path, "Results/denoised_metab_clus.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  betas = readRDS(paste(path_to_uni_results, "betas_uni.rds", sep =""))
  pval = readRDS(paste(path_to_uni_results, "pvalues_uni.rds",sep ="")) 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/Air_met_base/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/Air_met_base/"
  ## sel prop ##
  metab_sp = readRDS(paste(path_to_multi_results, "selprop_spls_metab_base.rds",sep=""))
  clus_sp = readRDS(paste(path_to_multi_results, "selprop_spls_metab_clus_base.rds",sep=""))
  ## out ## 
  metab_out = readRDS(paste(path_to_multi_results, "out_spls_metab_base.rds",sep=""))
  clus_out = readRDS(paste(path_to_multi_results, "out_spls_metab_clus_base.rds",sep=""))
  ## betas ## 
  metab_load = readRDS(paste(path_to_multi_results, "load_metab_base.rds",sep=""))
  clus_load = readRDS(paste(path_to_multi_results, "load_metab_clus_base.rds",sep=""))
}

if(ncomp2){
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/Air_clus_bio/sPLS_BiSelection/pfer/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/Air_clus_bio/sPLS_BiSelection/pfer/"
  ## sel prop X ##
  bio_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/selpropX_1_spls_bio.rds",sep=""))
  clus_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/selpropX_1_spls_metab_clus_base.rds",sep=""))
  combine_sp_X = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/selpropX_1_spls_combine.rds",sep=""))
  ## out X ## 
  bio_out = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/out_spls_bio.rds",sep=""))
  clus_out = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/out_spls_metab_clus_base.rds",sep=""))
  combine_out = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/out_spls_combine.rds",sep=""))
  # ## loadings ## 
  # bio_load = readRDS(paste(path_to_multi_results, "load_bio.rds",sep=""))
  # clus_load = readRDS(paste(path_to_multi_results, "load_metab_clus_base.rds",sep=""))
  # combine_load = readRDS(paste(path_to_multi_results, "load_combine.rds",sep=""))
  
  ## sel prop XY ##
  bio_sp_Y1 = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/selpropY_1_spls_bio.rds",sep=""))
  clus_sp_Y1 = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/selpropY_1_spls_metab_clus_base.rds",sep=""))
  combine_sp_Y1 = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/selpropY_1_spls_combine.rds",sep=""))
  bio_sp_Y2 = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/selpropY_2_spls_bio.rds",sep=""))
  clus_sp_Y2 = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/selpropY_2_spls_metab_clus_base.rds",sep=""))
  combine_sp_Y2 = readRDS(paste(path_to_multi_results, "sparsityXY_ncomp3/selpropY_2_spls_combine.rds",sep=""))
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

# colnames(metab) = make.names(colnames(metab))
# colnames(metab_clus) = make.names(colnames(metab_clus))

# -----------------------Sel prop plots sparsity on XY--------------------------------------------------
## Visualisation of selection proportions by stability selection models 

## Biomarkers alone on X  ##
bio_sp_X=as.data.frame(bio_sp_X) %>% sort(bio_sp_X, decreasing=TRUE)
colname <- as.character(colnames(bio_sp_X))
bio_sp_X = as.numeric(bio_sp_X)
names(bio_sp_X) <- colname

pdf(paste(path_to_figure, "sPLS_spX_bio.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(bio_sp_X, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(bio_sp_X>=bio_out_XY$summary$pix, yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of biomarkers alone with multivariate response of air pollutants exposure",
     cex.main = 0.6)
abline(h=bio_out_XY$summary$pix, lty=2, col="darkred")
for (i in 1:length(bio_sp_X)){
  axis(side=1, at=i, labels =names(bio_sp_X)[i], las=2,cex.axis = 0.5,
       col=ifelse(bio_sp_X[i]>=bio_out_XY$summary$pix, yes="red", no="grey"),
       col.axis=ifelse(bio_sp_X[i]>=bio_out_XY$summary$pix, yes="red", no="grey"))
}
dev.off()

## Biomarkers alone on Y  ##
bio_sp_Y=as.data.frame(bio_sp_Y) %>% sort(bio_sp_Y, decreasing=TRUE)
colname <- as.character(colnames(bio_sp_Y))
bio_sp_Y = as.numeric(bio_sp_Y)
names(bio_sp_Y) <- colname

pdf(paste(path_to_figure, "sPLS_spY_bio.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(bio_sp_Y, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(bio_sp_Y>=bio_out_XY$summary$piy, yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of multivariate response of air pollutants exposure",
     cex.main = 0.6)
abline(h=bio_out_XY$summary$piy, lty=2, col="darkred")
for (i in 1:length(bio_sp_Y)){
  axis(side=1, at=i, labels =names(bio_sp_Y)[i], las=2,cex.axis = 0.5,
       col=ifelse(bio_sp_Y[i]>=bio_out_XY$summary$piy, yes="red", no="grey"),
       col.axis=ifelse(bio_sp_Y[i]>=bio_out_XY$summary$piy, yes="red", no="grey"))
}
dev.off()

## Clustered metabolites alone on X ##
clus_sp_X=as.data.frame(clus_sp_X) %>% sort(clus_sp_X, decreasing=TRUE)
colname <- as.character(colnames(clus_sp_X))
clus_sp_X = as.numeric(clus_sp_X)
names(clus_sp_X) <- colname

pdf(paste(path_to_figure, "sPLS_spX_clus.pdf", sep =""))
par(mar=c(15,5,1,1))
plot(clus_sp_X, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(clus_sp_X>=clus_out_XY$summary$pix, yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of clusted metabolites alone with multivariate response of air pollutants exposure",
     cex.main = 0.5)
abline(h=clus_out_XY$summary$pix, lty=2, col="darkred")
for (i in 1:length(clus_sp_X)){
  axis(side=1, at=i, labels =names(clus_sp_X)[i], las=2,cex.axis = 0.4,
       col=ifelse(clus_sp_X[i]>=clus_out_XY$summary$pix, yes="red", no="grey"),
       col.axis=ifelse(clus_sp_X[i]>=clus_out_XY$summary$pix, yes="red", no="grey"))
}
dev.off()
## Clustered metabolites alone on Y ##
clus_sp_Y=as.data.frame(clus_sp_Y) %>% sort(clus_sp_Y, decreasing=TRUE)
colname <- as.character(colnames(clus_sp_Y))
clus_sp_Y = as.numeric(clus_sp_Y)
names(clus_sp_Y) <- colname

pdf(paste(path_to_figure, "sPLS_spY_clus.pdf", sep =""))
par(mar=c(15,5,1,1))
plot(clus_sp_Y, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(clus_sp_Y>=clus_out_XY$summary$piy, yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of clusted metabolites alone with multivariate response of air pollutants exposure",
     cex.main = 0.5)
abline(h=clus_out_XY$summary$piy, lty=2, col="darkred")
for (i in 1:length(clus_sp_Y)){
  axis(side=1, at=i, labels =names(clus_sp_Y)[i], las=2,cex.axis = 0.4,
       col=ifelse(clus_sp_Y[i]>=clus_out_XY$summary$piy, yes="red", no="grey"),
       col.axis=ifelse(clus_sp_Y[i]>=clus_out_XY$summary$piy, yes="red", no="grey"))
}
dev.off()

## Combine ##
combine_sp_X=as.data.frame(combine_sp_X) %>% sort(combine_sp_X, decreasing=TRUE)
colname <- as.character(colnames(combine_sp_X))
combine_sp_X = as.numeric(combine_sp_X)
names(combine_sp_X) <- colname

pdf(paste(path_to_figure, "sPLS_spX_combine.pdf", sep =""))
par(mar=c(15,5,1,1))
plot(combine_sp_X, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(combine_sp_X>=combine_out_XY$summary$pix, yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of clusted metabolites and biomarkers with multivariate response",
     cex.main = 0.6)
abline(h=combine_out_XY$summary$pix, lty=2, col="darkred")
for (i in 1:length(combine_sp_X)){
  axis(side=1, at=i, labels =names(combine_sp_X)[i], las=2,cex.axis = 0.4,
       col=ifelse(combine_sp_X[i]>=combine_out_XY$summary$pix, yes="red", no="grey"),
       col.axis=ifelse(combine_sp_X[i]>=combine_out_XY$summary$pix, yes="red", no="grey"))
}
dev.off()


## Combine ##
combine_sp_Y=as.data.frame(combine_sp_Y) %>% sort(combine_sp_Y, decreasing=TRUE)
colname <- as.character(colnames(combine_sp_Y))
combine_sp_Y = as.numeric(combine_sp_Y)
names(combine_sp_Y) <- colname

pdf(paste(path_to_figure, "sPLS_spY_combine.pdf", sep =""))
par(mar=c(15,5,1,1))
plot(combine_sp_Y, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(combine_sp_Y>=combine_out_XY$summary$piy, yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of clusted metabolites and biomarkers with multivariate response",
     cex.main = 0.6)
abline(h=combine_out_XY$summary$piy, lty=2, col="darkred")
for (i in 1:length(combine_sp_Y)){
  axis(side=1, at=i, labels =names(combine_sp_Y)[i], las=2,cex.axis = 0.4,
       col=ifelse(combine_sp_Y[i]>=combine_out_XY$summary$piy, yes="red", no="grey"),
       col.axis=ifelse(combine_sp_Y[i]>=combine_out_XY$summary$piy, yes="red", no="grey"))
}
dev.off()


# ----------ggplots on all sparsity on XY ---------------------------------------------------------------
# clus_sp_X = t(clus_sp_X)
clus_sp_X = as.data.frame(clus_sp_X)
# bio_sp_X = t(bio_sp_X)
bio_sp_X = as.data.frame(bio_sp_X)
# combine_sp_X = t(combine_sp_X)
combine_sp_X = as.data.frame(combine_sp_X)

colnames(clus_sp_X)[1] = "sp"
colnames(bio_sp_X)[1] = "sp"
colnames(combine_sp_X)[1] = "sp"

clus_sp_X$type<-rep("Metabolites",times=nrow(clus_sp_X))
bio_sp_X$type<-rep("Biomarkers",times=nrow(bio_sp_X))
combine_sp_X$type<-rep("Combined",times=nrow(combine_sp_X))

clus_sp_X$encoding<-rownames(clus_sp_X)
bio_sp_X$encoding<-rownames(bio_sp_X)
combine_sp_X$encoding<-rownames(combine_sp_X)
# combine_sp_X = combine_sp_X[,c(1,4,5)]

#optimal selection proportion for each

clus_op_sp<-clus_out$summary$pix
bio_op_sp<-bio_out$summary$pix
combine_op_sp<-combine_out$summary$pix
threshold<-max(clus_op_sp,bio_op_sp,combine_op_sp)

selection<-rbind(clus_sp_X,bio_sp_X,combine_sp_X)
selection$encoding <- chartr(".", " ", selection$encoding)
mygrep = selection %>% group_by(encoding) %>% filter(sp>threshold) %>% .$encoding
selection = selection[which(selection$encoding %in% mygrep),]


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
    legend.text = element_text(size =10)
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
  geom_hline(yintercept = c(clus_op_sp[1],bio_op_sp[1],combine_op_sp[1]), linetype="dashed", color = c("skyblue","green","red")) +
  # geom_hline(yintercept = clus_NO2_op_sp, ) + geom_hline(yintercept =bio_NO2_op_sp )+ geom_hline(yintercept =combine_NO2_op_sp)+
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp_X.pdf")

## sparsity on Y ##
# clus_sp_Y=t(clus_sp_Y)
clus_sp_Y = as.data.frame(clus_sp_Y1)
# bio_sp_Y=t(bio_sp_Y)
bio_sp_Y = as.data.frame(bio_sp_Y1)
# combine_sp_Y=t(combine_sp_Y)
combine_sp_Y = as.data.frame(combine_sp_Y1)

colnames(clus_sp_Y)[1] = "sp"
colnames(bio_sp_Y)[1] = "sp"
colnames(combine_sp_Y)[1] = "sp"

clus_sp_Y$type<-rep("Metabolites",times=nrow(clus_sp_Y))
bio_sp_Y$type<-rep("Biomarkers",times=nrow(bio_sp_Y))
combine_sp_Y$type<-rep("Combined",times=nrow(combine_sp_Y))

clus_sp_Y$encoding<-rownames(clus_sp_Y)
bio_sp_Y$encoding<-rownames(bio_sp_Y)
combine_sp_Y$encoding<-rownames(combine_sp_Y)
# combine_sp_Y = combine_sp_Y[,c(1,4,5)]

#optimal selection proportion for each

clus_op_sp<-clus_out$summary$piy
bio_op_sp<-bio_out$summary$piy
combine_op_sp<-combine_out$summary$piy

threshold<-max(clus_op_sp,bio_op_sp,combine_op_sp)

selection<-rbind(clus_sp_Y,bio_sp_Y,combine_sp_Y)
selection$encoding <- chartr(".", " ", selection$encoding)
mygrep = selection %>% group_by(encoding) %>% filter(sp>threshold) %>% .$encoding
selection = selection[which(selection$encoding %in% mygrep),]


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
    legend.text = element_text(size =10)
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
  geom_hline(yintercept = c(clus_op_sp[1],bio_op_sp[1],combine_op_sp[1]), linetype="dashed", color = c("skyblue","green","red")) +
  # geom_hline(yintercept = clus_NO2_op_sp, ) + geom_hline(yintercept =bio_NO2_op_sp )+ geom_hline(yintercept =combine_NO2_op_sp)+
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp_Y.pdf")

## sparsity on Y ##

clus_sp_Y1 = as.data.frame(clus_sp_Y1)
bio_sp_Y1 = as.data.frame(bio_sp_Y1)
combine_sp_Y1 = as.data.frame(combine_sp_Y1)

colnames(clus_sp_Y1)[1] = "sp"
colnames(bio_sp_Y1)[1] = "sp"
colnames(combine_sp_Y1)[1] = "sp"

clus_sp_Y1$type<-rep("Metabolites",times=nrow(clus_sp_Y1))
bio_sp_Y1$type<-rep("Biomarkers",times=nrow(bio_sp_Y1))
combine_sp_Y1$type<-rep("Combined",times=nrow(combine_sp_Y1))

clus_sp_Y1$encoding<-rownames(clus_sp_Y1)
bio_sp_Y1$encoding<-rownames(bio_sp_Y1)
combine_sp_Y1$encoding<-rownames(combine_sp_Y1)
combine_sp_X = combine_sp_X[,c(1,4,5)]

selection<-rbind(clus_sp_Y1,bio_sp_Y1,combine_sp_Y1)

selection <- filter(selection, sp>0.25)

#optimal selection proportion for each

clus_op_sp<-clus_out$summary$piy
bio_op_sp<-bio_out$summary$piy
combine_op_sp<-combine_out$summary$piy

threshold<-max(clus_op_sp,bio_op_sp,combine_op_sp)


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
    legend.text = element_text(size =10)
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
  geom_hline(yintercept = c(clus_op_sp[1],bio_op_sp[1],combine_op_sp[1]), linetype="dashed", color = c("skyblue","green","red")) +
  # geom_hline(yintercept = clus_NO2_op_sp, ) + geom_hline(yintercept =bio_NO2_op_sp )+ geom_hline(yintercept =combine_NO2_op_sp)+
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp_Y1.pdf")

clus_sp_Y2= as.data.frame(clus_sp_Y2)
bio_sp_Y2 = as.data.frame(bio_sp_Y2)
combine_sp_Y2 = as.data.frame(combine_sp_Y2)

colnames(clus_sp_Y2)[1] = "sp"
colnames(bio_sp_Y2)[1] = "sp"
colnames(combine_sp_Y2)[1] = "sp"

clus_sp_Y2$type<-rep("Metabolites",times=nrow(clus_sp_Y2))
bio_sp_Y2$type<-rep("Biomarkers",times=nrow(bio_sp_Y2))
combine_sp_Y2$type<-rep("Combined",times=nrow(combine_sp_Y2))

clus_sp_Y2$encoding<-rownames(clus_sp_Y2)
bio_sp_Y2$encoding<-rownames(bio_sp_Y2)
combine_sp_Y2$encoding<-rownames(combine_sp_Y2)
combine_sp_X = combine_sp_X[,c(1,4,5)]

selection<-rbind(clus_sp_Y2,bio_sp_Y2,combine_sp_Y2)

selection <- filter(selection, sp>0.25)

# ---------------------Correlation pattern----------------------------------------------------
sp = clus_sp_X %>% as.data.frame() %>% filter (. >= clus_out$summary$pix) 
sp_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_clus.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()

sum(clus_sp_X>=clus_out$summary$pix) # 23
sum(bio_sp_X>=bio_out$summary$pix)  # 10
sum(combine_sp_X>=combine_out$summary$pix)  # 27

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

# ------------------Clustered Metabolites sPLS selection prop v.s. loadings  -------------------------------------------------------

## clustered metabolites ##
tmp = rep(c(0.005,-0.005),length.out = length(clus_load))
names(tmp)=names(sort(clus_load))
x_nudge = tmp[names(clus_load)]

tmp = rep(c(0.02,-0.02),length.out = length(clus_sp))
names(tmp)=names(sort(clus_sp))
y_nudge = tmp[names(clus_sp)]

pdf(paste(path_to_figure, "spls_volcano_clus.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(clus_load, clus_sp, las=1, pch=19,
     col=ifelse(abs(clus_sp) < 0.78,"grey","skyblue"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "skyblue",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(clus_load+x_nudge, clus_sp+y_nudge,
     labels = ifelse(abs(clus_sp) < 0.78,"",names(clus_sp)),
     cex = 0.4)
dev.off()
sum(clus_sp>=0.81)

# ---------------------Correlation pattern----------------------------------------------------
sp_metab = metab_sp %>% data.frame() %>% filter (. >= 0.81) 
sp_metab_df = metab %>% select(which(colnames(metab) %in% rownames(sp_metab)))
mycor = cor(sp_metab_df)
pdf(paste(path_to_figure, "corr_0.81_metab.pdf", sep =""))
pheatmap(mycor, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)
dev.off()

sp_clus = clus_sp %>% data.frame() %>% filter (. >= 0.78) 
sp_clus_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_clus)))
mycor = cor(sp_clus_df)
pdf(paste(path_to_figure, "corr_0.78_metab_clus.pdf", sep =""))
pheatmap(mycor, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)
dev.off()


# -----------------------Sel prop plots sparsity only on X --------------------------------------------------
## Visualisation of selection proportions by stability selection models 

## Biomarkers alone ##
bio_sp=as.data.frame(bio_sp) %>% sort(bio_sp, decreasing=TRUE)
colname <- as.character(colnames(bio_sp))
bio_sp = as.numeric(bio_sp)
names(bio_sp) <- colname

pdf(paste(path_to_figure, "sPLS_sp_bio.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(bio_sp, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(bio_sp>=bio_out$summary$pix, yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of biomarkers alone with multivariate response of air pollutants exposure",
     cex.main = 0.6)
abline(h=bio_out$summary$pix, lty=2, col="darkred")
for (i in 1:length(bio_sp)){
  axis(side=1, at=i, labels =names(bio_sp)[i], las=2,cex.axis = 0.5,
       col=ifelse(bio_sp[i]>=bio_out$summary$pix, yes="red", no="grey"),
       col.axis=ifelse(bio_sp[i]>=bio_out$summary$pix, yes="red", no="grey"))
}
dev.off()

## Clustered metabolites alone ##
clus_sp=as.data.frame(clus_sp) %>% sort(clus_sp, decreasing=TRUE)
colname <- as.character(colnames(clus_sp))
clus_sp = as.numeric(clus_sp)
names(clus_sp) <- colname

pdf(paste(path_to_figure, "sPLS_sp_clus.pdf", sep =""))
par(mar=c(15,5,1,1))
plot(clus_sp, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(clus_sp>=clus_out$summary$pix, yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of clusted metabolites alone with multivariate response of air pollutants exposure",
     cex.main = 0.5)
abline(h=clus_out$summary$pix, lty=2, col="darkred")
for (i in 1:length(clus_sp)){
  axis(side=1, at=i, labels =names(clus_sp)[i], las=2,cex.axis = 0.4,
       col=ifelse(clus_sp[i]>=clus_out$summary$pix, yes="red", no="grey"),
       col.axis=ifelse(clus_sp[i]>=clus_out$summary$pix, yes="red", no="grey"))
}
dev.off()

## Combine ##
combine_sp=as.data.frame(combine_sp) %>% sort(combine_sp, decreasing=TRUE)
colname <- as.character(colnames(combine_sp))
combine_sp = as.numeric(combine_sp)
names(combine_sp) <- colname

pdf(paste(path_to_figure, "sPLS_sp_combine.pdf", sep =""))
par(mar=c(15,5,1,1))
plot(combine_sp, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(combine_sp>=combine_out$summary$pix, yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of clusted metabolites and biomarkers with multivariate response",
     cex.main = 0.6)
abline(h=combine_out$summary$pix, lty=2, col="darkred")
for (i in 1:length(combine_sp)){
  axis(side=1, at=i, labels =names(combine_sp)[i], las=2,cex.axis = 0.4,
       col=ifelse(combine_sp[i]>=combine_out$summary$pix, yes="red", no="grey"),
       col.axis=ifelse(combine_sp[i]>=combine_out$summary$pix, yes="red", no="grey"))
}
dev.off()

# ---------------ggplot combining all ----------------------------------------------------------
clus_sp = t(clus_sp)
clus_sp = as.data.frame(clus_sp)
bio_sp = t(bio_sp)
bio_sp = as.data.frame(bio_sp)
combine_sp = t(combine_sp)
combine_sp = as.data.frame(combine_sp)

colnames(clus_sp)[1] = "sp"
colnames(bio_sp)[1] = "sp"
colnames(combine_sp)[1] = "sp"

clus_sp$type<-rep("Metabolites",times=nrow(clus_sp))
bio_sp$type<-rep("Biomarkers",times=nrow(bio_sp))
combine_sp$type<-rep("Combined",times=nrow(combine_sp))

clus_sp$encoding<-rownames(clus_sp)
bio_sp$encoding<-rownames(bio_sp)
combine_sp$encoding<-rownames(combine_sp)

selection<- rbind(clus_sp,bio_sp,combine_sp)
selection$encoding <- chartr(".", " ", selection$encoding)
mygrep = selection %>% group_by(encoding) %>% filter(sp>0.9) %>% .$encoding
selection = selection[which(selection$encoding %in% mygrep),]


#optimal selection proportion for each

clus_op_sp<-clus_out$summary$pix
bio_op_sp<-bio_out$summary$pix
combine_op_sp<-combine_out$summary$pix

threshold<-max(clus_op_sp,bio_op_sp,combine_op_sp)


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
    legend.text = element_text(size =10)
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
  geom_hline(yintercept = c(clus_op_sp,bio_op_sp,combine_op_sp), linetype="dashed", color = c("skyblue","green","red"))+ 
  # geom_hline(yintercept = clus_NO2_op_sp, ) + geom_hline(yintercept =bio_NO2_op_sp )+ geom_hline(yintercept =combine_NO2_op_sp)+
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp.pdf")


# # ------------------Individual Metabolites sPLS selection prop v.s. loading  -------------------------------------------------------
# 
# ## metab individual ##
# tmp = rep(c(0.005,-0.005),length.out = length(metab_load))
# names(tmp)=names(sort(metab_load))
# x_nudge = tmp[names(metab_load)]
# 
# tmp = rep(c(0.02,-0.02),length.out = length(metab_sp))
# names(tmp)=names(sort(metab_sp))
# y_nudge = tmp[names(metab_sp)]
# 
# pdf(paste(path_to_figure, "spls_volcano.pdf", sep =""), width = 5.5, height = 5.5)
# par(mar=c(5,5,1,1))
# plot(metab_load, metab_sp, las=1, pch=19,
#      col=ifelse(abs(metab_sp) < 0.81,"grey","lightgoldenrod"),
#      cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
#      xlab="Mean betas coefficients", 
#      ylab="Selection proportion")
# legend("bottomright",pch=19,
#        col = "lightgoldenrod",
#        legend = "Stably selected metabolites", cex = 0.7, bg = "white")
# text(metab_load+x_nudge, metab_sp+y_nudge,
#      labels = ifelse(abs(metab_sp) < 0.81,"",names(metab_sp)),
#      cex = 0.4)
# dev.off()
# sum(metab_sp>=0.81)
# 
