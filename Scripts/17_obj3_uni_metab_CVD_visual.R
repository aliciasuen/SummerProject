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
cluster = F
bio = T
comb = T

## For univariate -----------------------------------------------------------------------------------------------------------------------
if(cluster){
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj3/"
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Univariate_obj3/cluster/"
  air_pval = readRDS(paste(path_to_Results_report, "pvalues_cluster_uni.rds", sep=""))
  air_beta = readRDS(paste(path_to_Results_report, "betas_cluster_uni.rds", sep =""))
}

if(bio){
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj3/"
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Univariate_obj3/biomarker/"
  air_pval = readRDS(paste(path_to_Results_report, "pvalues_bio_uni.rds", sep=""))
  air_beta = readRDS(paste(path_to_Results_report, "betas_bio_uni.rds", sep =""))
  }

if(comb){
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj3/"
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Univariate_obj3/combine/"
  air_pval = readRDS(paste(path_to_Results_report, "pvalues_combine_uni.rds", sep=""))
  air_beta = readRDS(paste(path_to_Results_report, "betas_combine_uni.rds", sep =""))
}


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






# Univariate plot -------------------------------------------------------------
## Univariate ## ## 7 air * 74 variables ## 
pval = air_pval %>% as.data.frame()
beta = air_beta %>% as.data.frame()
bonf_threshold = -log10(0.05/(nrow(pval)))

selection = cbind(pval, beta)
colnames(selection)[1] = "pval"
colnames(selection)[2] = "beta"
selection$encoding = rownames(selection)
selection$encoding <- chartr(".", " ", selection$encoding)
if(cluster){
  selection$mycolour_point=rep("blue",times=c(50))
  selection$features <- ifelse(selection$encoding == combine_col[29:78,]$features, combine_col[29:78,]$number, NA)
}
if(comb){
  selection$mycolour_point=rep(c("brown","blue"),times=c(28,50))
  selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)
}
if(bio){
  selection$mycolour_point=rep("brown",times=c(28))
  selection$features <- ifelse(selection$encoding == combine_col[1:28,]$features, combine_col[1:28,]$number, NA)
}
selection = selection %>% mutate(selection$newcolour,ifelse(-log10(pval) < bonf_threshold, "grey", mycolour_point))
colnames(selection)[6]= "newcolour"


p2=ggplot(selection,
          aes(beta, -log10(pval), label=ifelse(-log10(pval) < bonf_threshold,"", selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_hline(aes(yintercept = bonf_threshold),linetype = "dashed",colour = "violet") +
  geom_vline(aes(xintercept = 0), linetype = "dotted", colour = "grey")+
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=4, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab(expression(beta))+
  ylab("-log10(p-value)")+
  ggtitle("")+
  # xlim(xlim) +
  # ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_uni_bio.pdf",sep =""), width = 5.5, height = 5.5)
p2
dev.off()







# ---------------------Uni-multi-volcano----------------------------------------------------

## Univariate ## ## 7 air * 74 variables ## 
pval = t(air_pval) %>% as.data.frame()
pval = pval[7]
bonf_threshold = -log10(0.05/(7*78))

beta = t(air_beta) %>% as.data.frame()
beta = beta[7]

## LASSO - NO2 ##

combine_sp = as.data.frame(combine_sp)
colnames(combine_sp)[1] = "sp"
combine_sp$encoding<-rownames(combine_sp)
combine_op_sp<-Argmax(out_combine)[2]

## Combining ## 
selection = cbind(combine_sp,pval,beta)
colnames(selection)[3]= "pval"
colnames(selection)[4]= "beta"
selection$encoding <- chartr(".", " ", selection$encoding)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,50))
selection = selection %>% mutate(selection$newcolour,ifelse(-log10(pval)*sign(beta) < bonf_threshold & sp < combine_op_sp & -log10(pval)*sign(beta) > -bonf_threshold | sp <0.5, "grey", mycolour_point))
colnames(selection)[6]= "newcolour"
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)

## LASSO - base
# xlim=c(0,1.05)
# ylim=c(0,1.05)
p1=ggplot(selection,
          aes(-log10(pval)*sign(beta), sp, label=ifelse(-log10(pval)*sign(beta) < bonf_threshold & sp < combine_op_sp & -log10(pval)*sign(beta) > -bonf_threshold | sp <0.5, "", selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = bonf_threshold),linetype = "dashed",colour = "violet") +
  geom_vline(aes(xintercept = -bonf_threshold),linetype = "dashed",colour = "violet") +
  geom_hline(aes(yintercept = combine_op_sp),linetype = "dashed",colour = "coral") +
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=4, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("-log10(p-value)")+
  ylab("Selection Proportion of stability selection")+
  ggtitle("NO2")+
  # xlim(xlim) +
  # ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_uni_lasso.pdf",sep =""), width = 5.5, height = 5.5)
p1
dev.off()














# -------------------------------------------------------------------------
lasso = T
if(lasso){
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj1/report/pfer/"
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
## LASSO - base
# xlim=c(0,1.05)
# ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,50))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_PM10_op_sp<-Argmax(out_combine_PM10)[2]

selection = selection %>% mutate(selection$newcolour,ifelse(sp_NO2<combine_NO2_op_sp & sp_PM10<combine_PM10_op_sp, "grey",mycolour_point))
colnames(selection)[6] = "newcolour"
# sel_lasso_1$var_cat=variable_cat
# sel_lasso_1$label=1:(nrow(sel_lasso_1))
# sel_lasso_1$mycolour_lab=darken(sel_lasso_1$mycolour_point, amount=0.5)



p1=ggplot(selection,
          aes(sp_NO2, sp_PM10, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM10 < combine_PM10_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (PM10)")+
  ggtitle("NO2 v.s PM10")+
  # xlim(xlim) +
  # ylim(ylim) +
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
# xlim=c(0,1.05)
# ylim=c(0,1.05)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,50))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_PM2.5_op_sp<-Argmax(out_combine_PM10)[2]

selection = selection %>% mutate(selection$newcolour,ifelse(sp_NO2<combine_NO2_op_sp & sp_PM2.5<combine_PM2.5_op_sp, "grey",mycolour_point))
colnames(selection)[6] = "newcolour"




p2=ggplot(selection,
          aes(sp_NO2, sp_PM2.5, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM2.5 < combine_PM2.5_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (PM2.5)")+
  ggtitle("NO2 v.s. PM2.5")+
  # xlim(xlim) +
  # ylim(ylim) +
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

selection$mycolour_point=rep(c("brown","blue"),times=c(28,50))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_abs_op_sp<-Argmax(out_combine_abs)[2]

selection = selection %>% mutate(selection$newcolour,ifelse(sp_NO2<combine_NO2_op_sp & sp_PM2.5_absorbance<combine_abs_op_sp, "grey",mycolour_point))
colnames(selection)[6] = "newcolour"




p3=ggplot(selection,
          aes(sp_NO2, sp_PM2.5_absorbance, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM2.5_absorbance < combine_abs_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (PM2.5 absorbance)")+
  ggtitle("NO2 v.s PM2.5 absorbance ")+
  # xlim(xlim) +
  # ylim(ylim) +
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

selection$mycolour_point=rep(c("brown","blue"),times=c(28,50))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_road_op_sp<-Argmax(out_combine_road)[2]

selection = selection %>% mutate(selection$newcolour,ifelse(sp_NO2<combine_NO2_op_sp & sp_road<combine_road_op_sp, "grey",mycolour_point))
colnames(selection)[6] = "newcolour"


p4=ggplot(selection,
          aes(sp_NO2, sp_road, label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_road < combine_road_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (NO2)")+
  ylab("Selection Proportion (Close to Major road)")+
  ggtitle("NO2 v.s. Major road")+
  # xlim(xlim) +
  # ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_no2road_sp.pdf",sep =""), width = 5, height = 5)
p4
dev.off()

# ------------betas NO2 v.s.PM10-------------------------------------------------------------
combine_NO2_sp = as.data.frame(combine_NO2_sp)
colnames(combine_NO2_sp)[1] = "sp_NO2"

combine_PM10_sp = as.data.frame(combine_PM10_sp)
colnames(combine_PM10_sp)[1] = "sp_PM10"

combine_NO2_betas = as.data.frame(combine_NO2_betas)
colnames(combine_NO2_betas)[1] = "beta_NO2"

combine_PM10_betas = as.data.frame(combine_PM10_betas)
colnames(combine_PM10_betas)[1] = "beta_PM10"
combine_PM10_betas$encoding<-rownames(combine_PM10_betas)

selection = cbind(combine_NO2_betas,combine_PM10_betas, combine_NO2_sp,combine_PM10_sp)

selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)
## LASSO - base
selection$mycolour_point=rep(c("brown","blue"),times=c(28,50))
selection = selection[,-c(5,6,8,9)]

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_PM10_op_sp<-Argmax(out_combine_PM10)[2]

selection = selection %>% mutate(selection$newcolour,ifelse(sp_NO2<combine_NO2_op_sp & sp_PM10<combine_PM10_op_sp, "grey",mycolour_point))
colnames(selection)[8] = "newcolour"


p1=ggplot(selection,
          aes(beta_NO2, beta_PM10,label=ifelse((sp_NO2 < combine_NO2_op_sp & sp_PM10 < combine_PM10_op_sp),"",selection$features))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "coral") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "coral") +
  geom_point(aes(beta_NO2, beta_PM10)) +
  geom_label_repel(color=selection$newcolour,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Beta (NO2)")+
  ylab("Beta (PM10)")+
  ggtitle("NO2 v.s PM10")+
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_no2pm10_beta.pdf",sep =""), width = 5.5, height = 5.5)
p1
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

