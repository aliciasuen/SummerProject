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

spls = T

if(spls){
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_biomarkers.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj2/report/"
  path_to_multi_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj1/Air_clus_bio/lasso/pfer/"
  path_to_spls_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj2/Air_clus_bio/sPLS_BiSelection/pfer/"
   ## sel prop ##
  bio_NO2_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio_NO2.rds",sep=""))
  clus_NO2_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus_NO2.rds",sep=""))
  combine_NO2_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_NO2.rds",sep=""))
  ## out ## 
  out_bio_NO2 = readRDS(paste(path_to_multi_Results_report, "out_bio_NO2.rds",sep=""))
  out_clus_NO2 = readRDS(paste(path_to_multi_Results_report, "out_metab_clus_NO2.rds",sep=""))
  out_combine_NO2 = readRDS(paste(path_to_multi_Results_report, "out_combine_NO2.rds",sep=""))
  ## betas ## 
  bio_NO2_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio_NO2.rds",sep=""))
  clus_NO2_betas  = readRDS(paste(path_to_multi_Results_report, "betas_stability_clus_NO2.rds",sep=""))
  combine_NO2_betas  = readRDS(paste(path_to_multi_Results_report, "betas_combine_NO2.rds",sep=""))
  
  ## sel prop XY ##
  bio_sp_X = readRDS(paste(path_to_spls_Results_report, "sparsityXY/selpropX_spls_bio.rds",sep=""))
  clus_sp_X = readRDS(paste(path_to_spls_Results_report, "sparsityXY/selpropX_spls_metab_clus_base.rds",sep=""))
  combine_sp_X = readRDS(paste(path_to_spls_Results_report, "sparsityXY/selpropX_spls_combine.rds",sep=""))
  bio_sp_Y = readRDS(paste(path_to_spls_Results_report, "sparsityXY/selpropY_spls_bio.rds",sep=""))
  clus_sp_Y = readRDS(paste(path_to_spls_Results_report, "sparsityXY/selpropY_spls_metab_clus_base.rds",sep=""))
  combine_sp_Y = readRDS(paste(path_to_spls_Results_report, "sparsityXY/selpropY_spls_combine.rds",sep=""))
  ## out XY ## 
  bio_out_XY = readRDS(paste(path_to_spls_Results_report, "sparsityXY/out_spls_bio.rds",sep=""))
  clus_out_XY = readRDS(paste(path_to_spls_Results_report, "sparsityXY/out_spls_metab_clus_base.rds",sep=""))
  combine_out_XY = readRDS(paste(path_to_spls_Results_report, "sparsityXY/out_spls_combine.rds",sep=""))
  
}


# making the legend for combined ------------------------------------------
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


# ------------lasso NO2 v.s.sPLS multi NO2-------------------------------------------------------------
combine_NO2_sp = as.data.frame(combine_NO2_sp)
colnames(combine_NO2_sp)[1] = "sp"
combine_NO2_sp$encoding<-rownames(combine_NO2_sp)
combine_NO2_sp$type<-rep("LASSO",times=nrow(combine_NO2_sp))

combine_sp_X = as.data.frame(t(combine_sp_X))
colnames(combine_sp_X)[1] = "sp"
combine_sp_X$encoding<-rownames(combine_sp_X)
combine_sp_X$type<-rep("sPLS",times=nrow(combine_sp_X))

selection = cbind(combine_NO2_sp,combine_sp_X)
colnames(selection)[1] = "sp_lasso"
colnames(selection)[4] = "sp_spls"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,50))

combine_NO2_op_sp<-Argmax(out_combine_NO2)[2]
combine_spls_op_sp<-combine_out_XY$summary$pix[1]

selection = selection %>% mutate(selection$newcolour,ifelse(sp_lasso<combine_NO2_op_sp & sp_spls<combine_spls_op_sp, "grey",mycolour_point))
colnames(selection)[6] = "newcolour"


p1=ggplot(selection,
          aes(sp_lasso, sp_spls, label=ifelse((sp_lasso < combine_NO2_op_sp & sp_spls < combine_spls_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "navy") +
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (LASSO-NO2)")+
  ylab("Selection Proportion (sPLS - Multivariate)")+
  ggtitle("")+
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_spls_sp.pdf",sep =""), width = 5, height = 5)
p1
dev.off()
