### Summmer Project -- Sensitivity Analysis 
### 3rd Aug - Alicia  

## 1) Townsend Index low and high
## 2) PCA 8 components 
## 3) Cluster manually dataset 

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))

# devtools::install("../Functions/focus-master")

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


# Loading datasets --------------------------------------------------------

metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
combine = readRDS(paste(path, "Results/combined_biometab.rds", sep=""))
combine = combine[order(as.numeric(row.names(combine))),] 
biobank=readRDS(paste(path, "Results/biobank_final.rds", sep=""))
biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep="")) 

path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/sensitivity/"
path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/sensitivity/"


# ------1) Townsend Low and High -------------------------------------------------------------------

## Getting median of Townsend Index and Stratify data ##
town_median = as.vector(summary(biobank$`189-0.0`)[3])

town_low = biobank %>% filter(biobank$`189-0.0`<= town_median)
metab_low = metab_clus %>% filter(rownames(metab_clus) %in% town_low$eid) %>% as.matrix()
bio_low = biomarker %>% filter(rownames(biomarker) %in% town_low$eid) %>% as.matrix()
combine_low = combine %>% filter(rownames(combine) %in% town_low$eid)%>% as.matrix()
air_low = select(town_low, c("24003-0.0":"24008-0.0", "24014-0.0"))
colnames(air_low) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")


town_high = biobank %>% filter(biobank$`189-0.0`> town_median)
metab_high = metab_clus %>% filter(rownames(metab_clus) %in% town_high$eid)%>% as.matrix()
bio_high = biomarker %>% filter(rownames(biomarker) %in% town_high$eid)%>% as.matrix()
combine_high = combine %>% filter(rownames(combine) %in% town_high$eid)%>% as.matrix()
air_high = select(town_high, c("24003-0.0":"24008-0.0", "24014-0.0"))
colnames(air_high) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")

# ------------------Townsend Low-------------------------------------------------------
## Clusters ##
NO2 = as.matrix(air_low$NO2)

out = VariableSelection(xdata = metab_low, ydata = NO2,K=100, tau = 0.5, verbose=FALSE, PFER_method = "MB",PFER_thr = 20,
                        family = "gaussian")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_metab_clus_NO2_low.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_NO2_low.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/clus_NO2_low.pdf",sep =""))
CalibrationPlot(out)
dev.off()

NO2 <- readRDS(paste(path_to_results, "out_metab_clus_NO2_low.rds",sep = ""))
hat_lambda_id.1=ArgmaxId(NO2)[1]
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
saveRDS(average_load.1,paste(path_to_results,"betas_stability_clus_NO2_low.rds", sep =""))

## Biomarkers ##
NO2 = as.matrix(air_low$NO2)
out = VariableSelection(xdata = bio_low, ydata = NO2,K=100, tau = 0.5, verbose=FALSE,PFER_method = "MB",PFER_thr = 20,
                        family = "gaussian")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_bio_NO2_low.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_bio_NO2_low.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/bio_NO2_low.pdf",sep =""))
CalibrationPlot(out)
dev.off()

NO2 <- readRDS(paste(path_to_results, "out_bio_NO2_low.rds",sep = ""))
hat_lambda_id.1=ArgmaxId(NO2)[1]
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
saveRDS(average_load.1,paste(path_to_results,"betas_stability_bio_NO2_low.rds", sep =""))


## Combine ##
NO2 = as.matrix(air_low$NO2)

out = VariableSelection(xdata = combine_low, ydata = NO2,K=100, tau = 0.5, verbose=FALSE,PFER_method = "MB",PFER_thr = 20,
                        family = "gaussian")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_NO2_low.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_NO2_low.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/combine_NO2_low.pdf",sep =""))
CalibrationPlot(out)
dev.off()

NO2 <- readRDS(paste(path_to_results, "out_combine_NO2_low.rds",sep = ""))
hat_lambda_id.1=ArgmaxId(NO2)[1]
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
saveRDS(average_load.1,paste(path_to_results,"betas_stability_combine_NO2_low.rds", sep =""))

# ------------------Townsend High-------------------------------------------------------
## Clusters ##
NO2 = as.matrix(air_high$NO2)

out = VariableSelection(xdata = metab_high, ydata = NO2,K=100, tau = 0.5, verbose=FALSE,PFER_method = "MB",PFER_thr = 20,
                        family = "gaussian")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_metab_clus_NO2_high.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_metab_clus_NO2_high.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/clus_NO2_high.pdf",sep =""))
CalibrationPlot(out)
dev.off()

NO2 <- readRDS(paste(path_to_results, "out_metab_clus_NO2_high.rds",sep = ""))
hat_lambda_id.1=ArgmaxId(NO2)[1]
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
saveRDS(average_load.1,paste(path_to_results,"betas_stability_clus_NO2_high.rds", sep =""))

## Biomarkers ##
NO2 = as.matrix(air_high$NO2)
out = VariableSelection(xdata = bio_high, ydata = NO2,K=100, tau = 0.5, verbose=FALSE,PFER_method = "MB",PFER_thr = 20,
                        family = "gaussian")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_bio_NO2_high.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_bio_NO2_high.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/bio_NO2_high.pdf",sep =""))
CalibrationPlot(out)
dev.off()

NO2 <- readRDS(paste(path_to_results, "out_bio_NO2_high.rds",sep = ""))
hat_lambda_id.1=ArgmaxId(NO2)[1]
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
saveRDS(average_load.1,paste(path_to_results,"betas_stability_bio_NO2_high.rds", sep =""))

## Combine ##
NO2 = as.matrix(air_high$NO2)

out = VariableSelection(xdata = combine_high, ydata = NO2,K=100, tau = 0.5, verbose=FALSE,PFER_method = "MB",PFER_thr = 20,
                        family = "gaussian")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_NO2_high.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_NO2_high.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/combine_NO2_high.pdf",sep =""))
CalibrationPlot(out)
dev.off()

NO2 <- readRDS(paste(path_to_results, "out_combine_NO2_high.rds",sep = ""))
hat_lambda_id.1=ArgmaxId(NO2)[1]
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
saveRDS(average_load.1,paste(path_to_results,"betas_stability_combine_NO2_high.rds", sep =""))


# -------------------------------------------------------------------------
townsend = T
if(townsend){
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/sensitivity/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/sensitivity/"
  ## sel prop ##
  bio_low_sp = readRDS(paste(path_to_multi_results, "selprop_bio_NO2_low.rds",sep=""))
  bio_high_sp = readRDS(paste(path_to_multi_results, "selprop_bio_NO2_high.rds",sep=""))
  clus_low_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_NO2_low.rds",sep=""))
  clus_high_sp = readRDS(paste(path_to_multi_results, "selprop_metab_clus_NO2_high.rds",sep=""))
  combine_low_sp = readRDS(paste(path_to_multi_results, "selprop_combine_NO2_low.rds",sep=""))
  combine_high_sp = readRDS(paste(path_to_multi_results, "selprop_combine_NO2_high.rds",sep=""))
  ## out ## 
  out_bio_low = readRDS(paste(path_to_multi_results, "out_bio_NO2_low.rds",sep=""))
  out_bio_high = readRDS(paste(path_to_multi_results, "out_bio_NO2_high.rds",sep=""))
  out_clus_low = readRDS(paste(path_to_multi_results, "out_metab_clus_NO2_low.rds",sep=""))
  out_clus_high = readRDS(paste(path_to_multi_results, "out_metab_clus_NO2_high.rds",sep=""))
  out_combine_low = readRDS(paste(path_to_multi_results, "out_combine_NO2_low.rds",sep=""))
  out_combine_high = readRDS(paste(path_to_multi_results, "out_combine_NO2_high.rds",sep=""))
  ## betas ## 
  bio_betas_low  = readRDS(paste(path_to_multi_results, "betas_stability_bio_NO2_low.rds",sep=""))
  bio_betas_high  = readRDS(paste(path_to_multi_results, "betas_stability_bio_NO2_high.rds",sep=""))
  clus_betas_low  = readRDS(paste(path_to_multi_results, "betas_stability_clus_NO2_low.rds",sep=""))
  clus_betas_high = readRDS(paste(path_to_multi_results, "betas_stability_clus_NO2_high.rds",sep=""))
  combine_betas_low  = readRDS(paste(path_to_multi_results, "betas_stability_combine_NO2_low.rds",sep=""))
  combine_betas_high  = readRDS(paste(path_to_multi_results, "betas_stability_combine_NO2_high.rds",sep=""))
}

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





# -------------------------Plotting comparison between Townsend low and high-----------------------------------------------


combine_low_sp = as.data.frame(combine_low_sp)
colnames(combine_low_sp)[1] = "sp"
combine_low_sp$encoding<-rownames(combine_low_sp)
combine_low_sp$type<-rep("Low",times=nrow(combine_low_sp))

combine_high_sp = as.data.frame(combine_high_sp)
colnames(combine_high_sp)[1] = "sp"
combine_high_sp$encoding<-rownames(combine_high_sp)
combine_high_sp$type<-rep("High",times=nrow(combine_high_sp))

selection = cbind(combine_low_sp,combine_high_sp)
colnames(selection)[1] = "sp_low"
colnames(selection)[4] = "sp_high"
selection = selection[,-c(2,3,6)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,46))

combine_low_op_sp<-Argmax(out_combine_low)[2]
combine_high_op_sp<-Argmax(out_combine_high)[2]

selection = selection %>% mutate(selection$newcolour,ifelse(sp_low<combine_low_op_sp & sp_high<combine_high_op_sp, "grey",mycolour_point))
colnames(selection)[6] = "newcolour"
# sel_lasso_1$var_cat=variable_cat
# sel_lasso_1$label=1:(nrow(sel_lasso_1))
# sel_lasso_1$mycolour_lab=darken(sel_lasso_1$mycolour_point, amount=0.5)



p1=ggplot(selection,
          aes(sp_low, sp_high, label=ifelse((sp_low < combine_low_op_sp & sp_high < combine_high_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (High SES)")+
  ylab("Selection Proportion (Low SES)")+
  ggtitle("NO2")+
  # xlim(xlim) +
  # ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_SES_sp.pdf",sep =""), width = 5.5, height = 5.5)
p1
dev.off()




# -------------------------------------------------------------------------
manual = T

if(manual){
  biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/sensitivity/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/cluster_manual/group1_med/"
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_clus_bio/lasso/pfer/"
  ## sel prop ##
  combine_NO2_sp_man = readRDS(paste(path_to_multi_results, "selprop_combine_NO2.rds",sep=""))
  combine_NO2_sp = readRDS(paste(path_to_results, "selprop_combine_NO2.rds",sep=""))
  ## out ## 
  out_combine_NO2_man = readRDS(paste(path_to_multi_results, "out_combine_NO2.rds",sep=""))
  out_combine_NO2 = readRDS(paste(path_to_results, "out_combine_NO2.rds",sep=""))
  ## betas ## 
  combine_NO2_betas_man  = readRDS(paste(path_to_multi_results, "betas_combine_NO2.rds",sep=""))
  combine_NO2_betas  = readRDS(paste(path_to_results, "betas_combine_NO2.rds",sep=""))
}


# Plotting Clustering manual group1 v.s. HC grouped LASSO NO2 -------------

combine_NO2_sp = as.data.frame(combine_NO2_sp)
colnames(combine_NO2_sp)[1] = "sp"
combine_NO2_sp$encoding<-rownames(combine_NO2_sp)
combine_NO2_sp$type<-rep("HC",times=nrow(combine_NO2_sp))

combine_NO2_sp_man = as.data.frame(combine_NO2_sp_man)
colnames(combine_NO2_sp_man)[1] = "sp"
combine_NO2_sp_man$encoding<-rownames(combine_NO2_sp_man)
combine_NO2_sp_man$type<-rep("Manual",times=nrow(combine_NO2_sp_man))

selection = merge(combine_NO2_sp,combine_NO2_sp_man, by ="encoding", all.x = T)
colnames(selection)[2] = "sp_hc"
colnames(selection)[4] = "sp_man"
selection = selection[,-c(3,5)]
selection$encoding <- chartr(".", " ", selection$encoding)
selection = selection %>% mutate(selection$features, ifelse(selection$encoding == combine_col$features, combine_col$number,NA ))
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)
selection$mycolour_point=rep(c("brown","blue"),times=c(28,46))

combine_op_sp<-Argmax(out_combine_NO2)[2]
combine_man_op_sp<-Argmax(out_combine_NO2_man)[2]

selection = selection %>% mutate(selection$newcolour,ifelse(sp_hc<combine_op_sp & sp_man<combine_man_op_sp, "grey",mycolour_point))
colnames(selection)[6] = "newcolour"
# sel_lasso_1$var_cat=variable_cat
# sel_lasso_1$label=1:(nrow(sel_lasso_1))
# sel_lasso_1$mycolour_lab=darken(sel_lasso_1$mycolour_point, amount=0.5)



p1=ggplot(selection,
          aes(sp_hc, sp_man, label=ifelse((sp_hc < combine_op_sp & sp_man < combine_man_op_sp),"",selection$features))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = "coral") +
  geom_point(colour=selection$newcolour) +
  geom_label_repel(color=selection$newcolour,
                   size=2.5, segment.color="grey",
                   segment.size=0.5, max.overlaps = Inf, 
                   nudge_y = 0.001, nudge_x = 0.001, box.padding = 0.3,
                   label.size = NA, label.padding=.01, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab("Selection Proportion (Metabolites Grouped by HC)")+
  ylab("Selection Proportion (Metabolites Grouped by subclasses and lipoprotein)")+
  ggtitle("NO2")+
  # xlim(xlim) +
  # ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "report_lasso_manual_sp.pdf",sep =""), width = 5.5, height = 5.5)
p1
dev.off()











# -----------2) PCA 8 components--------------------------------------------------------------

metab_clus = readRDS(paste(path, "Results/pca/pca_PC8.rds", sep =""))
air= select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")

NO2 = as.matrix(air$NO2)

out = VariableSelection(xdata = metab_clus, ydata = NO2,K=100, tau = 0.5, verbose=FALSE,
                        family = "gaussian")
selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_pca_NO2.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_pca_NO2.rds", sep =""))

pdf(paste(path_to_save,"CalibrationPlot/pca_NO2.pdf",sep =""))
CalibrationPlot(out)
dev.off()

NO2 <- readRDS(paste(path_to_results, "out_pca_NO2.rds",sep = ""))
hat_lambda_id.1=ArgmaxId(NO2)[1]
average_load.1=apply(NO2$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
saveRDS(average_load.1,paste(path_to_results,"betas_stability_pca_NO2.rds", sep =""))



