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
lasso = T
spls = F
cluster = F
bio = F
comb = T

## -----------------------------------------------------------------------------------------------------------------------
if(lasso){
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_selected_biomarker.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_selected_metab.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  combine = readRDS(paste(path, "Results_report/updates/denoised_selected_combined.rds", sep=""))
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj3/selected_lasso/"
  path_to_multi_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj3/selected_lasso/"
  ## sel prop ##
  bio_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio.rds",sep=""))
  clus_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus.rds",sep=""))
  combine_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine.rds",sep=""))
  ## out ## 
  out_bio = readRDS(paste(path_to_multi_Results_report, "out_bio.rds",sep=""))
  out_metab= readRDS(paste(path_to_multi_Results_report, "out_metab_clus.rds",sep=""))
  out_combine = readRDS(paste(path_to_multi_Results_report, "out_combine.rds",sep=""))
  ## betas ## 
  bio_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio.rds",sep=""))
  bio_metab  = readRDS(paste(path_to_multi_Results_report, "betas_clus.rds",sep=""))
  bio_combine = readRDS(paste(path_to_multi_Results_report, "betas_combine.rds",sep=""))
}

if(spls){
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_selected_biomarker.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_selected_metab.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  combine = readRDS(paste(path, "Results_report/updates/denoised_selected_combine.rds", sep=""))
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj3/spls/"
  path_to_multi_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj3/spls/"
  ## sel prop ##
  bio_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio.rds",sep=""))
  clus_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus.rds",sep=""))
  combine_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine.rds",sep=""))
  ## out ## 
  out_bio = readRDS(paste(path_to_multi_Results_report, "out_bio.rds",sep=""))
  out_metab= readRDS(paste(path_to_multi_Results_report, "out_metab_clus.rds",sep=""))
  out_combine = readRDS(paste(path_to_multi_Results_report, "out_combine.rds",sep=""))
  ## betas ## 
  bio_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio.rds",sep=""))
  bio_metab  = readRDS(paste(path_to_multi_Results_report, "betas_clus.rds",sep=""))
  bio_combine = readRDS(paste(path_to_multi_Results_report, "betas_combine.rds",sep=""))
}
## For univariate -----------------------------------------------------------------------------------------------------------------------
if(cluster){
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj4/"
  air_pval = readRDS(paste(path_to_Results_report, "pvalues_cluster_uni.rds", sep=""))
  air_beta = readRDS(paste(path_to_Results_report, "betas_cluster_uni.rds", sep =""))
}

if(bio){
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj4/"
  air_pval = readRDS(paste(path_to_Results_report, "pvalues_bio_uni.rds", sep=""))
  air_beta = readRDS(paste(path_to_Results_report, "betas_bio_uni.rds", sep =""))
}

if(comb){
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj4/"
  air_pval = readRDS(paste(path_to_Results_report, "pvalues_combine_uni.rds", sep=""))
  air_beta = readRDS(paste(path_to_Results_report, "betas_combine_uni.rds", sep =""))
}



# --------# making the legend for combined #-----------------------------------------------------------------
combine = readRDS(paste(path, "Results_report/updates/denoised_selected_combined.rds", sep=""))
combine = combine[order(as.numeric(row.names(combine))),] 
combine_col = colnames(combine) %>% as.data.frame()
biomarker = combine_col[1:13,] %>% as.data.frame()
biomarker$dataset<-rep("biomarker",times=nrow(biomarker))
colnames(biomarker)[1] = "features"
metab = combine_col[14:31,] %>% as.data.frame()
metab$dataset<-rep("metab",times=nrow(metab))
colnames(metab)[1] = "features"
combine_col<-rbind(biomarker,metab)
combine_col$features <- chartr(".", " ", combine_col$features)
combine_col$number = rownames(combine_col)
bio_col = combine_col[1:13,]
metab_col = combine_col[14:31,]


# ---------------ggplot combining all ----------------------------------------------------------

clus_sp = as.data.frame(clus_sp)
bio_sp = as.data.frame(bio_sp)
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

selection<-rbind(clus_sp,bio_sp,combine_sp)


#optimal selection proportion for each
clus_op_sp<-Argmax(out_metab)[2]
bio_op_sp<-Argmax(out_bio)[2]
combine_op_sp<-Argmax(out_combine)[2]

selection$encoding <- chartr(".", " ", selection$encoding)
mygrep = selection %>% group_by(encoding) %>% filter(sp>0.9) %>% .$encoding
selection = selection[which(selection$encoding %in% mygrep),]


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
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=13),
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
  geom_hline(yintercept = c(clus_op_sp,bio_op_sp,combine_op_sp), linetype="dashed", color = c("blue","brown","green4"))+
  ggsave(height=18, width=16,path = path_to_figure, filename="all_sp_lasso.pdf")


# ---------------------Correlation pattern----------------------------------------------------
## cluster ##

sp_clus = clus_sp %>% data.frame() %>% filter (. >= Argmax(out_metab)[2]) 
sp_clus_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_clus)))
mycor = cor(sp_clus_df)
pdf(paste(path_to_figure, "corr_clus.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_sp>=Argmax(out_metab)[2]) 

## biomarker ##

sp_bio = bio_sp %>% data.frame() %>% filter (. >= Argmax(out_bio)[2]) 
sp_bio_df = biomarker %>% select(which(colnames(biomarker) %in% rownames(sp_bio)))
mycor = cor(sp_bio_df)
pdf(paste(path_to_figure, "corr_bio.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(bio_sp>=Argmax(out_bio)[2]) 

## combined ##

sp_combine = combine_sp %>% data.frame() %>% filter (. >= Argmax(out_combine)[2]) 
sp_combine_df = combine %>% select(which(colnames(combine) %in% rownames(sp_combine)))
mycor = cor(sp_combine_df)
pdf(paste(path_to_figure, "corr_combine.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(combine_sp>=Argmax(out_combine)[2]) 


# ---------------------Uni-multi-volcano----------------------------------------------------
path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj3/selected_lasso/uni_lasso/"

## Univariate ## ##  78 variables ## 
pval = air_pval %>% as.data.frame()
bonf_threshold = -log10(0.05/nrow(pval))

beta = air_beta %>% as.data.frame()

# LASSO #

combine_sp = as.data.frame(combine_sp)
colnames(combine_sp)[1] = "sp"
combine_sp$encoding<-rownames(combine_sp)
combine_op_sp<-Argmax(out_combine)[2]

## Combining ## 
selection = cbind(combine_sp,pval,beta)
colnames(selection)[3]= "pval"
colnames(selection)[4]= "beta"
selection$encoding <- chartr(".", " ", selection$encoding)
selection$mycolour_point=rep(c("brown","blue"),times=c(13,18))
selection = selection %>% mutate(selection$newcolour,ifelse(-log10(pval)*sign(beta) < bonf_threshold & sp < combine_op_sp & -log10(pval)*sign(beta) > -bonf_threshold | sp <0.5, "grey", mycolour_point))
colnames(selection)[6]= "newcolour"
selection$features <- ifelse(selection$encoding == combine_col$features, combine_col$number, NA)


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
  xlab("Signed -log10(p-value)")+
  ylab("Selection Proportion of stability selection")+
  ggtitle("")+
  # xlim(xlim) +
  # ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf(paste(path_to_figure, "combine_uni_lasso.pdf",sep =""), width = 5.5, height = 5.5)
p1
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

