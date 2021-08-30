### Summmer Project -- Objective 1 : Multivariate - LASSO stability selection on metabolites and air pollutants
### 16th June - Alicia 


# Load packages
library(tidyverse)
library(colorspace)
library(plotrix)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

## -----------------------------------------------------------------------------------------------------------------------
met_no_bio = T
met_base = F
met_first= F

## -----------------------------------------------------------------------------------------------------------------------
if(met_no_bio){
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj2/Air_clus_bio/"
  path_to_multi_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj2/Air_clus_bio/"
  ## sel prop ##
  bio_sp = readRDS(paste(path_to_multi_results, "selprop_spls_bio.rds",sep=""))
  clus_sp = readRDS(paste(path_to_multi_results, "selprop_spls_metab_clus_base.rds",sep=""))
  combine_sp = readRDS(paste(path_to_multi_results, "selprop_spls_combine.rds",sep=""))
  ## out ## 
  bio_out = readRDS(paste(path_to_multi_results, "out_spls_bio.rds",sep=""))
  clus_out = readRDS(paste(path_to_multi_results, "out_spls_metab_clus_base.rds",sep=""))
  combine_out = readRDS(paste(path_to_multi_results, "out_spls_combine.rds",sep=""))
  ## loadings ## 
  bio_load = readRDS(paste(path_to_multi_results, "load_bio.rds",sep=""))
  clus_load = readRDS(paste(path_to_multi_results, "load_metab_clus_base.rds",sep=""))
  combine_load = readRDS(paste(path_to_multi_results, "load_combine.rds",sep=""))
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

if(met_first){
  path_to_uni_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Univariate_obj1/Air_met_first/"
  betas = readRDS(paste(path_to_uni_results, "betas_uni.rds", sep =""))
  betas = betas[,-1]
  pval = readRDS(paste(path_to_uni_results, "pvalues_uni.rds",sep =""))
  pval = pval[, -1]
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Obj1/Air_met_first/"
}

source(paste(path,"Functions/penalisation_functions.R", sep=""))

colnames(metab) = make.names(colnames(metab))
colnames(metab_clus) = make.names(colnames(metab_clus))

# ------------------Individual Metabolites sPLS selection prop v.s. loading  -------------------------------------------------------

## metab individual ##
tmp = rep(c(0.005,-0.005),length.out = length(metab_load))
names(tmp)=names(sort(metab_load))
x_nudge = tmp[names(metab_load)]

tmp = rep(c(0.02,-0.02),length.out = length(metab_sp))
names(tmp)=names(sort(metab_sp))
y_nudge = tmp[names(metab_sp)]

pdf(paste(path_to_figure, "spls_volcano.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(5,5,1,1))
plot(metab_load, metab_sp, las=1, pch=19,
     col=ifelse(abs(metab_sp) < 0.81,"grey","lightgoldenrod"),
     cex.lab=1, cex = 0.5, #xlim = c(-max(abs(metab_NO2_betas)),max(abs(metab_NO2_betas))),
     xlab="Mean betas coefficients", 
     ylab="Selection proportion")
legend("bottomright",pch=19,
       col = "lightgoldenrod",
       legend = "Stably selected metabolites", cex = 0.7, bg = "white")
text(metab_load+x_nudge, metab_sp+y_nudge,
     labels = ifelse(abs(metab_sp) < 0.81,"",names(metab_sp)),
     cex = 0.4)
dev.off()
sum(metab_sp>=0.81)

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


# -----------------------Sel prop plots--------------------------------------------------
## Visualisation of selection proportions by stability selection models 

## Biomarkers alone ##
bio_sp=sort(bio_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "sPLS_sp_bio.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(10,5,1,1))
plot(bio_sp, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(bio_sp>=Argmax(bio_out)[2], yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of biomarkers alone with multivariate response of air pollutants exposure",
     cex.main = 0.6)
abline(h=Argmax(bio_out)[2], lty=2, col="darkred")
for (i in 1:length(bio_sp)){
  axis(side=1, at=i, labels =names(bio_sp)[i], las=2,cex.axis = 0.5,
       col=ifelse(bio_sp[i]>=Argmax(bio_out)[2], yes="red", no="grey"),
       col.axis=ifelse(bio_sp[i]>=Argmax(bio_out)[2], yes="red", no="grey"))
}
dev.off()

## Clustered metabolites alone ##
clus_sp=sort(clus_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "sPLS_sp_clus.pdf", sep =""))
par(mar=c(15,5,1,1))
plot(clus_sp, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(clus_sp>=Argmax(clus_out)[2], yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of clusted metabolites alone with multivariate response of air pollutants exposure",
     cex.main = 0.5)
abline(h=Argmax(clus_out)[2], lty=2, col="darkred")
for (i in 1:length(clus_sp)){
  axis(side=1, at=i, labels =names(clus_sp)[i], las=2,cex.axis = 0.4,
       col=ifelse(clus_sp[i]>=Argmax(clus_out)[2], yes="red", no="grey"),
       col.axis=ifelse(clus_sp[i]>=Argmax(clus_out)[2], yes="red", no="grey"))
}
dev.off()

## Combine ##
combine_sp=sort(combine_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "sPLS_sp_combine.pdf", sep =""))
par(mar=c(15,5,1,1))
plot(combine_sp, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(combine_sp>=Argmax(combine_out)[2], yes="red", no="grey"), cex.lab=1.5,
     main = "Selection propotion of clusted metabolites and biomarkers with multivariate response",
     cex.main = 0.6)
abline(h=Argmax(combine_out)[2], lty=2, col="darkred")
for (i in 1:length(combine_sp)){
  axis(side=1, at=i, labels =names(combine_sp)[i], las=2,cex.axis = 0.4,
       col=ifelse(combine_sp[i]>=Argmax(combine_out)[2], yes="red", no="grey"),
       col.axis=ifelse(combine_sp[i]>=Argmax(combine_out)[2], yes="red", no="grey"))
}
dev.off()

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

selection <- filter(selection, sp>0.25)

#optimal selection proportion for each

clus_op_sp<-Argmax(clus_out)[2]
bio_op_sp<-Argmax(bio_out)[2]
combine_op_sp<-Argmax(combine_out)[2]

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
  geom_hline(yintercept = c(clus_op_sp,bio_op_sp,combine_op_sp), linetype="dashed", color = c("skyblue","green","red")) +
  # geom_hline(yintercept = clus_NO2_op_sp, ) + geom_hline(yintercept =bio_NO2_op_sp )+ geom_hline(yintercept =combine_NO2_op_sp)+
  ggsave(height=18, width=15,path = path_to_figure, filename="all_sp.pdf")

# ---------------------Correlation pattern----------------------------------------------------
sp = clus_sp %>% data.frame() %>% filter (. >= Argmax(clus_out)[2]) 
sp_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp)))
mycor = cor(sp_df)
pdf(paste(path_to_figure, "corr_clus.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()

sum(clus_sp>=Argmax(clus_out)[2]) # 20
sum(bio_sp>=Argmax(bio_out)[2])  # 1
sum(combine_sp>=Argmax(combine_out)[2])  # 26

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
