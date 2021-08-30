### Summmer Project -- Training lasso visual 
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


## -----------------------------------------------------------------------------------------------------------------------
if(lasso){
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_selected_biomarker.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_selected_metab.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  combine = readRDS(paste(path, "Results_report/updates/denoised_selected_combined.rds", sep=""))
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj3/selected_lasso/training/"
  path_to_multi_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj3/selected_lasso/training/"
  ## sel prop ##
  # bio_sp = readRDS(paste(path_to_multi_Results_report, "selprop_bio.rds",sep=""))
  # clus_sp = readRDS(paste(path_to_multi_Results_report, "selprop_metab_clus.rds",sep=""))
  combine_sp = readRDS(paste(path_to_multi_Results_report, "selprop_combine_train.rds",sep=""))
  ## out ## 
  # out_bio = readRDS(paste(path_to_multi_Results_report, "out_bio.rds",sep=""))
  # out_metab= readRDS(paste(path_to_multi_Results_report, "out_metab_clus.rds",sep=""))
  out_combine = readRDS(paste(path_to_multi_Results_report, "out_combine_train.rds",sep=""))
  ## betas ## 
  # bio_betas  = readRDS(paste(path_to_multi_Results_report, "betas_bio.rds",sep=""))
  # bio_metab  = readRDS(paste(path_to_multi_Results_report, "betas_clus.rds",sep=""))
  bio_combine = readRDS(paste(path_to_multi_Results_report, "auc_average_beta.rds",sep=""))
}


# Combined sel prop plots -------------------------------------------------

combine_sp=sort(combine_sp, decreasing=TRUE)
pdf(paste(path_to_figure, "lasso_sp_combine.pdf", sep =""), width = 5.5, height = 5.5)
par(mar=c(15,5,1,1))
plot(combine_sp, type="h", lwd=3, las=1, cex.lab=1.3, bty="L", ylim=c(0,1),
     col=ifelse(combine_sp>=Argmax(out_combine)[2],yes="red",no="grey"),
     xaxt="n", xlab="", ylab="Selection proportions", 
     main = "Selection proportion of selected air pollution related - biomarkers with CVD", cex.main=0.6)
abline(h=Argmax(out_combine)[2], lty=2, col="darkred")
for (i in 1:length(out_combine)){
  axis(side=1, at=i, labels=names(combine_sp)[i], las=2, cex.axis=0.4,
       col=ifelse(combine_sp[i]>=Argmax(out_combine)[2], yes="red", no="grey"),
       col.axis=ifelse(combine_sp[i]>=Argmax(out_combine)[2], yes="red", no="grey"))
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


