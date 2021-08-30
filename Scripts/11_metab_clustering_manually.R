### Summmer Project -- Manually clustering metabolomics 
### 11st July 2021 

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))


library(dplyr)
library(cluster)    # clustering algorithms
library(factoextra)
library(FactoMineR)
library(pheatmap)
library(ggplot2)
library(grDevices)
library(RColorBrewer)


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

# Loading Clustering Functions #
source(paste(path,"Functions/barbara_clustering.R", sep=""))

# Load annotation
metab_annot = read.csv(paste(path, "Dictionaries/Data_Dictionary_Showcase.csv",sep=""))
grouping = read.csv(paste(path, "Dictionaries/cluster_manually_update.csv", sep =""))
grouping_df = grouping

# Load dataset of imputed metabolite

metab_base = readRDS(paste(path,"Results_report/updates/imputed_metab_base.rds",sep = ""))

# Explore correlation   
colnames(metab_base)
mycorr = cor(metab_base)
pheatmap(mycorr, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)

write.csv(mycorr, paste(path, "Dictionaries/correlation_metab.csv", sep=""))
mycorr = as.data.frame(mycorr)



# Heatmap - Grouping 1  ---------------------------------------------------
metab_group1 = metab_base
remove = c("Apolipoprotein A1", "Glycoprotein Acetyls","Total Concentration of Branched-Chain Amino Acids (Leucine + Isoleucine + Valine)")
metab_group1 = metab_group1 %>% select(!matches(remove))
metab_group1 = metab_group1[,-51]
grouping = grouping %>% filter(grouping$X %in% colnames(metab_group1))

grouping_trial = grouping %>% group_split(Grouping)

df_trial = list()
mycorr = list()
for (i in 1:28){
  df_trial[[i]]= metab_group1[,colnames(metab_group1) %in% grouping_trial[[i]]$X]
  mycorr[[i]] = cor(as.data.frame(df_trial[[i]]))
}

for (i in c(1:28)) {
  pdf(paste(path, "Figure_report/Preliminary/cluster_manual_group1/cluster_manual_", i, ".pdf", sep = ""))
  pheatmap(mycorr[[i]], show_rownames = T, show_colnames = T,
           breaks = seq(-1, 1, length.out = 100), fontsize_row = 7, fontsize_col = 7)
  dev.off()
}

# Heatmap - Grouping 2  ---------------------------------------------------
metab_group2 = metab_no_bio
remove = c("Apolipoprotein A1", "Glycoprotein Acetyls")
metab_group2 = metab_group2 %>% select(!matches(remove))
grouping = grouping %>% filter(grouping$X %in% colnames(metab_group2))
grouping2 = unique(grouping$Grouping_2)

grouping_trial_2 = grouping %>% group_split(Grouping_2)

df_trial_2 = list()
mycorr = list()
for (i in 1:13){
  df_trial_2[[i]]= metab_no_bio[,colnames(metab_no_bio) %in% grouping_trial_2[[i]]$X]
  mycorr[[i]] = cor(as.data.frame(df_trial_2[[i]]))
}

for (i in c(1:13)) {
  pdf(paste(path, "Figure_report/Preliminary/cluster_manual_group2/cluster_manual_", i, ".pdf", sep = ""))
  pheatmap(mycorr[[i]], show_rownames = T, show_colnames = T,
           breaks = seq(-1, 1, length.out = 100), fontsize_row = 7, fontsize_col = 7)
  dev.off()
}


# Heatmap - Grouping 3  ---------------------------------------------------
metab_group3 = metab_no_bio
remove = c("Apolipoprotein A1", "Glycoprotein Acetyls","Total Concentration of Branched-Chain Amino Acids (Leucine + Isoleucine + Valine)")
metab_group3 = metab_group3 %>% select(!matches(remove))
metab_group3 = metab_group3[,-51]
grouping = grouping %>% filter(grouping$X %in% colnames(metab_group3))

grouping_trial = grouping %>% group_split(Grouping_3)

df_trial = list()
mycorr = list()
for (i in 1:21){
  df_trial[[i]]= metab_group1[,colnames(metab_group1) %in% grouping_trial[[i]]$X]
  mycorr[[i]] = cor(as.data.frame(df_trial[[i]]))
}

for (i in c(1:21)) {
  pdf(paste(path, "Figure_report/Preliminary/cluster_manual_group3/cluster_manual_", i, ".pdf", sep = ""))
  pheatmap(mycorr[[i]], show_rownames = T, show_colnames = T,
           breaks = seq(-1, 1, length.out = 100), fontsize_row = 7, fontsize_col = 7)
  dev.off()
}


# ----------Clustering GroupSummary() group 1 ---------------------------------------------------------------

# Grouping structure
rownames(grouping) = grouping$X
group1 = factor(grouping$Grouping)
group1 = as.numeric(group1)
names(group1) = grouping$X

# Data wrangling
metab_group1 = as.matrix(metab_group1)

# Summary of the groups
mysummary_group1 <- GroupingSummary(metab_group1, group = group1, summary = "medoid")
xsummarised_group1 <- mysummary_group1$xsummarised

# Adding "Apolipoprotein A1", "Glycoprotein Acetyls" back in the dataset 
xsummarised_group1 = as.data.frame(xsummarised_group1)
xsummarised_group1$"Apolipoprotein A1" = metab_base$"Apolipoprotein A1"
xsummarised_group1$"Glycoprotein Acetyls" = metab_base$"Glycoprotein Acetyls"
xsummarised_group1$"Total Concentration of Branched-Chain Amino Acids (Leucine + Isoleucine + Valine)" = metab_base$"Total Concentration of Branched-Chain Amino Acids (Leucine + Isoleucine + Valine)"
xsummarised_group1 = as.matrix(xsummarised_group1)

mycorr = cor(xsummarised_group1)

pdf(paste(path,"Figure_report/Preliminary/cluster_manual_group1/cluster_corr_medoid.pdf", sep=""), width = 10, height = 7)
pheatmap(mycorr, show_rownames = T, show_colnames = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 7, fontsize_col = 7)
dev.off()


saveRDS(xsummarised_group1, paste(path,"Results_report/cluster_manual/cluster_group1_centroid_base.rds",sep = ""))
saveRDS(mysummary_group1, paste(path,"Dictionaries/cluster_manual/grouping_cluster_group1_centroid_base.rds",sep = ""))



# ----------Clustering GroupSummary() group 2---------------------------------------------------------------

# Grouping structure
rownames(grouping) = grouping$X
group2 = factor(grouping$Grouping_2)
group2 = as.numeric(group2)
names(group2) = grouping$X

# Data wrangling
metab_group2 = as.matrix(metab_group2)

# Summary of the groups
mysummary_group2 <- GroupingSummary(metab_group2, group = group2, summary = "medoid")
xsummarised_group2 <- mysummary_group2$xsummarised

# Adding "Apolipoprotein A1", "Glycoprotein Acetyls" back in the dataset 
xsummarised_group2 = as.data.frame(xsummarised_group2)
xsummarised_group2$"Apolipoprotein A1" = metab_no_bio$"Apolipoprotein A1"
xsummarised_group2$"Glycoprotein Acetyls" = metab_no_bio$"Glycoprotein Acetyls"
xsummarised_group2 = as.matrix(xsummarised_group2)

mycorr = cor(xsummarised_group2)

pdf(paste(path,"Figure_report/Preliminary/cluster_manual_group2/cluster_corr_medoid.pdf", sep=""))
pheatmap(mycorr, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 7, fontsize_col = 7)
dev.off()

saveRDS(xsummarised_group2, paste(path,"Results_report/cluster_manual/cluster_group2_centroid_base.rds",sep = ""))
saveRDS(mysummary_group2, paste(path,"Dictionaries/cluster_manual/grouping_cluster_group2_centroid_base.rds",sep = ""))

## Plotting maximum correlation between summarised features
max = grouping_cluster_group2_medoid_base$description[,c(1,2,4)]
nb.cols <- max(max$group)
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
pdf(paste(path,"Figure_report/Preliminary/cluster_manual_group2/maxcorr_med_clusters.pdf", sep=""))
ggplot(max, aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)))+
  geom_point(aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)), size = 1)+
  scale_fill_manual(values = mycolors)+
  labs(x="Metabolomic features",y="Maximum correlation", title = "Maximum correlation between summarised features by medoid")+
  theme_bw()+
  theme(text = element_text(size=8),
        axis.text.x = element_text(face = "bold", angle = 70, hjust = 1, size = 5),
        legend.position = "none")
dev.off()


# ----------Clustering GroupSummary() group 3 ---------------------------------------------------------------

# Grouping structure
rownames(grouping) = grouping$X
group1 = factor(grouping$Grouping_3)
group1 = as.numeric(group1)
names(group1) = grouping$X

# Data wrangling
metab_group3 = as.matrix(metab_group3)

# Summary of the groups
mysummary_group1 <- GroupingSummary(metab_group3, group = group1, summary = "medoid")
xsummarised_group1 <- mysummary_group1$xsummarised

# Adding "Apolipoprotein A1", "Glycoprotein Acetyls" back in the dataset 
xsummarised_group1 = as.data.frame(xsummarised_group1)
xsummarised_group1$"Apolipoprotein A1" = metab_no_bio$"Apolipoprotein A1"
xsummarised_group1$"Glycoprotein Acetyls" = metab_no_bio$"Glycoprotein Acetyls"
xsummarised_group1$"Total Concentration of Branched-Chain Amino Acids (Leucine + Isoleucine + Valine)" = metab_no_bio$"Total Concentration of Branched-Chain Amino Acids (Leucine + Isoleucine + Valine)"
xsummarised_group1 = as.matrix(xsummarised_group1)

mycorr = cor(xsummarised_group1)

pdf(paste(path,"Figure_report/Preliminary/cluster_manual_group3/cluster_corr_medoid.pdf", sep=""), width = 10, height = 7)
pheatmap(mycorr, show_rownames = T, show_colnames = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 7, fontsize_col = 7)
dev.off()


saveRDS(xsummarised_group1, paste(path,"Results_report/cluster_manual/cluster_group1_centroid_base.rds",sep = ""))
saveRDS(mysummary_group1, paste(path,"Dictionaries/cluster_manual/grouping_cluster_group1_centroid_base.rds",sep = ""))

## Plotting maximum correlation between summarised features
max = grouping_cluster_group1_medoid_base$description[,c(1,2,4)]
nb.cols <- max(max$group)
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
pdf(paste(path,"Figure_report/Preliminary/cluster_manual_group1/maxcorr_med_clusters.pdf", sep=""))
ggplot(max, aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)))+
  geom_point(aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)), size = 1)+
  scale_fill_manual(values = mycolors)+
  labs(x="Metabolomic features",y="Maximum correlation", title = "Maximum correlation between summarised features by medoid")+
  theme_bw()+
  theme(text = element_text(size=8),
        axis.text.x = element_text(face = "bold", angle = 70, hjust = 1, size = 5),
        legend.position = "none")
dev.off()






# ----------Plotting max and min correlation group 1--------------------------------------------------------------
#### Plotting maximum correlation between summarised features ####
corr = mysummary_group1$description[,c(1:4)]
corr = corr[order(corr$max_cor),]
corr$max_cor = sort(corr$max_cor, decreasing = TRUE)
pdf(paste(path,"Figure_report/Preliminary/cluster_manual_group1/maxcorr_group1.pdf", sep=""))
plot(x = corr$group,y = corr$max_cor, xlab="Number of clusters", ylab="Maximum and minimum correlation", pch = 18, col = "navy")
points(x = corr$group,y = corr$min_cor, pch= 20, col = "lightgrey")
legend("bottomright",cex = 0.7, text.col=c("navy", "lightgrey"),pch = c(18,20),col =c("navy", "lightgrey"), xpd=TRUE,legend=c("Maximum correlation", "Minimum correlation"))
#abline(v=hc_ward_hat_k, col="darkred")
dev.off()


# ------------------------Trial according to the groups-------------------------------------------------

## extracting all HDLs ##
HDL = mycorr %>%
  dplyr::select(contains('HDL'))

col_HDL = colnames(HDL)
HDL = filter(HDL, rownames(HDL) %in% col_HDL)
HDL = as.matrix(HDL)
pdf("../Figure_report/Preliminary/cluster_man_HDL.pdf")
pheatmap(HDL, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)
dev.off()
remove = c("Triglycerides in Small HDL", "Triglycerides in HDL", "Triglycerides in Medium HDL", "Triglycerides in Very Large HDL", "Triglycerides in Large HDL", "Total Cholesterol Minus HDL-C", "Remnant Cholesterol (Non-HDL, Non-LDL -Cholesterol)",
"Cholesteryl Esters in Small HDL", "Concentration of Small HDL Particles", "Cholesterol in Small HDL", "Free Cholesterol in Small HDL", "Total Lipids in Small HDL", "Phospholipids in Small HDL")
HDL = as.data.frame(HDL)
HDL = HDL %>% select(!matches(remove))
HDL = HDL[, -1]
col_HDL = colnames(HDL)
HDL = filter(HDL, rownames(HDL) %in% col_HDL)
HDL = as.matrix(HDL)
pheatmap(HDL, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)

## extracting all Phospholipids ##
phospholipid = mycorr %>%
  dplyr::select(contains('Phospholipids'))

col_phospho = colnames(phospholipid)
phospholipid = filter(phospholipid, rownames(phospholipid) %in% col_phospho)
phospholipid = as.matrix(phospholipid)
pheatmap(phospholipid, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)


## extracting all "clincially validated" biomarkers (28) ##
clin_val = c("Total cholesterol", "VLDL cholesterol", "Clinical LDL cholesterol", "HDL cholesterol", "Total triglycerides", "Total triglycerides", "Omega-3 fatty acids", "Omega-6 fatty acids", "Polyunsaturated fatty acids", "Monounsaturated fatty acids", "Saturated fatty acids", "Docosahexaenoic acid", "Apolipoprotein B", "Apolipoprotein A1", "Alanine", "Glycine", "Histidine", "Total Concentration of Branched-Chain Amino Acids (Leucine + Isoleucine + Valine)", "Isoleucine", "Leucine", "Valine", "Phenylalanine", "Tyrosine", "Glucose", "Lactate", "Creatinine", "Albumin", "Glycoprotein acetyls")
clinically_biomarkers = mycorr %>%
  dplyr::select(matches(clin_val))

col_clin_val = colnames(clinically_biomarkers)
clinically_biomarkers = filter(clinically_biomarkers, rownames(clinically_biomarkers) %in% col_clin_val)
clinically_biomarkers = as.matrix(clinically_biomarkers)
pheatmap(clinically_biomarkers, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)

## extracting all cholesterol ##
cholesterol = mycorr %>%
  dplyr::select(contains('Cholesterol'))

col_chol = colnames(cholesterol)
cholesterol = filter(cholesterol, rownames(cholesterol) %in% col_chol)
cholesterol = as.matrix(cholesterol)
pheatmap(cholesterol, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)


## extracting all "Triglycerides" ##
trigly = mycorr %>%
  dplyr::select(contains('Triglycerides'))

col_trigly = colnames(trigly)
trigly = filter(trigly, rownames(trigly) %in% col_trigly)
trigly = as.matrix(trigly)
pheatmap(trigly, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)

## extracting all "Total lipids" ##
lipid = mycorr %>%
  dplyr::select(contains('Total lipids'))

col_lip = colnames(lipid)
lipid = filter(lipid, rownames(lipid) %in% col_lip)
lipid = as.matrix(lipid)
pheatmap(lipid, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)

## extracting all "Free cholesterol" ##
free_chol = mycorr %>%
  dplyr::select(contains('Free cholesterol'))

col_free_chol = colnames(free_chol)
free_chol = filter(free_chol, rownames(free_chol) %in% col_free_chol)
free_chol = as.matrix(free_chol)
pheatmap(free_chol, show_rownames = T, show_colnames = T,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5, fontsize_col = 5)

