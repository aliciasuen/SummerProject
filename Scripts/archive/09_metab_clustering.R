### Summmer Project -- Metabolomics Data Clustering
### 1st July 2021 

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
metab = readRDS(paste(path, "Results_report/updates/imputed_metab_base.rds", sep =""))
train70 = readRDS(paste(path, "Results_report/updates/split/imputed_metab_train70.rds", sep =""))
test30 = readRDS(paste(path, "Results_report/updates/split/imputed_metab_test30.rds", sep =""))

source(paste(path,"Functions/barbara_clustering.R", sep=""))

# -----------1) Using HC (ward) in VariableGrouping function--------------------------------------------------------------
df = metab
set.seed(123)
hc_ward_out <- VariableGrouping(df, summary = NULL, agglomeration_method = "ward")
hc_ward_hat_k <- which.max(hc_ward_out$score)
hc_ward_grouping <- hc_ward_out$membership[hc_ward_hat_k, ]

## using the variable most contributing to the first Principal Component is used.
hc_ward_mysummary <- GroupingSummary(df, group = hc_ward_grouping, summary = "medoid")
hc_ward_xsummarised <- hc_ward_mysummary$xsummarised

# metab_no_bio #
saveRDS(hc_ward_out, paste(path, "Results_report/updates/hc_ward_out.rds", sep =""))
saveRDS(hc_ward_mysummary, paste(path, "Results_report/updates/hc_ward_mysummary.rds",sep=""))
saveRDS(hc_ward_xsummarised, paste(path,"Results_report/updates/cluster_hc_ward_metab_no_bio.rds",sep = ""))
saveRDS(hc_ward_grouping, paste(path,"Dictionaries/grouping_hc_ward_metab_no_bio.rds",sep = ""))

# # metab_base #
# saveRDS(hc_ward_out, paste(path, "Results_report/hc_ward_out_base.rds", sep =""))
# saveRDS(hc_ward_mysummary, paste(path, "Results_report/hc_ward_mysummary_base.rds",sep=""))
# saveRDS(hc_ward_xsummarised, paste(path,"Results_report/cluster_hc_ward_metab_base.rds",sep = ""))
# saveRDS(hc_ward_grouping, paste(path,"Dictionaries/grouping_hc_ward_metab_base.rds",sep = ""))

# Loading metab_no_bio datasets #
hc_ward_out = readRDS(paste(path, "Results_report/hc_ward_out.rds", sep =""))
hc_ward_mysummary = readRDS(paste(path, "Results_report/hc_ward_mysummary.rds",sep=""))
hc_ward_xsummarised = readRDS(paste(path,"Results_report/cluster_hc_ward_metab_no_bio.rds",sep = ""))
hc_ward_grouping = readRDS(paste(path,"Dictionaries/grouping_hc_ward_metab_no_bio.rds",sep = ""))

# # Loading metab_base datasets #
# hc_ward_out = readRDS(paste(path, "Results_report/hc_ward_out_base.rds", sep =""))
# hc_ward_mysummary = readRDS(paste(path, "Results_report/hc_ward_mysummary_base.rds",sep=""))
# hc_ward_xsummarised = readRDS(paste(path,"Results_report/cluster_hc_ward_metab_base.rds",sep = ""))
# hc_ward_grouping = readRDS(paste(path,"Dictionaries/grouping_hc_ward_metab_base.rds",sep = ""))

## Plots of HC - "ward"
hc_ward_hat_k <- which.max(hc_ward_out$score)
#### Plotting Score v.s. # of the clusters ####
## vector of score measuring the balance between
##  separation and compactness for each grouping structure
pdf(paste(path,"Figure_report/hiercluster/score_hc_ward_no_bio.pdf", sep=""))
plot(hc_ward_out$score, xlab="Number of clusters", ylab="Score")
abline(v=hc_ward_hat_k, col="darkred", lwd=1, lty=2)
dev.off()

#### Plotting Explained variance of grouping v.s. # of the clusters ####
pdf(paste(path,"Figure_report/hiercluster/R2_hc_ward_no_bio.pdf", sep=""))
plot(hc_ward_out$R2, xlab="Number of clusters", ylab="Proportion of variance explained by the grouping")
abline(v=hc_ward_hat_k, col="darkred")
dev.off()


#### Heatmap of the summarised data ####
pdf(paste(path,"Figure_report/hiercluster/heatmap_hc_ward_no_bio.pdf", sep=""))
pheatmap(cor(hc_ward_xsummarised, method = "pearson"), border = NA, show_rownames = F, show_colnames = F,
         breaks = seq(-1, 1, length.out = 100))
dev.off()

#### Plotting Legends grouping ####
legend = hc_ward_mysummary$description[,1:2]
legend$group = sort(legend$group)
legend_1 = legend[1:79,]
legend_2 = legend[80:160,]
nb.cols <- max(legend$group)

pdf(paste(path,"Figure_report/hiercluster/legend_grouping_part1.pdf", sep=""),width=7, height=6)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c(paste0(legend_1$group,"-",legend_1$feature)),
       bty='n', cex=0.6, ncol=2, text.width = 0.06)
dev.off()

pdf(paste(path,"Figure_report/hiercluster/legend_grouping_part2.pdf", sep=""),width=6, height=6)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c(paste0(legend_2$group,"-",legend_2$feature)),
       bty='n', cex=0.6, ncol=2, text.col,text.width = 0.09)
dev.off()

#### Plotting maximum correlation between summarised features ####
corr = hc_ward_mysummary$description[,c(1:4)]
corr = corr[order(corr$max_cor),]
corr$max_cor = sort(corr$max_cor, decreasing = TRUE)
pdf(paste(path,"Figure_report/hiercluster/maxcorr_hc_ward_no_bio.pdf", sep=""))
plot(x = corr$group,y = corr$max_cor, xlab="Number of clusters", ylab="Maximum and minimum correlation", pch = 18, col = "navy")
points(x = corr$group,y = corr$min_cor, pch= 20, col = "lightgrey")
legend("bottomright",cex = 0.7, text.col=c("navy", "lightgrey"),pch = c(18,20),col =c("navy", "lightgrey"), xpd=TRUE,legend=c("Maximum correlation", "Minimum correlation"))
#abline(v=hc_ward_hat_k, col="darkred")
dev.off()















# -----------------old--------------------------------------------------------

# Load dataset of imputed metabolite
metab_base = readRDS(paste(path,"Results_report/imputed_metab_base.rds",sep = ""))
metab_no_bio = readRDS(paste(path, "Results_report/metab_no_bio.rds",sep=""))
df = metab_no_bio
# df = df %>% sample_frac(.05)


source(paste(path,"Functions/barbara_clustering.R", sep=""))

# -----------1) Using HC (ward) in VariableGrouping function--------------------------------------------------------------
set.seed(123)
hc_ward_out <- VariableGrouping(df, summary = NULL, agglomeration_method = "ward")
hc_ward_hat_k <- which.max(hc_ward_out$score)
hc_ward_grouping <- hc_ward_out$membership[hc_ward_hat_k, ]

## using the variable most contributing to the first Principal Component is used.
hc_ward_mysummary <- GroupingSummary(df, group = hc_ward_grouping, summary = "medoid")
hc_ward_xsummarised <- hc_ward_mysummary$xsummarised

# metab_no_bio #
saveRDS(hc_ward_out, paste(path, "Results_report/hc_ward_out.rds", sep =""))
saveRDS(hc_ward_mysummary, paste(path, "Results_report/hc_ward_mysummary.rds",sep=""))
saveRDS(hc_ward_xsummarised, paste(path,"Results_report/cluster_hc_ward_metab_no_bio.rds",sep = ""))
saveRDS(hc_ward_grouping, paste(path,"Dictionaries/grouping_hc_ward_metab_no_bio.rds",sep = ""))

# # metab_base #
# saveRDS(hc_ward_out, paste(path, "Results_report/hc_ward_out_base.rds", sep =""))
# saveRDS(hc_ward_mysummary, paste(path, "Results_report/hc_ward_mysummary_base.rds",sep=""))
# saveRDS(hc_ward_xsummarised, paste(path,"Results_report/cluster_hc_ward_metab_base.rds",sep = ""))
# saveRDS(hc_ward_grouping, paste(path,"Dictionaries/grouping_hc_ward_metab_base.rds",sep = ""))

# Loading metab_no_bio datasets #
hc_ward_out = readRDS(paste(path, "Results_report/hc_ward_out.rds", sep =""))
hc_ward_mysummary = readRDS(paste(path, "Results_report/hc_ward_mysummary.rds",sep=""))
hc_ward_xsummarised = readRDS(paste(path,"Results_report/cluster_hc_ward_metab_no_bio.rds",sep = ""))
hc_ward_grouping = readRDS(paste(path,"Dictionaries/grouping_hc_ward_metab_no_bio.rds",sep = ""))

# # Loading metab_base datasets #
# hc_ward_out = readRDS(paste(path, "Results_report/hc_ward_out_base.rds", sep =""))
# hc_ward_mysummary = readRDS(paste(path, "Results_report/hc_ward_mysummary_base.rds",sep=""))
# hc_ward_xsummarised = readRDS(paste(path,"Results_report/cluster_hc_ward_metab_base.rds",sep = ""))
# hc_ward_grouping = readRDS(paste(path,"Dictionaries/grouping_hc_ward_metab_base.rds",sep = ""))

## Plots of HC - "ward"
hc_ward_hat_k <- which.max(hc_ward_out$score)
#### Plotting Score v.s. # of the clusters ####
## vector of score measuring the balance between
##  separation and compactness for each grouping structure
pdf(paste(path,"Figure_report/hiercluster/score_hc_ward_no_bio.pdf", sep=""))
plot(hc_ward_out$score, xlab="Number of clusters", ylab="Score")
abline(v=hc_ward_hat_k, col="darkred", lwd=1, lty=2)
dev.off()

#### Plotting Explained variance of grouping v.s. # of the clusters ####
pdf(paste(path,"Figure_report/hiercluster/R2_hc_ward_no_bio.pdf", sep=""))
plot(hc_ward_out$R2, xlab="Number of clusters", ylab="Proportion of variance explained by the grouping")
abline(v=hc_ward_hat_k, col="darkred")
dev.off()


#### Heatmap of the summarised data ####
pdf(paste(path,"Figure_report/hiercluster/heatmap_hc_ward_no_bio.pdf", sep=""))
pheatmap(cor(hc_ward_xsummarised, method = "pearson"), border = NA, show_rownames = F, show_colnames = F,
         breaks = seq(-1, 1, length.out = 100))
dev.off()

#### Plotting Legends grouping ####
legend = hc_ward_mysummary$description[,1:2]
legend$group = sort(legend$group)
legend_1 = legend[1:79,]
legend_2 = legend[80:160,]
nb.cols <- max(legend$group)

pdf(paste(path,"Figure_report/hiercluster/legend_grouping_part1.pdf", sep=""),width=7, height=6)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c(paste0(legend_1$group,"-",legend_1$feature)),
       bty='n', cex=0.6, ncol=2, text.width = 0.06)
dev.off()

pdf(paste(path,"Figure_report/hiercluster/legend_grouping_part2.pdf", sep=""),width=6, height=6)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c(paste0(legend_2$group,"-",legend_2$feature)),
       bty='n', cex=0.6, ncol=2, text.col,text.width = 0.09)
dev.off()

#### Plotting maximum correlation between summarised features ####
corr = hc_ward_mysummary$description[,c(1:4)]
corr = corr[order(corr$max_cor),]
corr$max_cor = sort(corr$max_cor, decreasing = TRUE)
pdf(paste(path,"Figure_report/hiercluster/maxcorr_hc_ward_no_bio.pdf", sep=""))
plot(x = corr$group,y = corr$max_cor, xlab="Number of clusters", ylab="Maximum and minimum correlation", pch = 18, col = "navy")
points(x = corr$group,y = corr$min_cor, pch= 20, col = "lightgrey")
legend("bottomright",cex = 0.7, text.col=c("navy", "lightgrey"),pch = c(18,20),col =c("navy", "lightgrey"), xpd=TRUE,legend=c("Maximum correlation", "Minimum correlation"))
#abline(v=hc_ward_hat_k, col="darkred")
dev.off()


# # -----------2) Using HC (complete) in VariableGrouping function--------------------------------------------------------------
# set.seed(123)
# hc_comp_out <- VariableGrouping(df, summary = NULL, agglomeration_method = "complete")
# hc_comp_hat_k <- which.max(hc_comp_out$score)
# hc_comp_grouping <- hc_comp_out$membership[hc_comp_hat_k, ]
#
# ## using the variable most contributing to the first Principal Component is used.
# hc_comp_mysummary <- GroupingSummary(df, group = hc_comp_grouping, summary = "medoid")
# hc_comp_xsummarised <- hc_comp_mysummary$xsummarised
#
# saveRDS(hc_comp_xsummarised, paste(path,"Results_report/cluster_hc_comp_metab_base.rds",sep = ""))
# saveRDS(hc_comp_grouping, paste(path,"Dictionaries/grouping_hc_comp_metab_base.rds",sep = ""))
#
#
# # Plots of HC - "complete" ----------------------------------------------------
#
# ## Plotting Score v.s. # of the clusters ##
# pdf(paste(path,"Figure_report/score_hc_comp_clusters.pdf", sep=""), width = 5, height = 5)
# plot(hc_comp_out$score, xlab="Number of clusters", ylab="Score", main= "Score of clusters using HC (complete)")
# abline(v=hc_comp_hat_k, col="darkred")
# dev.off()
#
# ## Plotting R2 v.s. # of the clusters
# pdf(paste(path,"Figure_report/R2_hc_comp_clusters.pdf", sep=""), width = 5, height = 5)
# plot(hc_comp_out$R2, xlab="Number of clusters", ylab="R2", main= "R2 of clusters using HC (complete)")
# abline(v=hc_comp_hat_k, col="darkred")
# dev.off()
#
#
# ## Heatmap of the summarised data ##
# pdf(paste(path,"Figure_report/heatmap_hc_comp_clusters.pdf", sep=""),width = 5, height = 5)
# pheatmap(cor(hc_comp_xsummarised, method = "spearman"), border = NA, show_rownames = F, show_colnames = F,
#          breaks = seq(-1, 1, length.out = 100))
# dev.off()
#
# ## Plotting maximum correlation between summarised features
# max = hc_comp_mysummary$description[,c(1,2,4)]
# nb.cols <- max(max$group)
# mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
# pdf(paste(path,"Figure_report/maxcorr_hc_comp_clusters.pdf", sep=""))
# ggplot(max, aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)))+
#         geom_point(aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)), size = 1)+
#         scale_fill_manual(values = mycolors)+
#         labs(x="Metabolomic features",y="Maximum correlation", title = "Maximum correlation between summarised features by HC (complete)")+
#         theme_bw()+
#         theme(text = element_text(size=8),
#               axis.text.x = element_text(face = "bold", angle = 70, hjust = 1, size = 5),
#               legend.position = "none")
# dev.off()
#
# # -----------2) Using HC (single) in VariableGrouping function--------------------------------------------------------------
# set.seed(123)
# hc_sing_out <- VariableGrouping(df, summary = NULL, agglomeration_method = "single")
# hc_sing_hat_k <- which.max(hc_sing_out$score)
# hc_sing_grouping <- hc_sing_out$membership[hc_sing_hat_k, ]
#
# ## using the variable most contributing to the first Principal Component is used.
# hc_sing_mysummary <- GroupingSummary(df, group = hc_sing_grouping, summary = "medoid")
# hc_sing_xsummarised <- hc_sing_mysummary$xsummarised
#
# saveRDS(hc_sing_xsummarised, paste(path,"Results_report/cluster_hc_sing_metab_base.rds",sep = ""))
# saveRDS(hc_sing_grouping, paste(path,"Dictionaries/grouping_hc_sing_metab_base.rds",sep = ""))
#
#
# # Plots of HC - "single" ----------------------------------------------------
#
# ## Plotting Score v.s. # of the clusters ##
# pdf(paste(path,"Figure_report/score_hc_sing_clusters.pdf", sep=""), width = 5, height = 5)
# plot(hc_sing_out$score, xlab="Number of clusters", ylab="Score", main= "Score of clusters using HC (single)")
# abline(v=hc_sing_hat_k, col="darkred")
# dev.off()
#
# ## Plotting R2 v.s. # of the clusters
# pdf(paste(path,"Figure_report/R2_hc_sing_clusters.pdf", sep=""), width = 5, height = 5)
# plot(hc_sing_out$R2, xlab="Number of clusters", ylab="R2", main= "R2 of clusters using HC (single)")
# abline(v=hc_sing_hat_k, col="darkred")
# dev.off()
#
#
# ## Heatmap of the summarised data ##
# pdf(paste(path,"Figure_report/heatmap_hc_sing_clusters.pdf", sep=""),width = 5, height = 5)
# pheatmap(cor(hc_sing_xsummarised, method = "spearman"), border = NA, show_rownames = F, show_colnames = F,
#          breaks = seq(-1, 1, length.out = 100))
# dev.off()
#
# ## Plotting maximum correlation between summarised features
# max = hc_sing_mysummary$description[,c(1,2,4)]
# nb.cols <- max(max$group)
# mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
# pdf(paste(path,"Figure_report/maxcorr_hc_sing_clusters.pdf", sep=""))
# ggplot(max, aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)))+
#         geom_point(aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)), size = 1)+
#         scale_fill_manual(values = mycolors)+
#         labs(x="Metabolomic features",y="Maximum correlation", title = "Maximum correlation between summarised features by HC (single)")+
#         theme_bw()+
#         theme(text = element_text(size=8),
#               axis.text.x = element_text(face = "bold", angle = 70, hjust = 1, size = 5),
#               legend.position = "none")
# dev.off()
#
# # -------------2) Using variable most contributing to PC1 as medoid------------------------------------------------------------
#
# med_out <- VariableGrouping(df, summary = "medoid")
# med_hat_k <- which.max(med_out$score)
# med_grouping <- med_out$membership[med_hat_k, ]
#
# med_mysummary <- GroupingSummary(df, group = med_grouping, summary = "medoid")
# med_xsummarised <- med_mysummary$xsummarised
#
# saveRDS(med_xsummarised, paste(path,"Results_report/cluster_medoid_metab_base.rds",sep = ""))
# saveRDS(med_grouping, paste(path,"Dictionaries/grouping_medoid_metab_base.rds",sep = ""))
#
#
# # Plots of medoid ----------------------------------------------------
#
# ## Plotting Score v.s. # of the clusters ##
# pdf(paste(path,"Figure_report/score_med_clusters.pdf", sep=""), width = 5, height = 5)
# plot(med_out$score, xlab="Number of clusters", ylab="Score", main= "Score of clusters using medoid")
# abline(v=med_hat_k, col="darkred")
# dev.off()
#
# ## Plotting R2 v.s. # of the clusters
# pdf(paste(path,"Figure_report/R2_med_clusters.pdf", sep=""), width = 5, height = 5)
# plot(med_out$R2, xlab="Number of clusters", ylab="R2", main= "R2 of clusters using medoid")
# abline(v=med_hat_k, col="darkred")
# dev.off()
#
#
# ## Heatmap of the summarised data ##
# pdf(paste(path,"Figure_report/heatmap_medoid_clusters.pdf", sep=""),width = 5, height = 5)
# pheatmap(cor(med_xsummarised, method = "spearman"), show_rownames = FALSE, show_colnames = FALSE,
#          breaks = seq(-1, 1, length.out = 100))
# dev.off()
#
# ## Plotting maximum correlation between summarised features
# max = med_mysummary$description[,c(1,2,4)]
# nb.cols <- max(max$group)
# mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
# pdf(paste(path,"Figure_report/maxcorr_med_clusters.pdf", sep=""))
# ggplot(max, aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)))+
#         geom_point(aes(x = reorder(feature, -max_cor), y = max_cor, color = factor(group)), size = 1)+
#         scale_fill_manual(values = mycolors)+
#         labs(x="Metabolomic features",y="Maximum correlation", title = "Maximum correlation between summarised features by medoid")+
#         theme_bw()+
#         theme(text = element_text(size=8),
#               axis.text.x = element_text(face = "bold", angle = 70, hjust = 1, size = 5),
#               legend.position = "none")
# dev.off()

# -------------------------------------------------------------------------

print(paste("The number of cluster using hc (ward) is", hc_ward_hat_k))
# print(paste("The number of cluster using hc (comp) is", hc_comp_hat_k))
# print(paste("The number of cluster using hc (single) is", hc_sing_hat_k))
# print(paste("The number of cluster using medoid is", med_hat_k))



# # -----------3) Using Hierarchical Clustering on Principal Components --------------------------------------------------------------
#
# res.pca <- FactoMineR::PCA(t(df), ncp = 3, graph = FALSE)
# eig.val <- res.pca$eig
# barplot(eig.val[, 2],
#         names.arg = 1:nrow(eig.val),
#         main = "Variances Explained by PCs (%)",
#         xlab = "Principal Components",
#         ylab = "Percentage of variances",
#         col ="steelblue")
#
# res.hcpc <- FactoMineR::HCPC(res.pca, graph = FALSE)
# pdf(paste(path, "Figure_report/cluster_HCPC.pdf", sep =""))
# fviz_dend(res.hcpc,
#           cex = 0.8,                     # Label size
#           palette = "jco",               # Color palette see ?ggpubr::ggpar
#           rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
#           rect_border = "jco",           # Rectangle color
#           labels_track_height = 0.8      # Augment the room for labels
# )
# dev.off()
#
