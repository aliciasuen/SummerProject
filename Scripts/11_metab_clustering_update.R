### Summmer Project -- Metabolomics Data Clustering
### 23/08/2021

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

# # -----------1) Using HC (ward) in VariableGrouping function--------------------------------------------------------------
# df = metab
# set.seed(123)
# hc_ward_out <- VariableGrouping(df, summary = NULL, agglomeration_method = "ward")
# hc_ward_hat_k <- which.max(hc_ward_out$score)
# hc_ward_grouping <- hc_ward_out$membership[hc_ward_hat_k, ]
# 
# ## using the variable most contributing to the first Principal Component is used.
# hc_ward_mysummary <- GroupingSummary(df, group = hc_ward_grouping, summary = "medoid")
# hc_ward_xsummarised <- hc_ward_mysummary$xsummarised
# 
# # metab_no_bio #
# saveRDS(hc_ward_out, paste(path, "Results_report/updates/hc_ward_out.rds", sep =""))
# saveRDS(hc_ward_mysummary, paste(path, "Results_report/updates/hc_ward_mysummary.rds",sep=""))
# saveRDS(hc_ward_xsummarised, paste(path,"Results_report/updates/cluster_hc_ward_metab_no_bio.rds",sep = ""))
# saveRDS(hc_ward_grouping, paste(path,"Dictionaries/grouping_hc_ward_metab_no_bio.rds",sep = ""))
# 
# #### Plotting Score v.s. # of the clusters ####
# ## vector of score measuring the balance between
# ##  separation and compactness for each grouping structure
# pdf(paste(path,"Figure_report/hiercluster/score_hc_ward_no_bio.pdf", sep=""))
# plot(hc_ward_out$score, xlab="Number of clusters", ylab="Score")
# abline(v=hc_ward_hat_k, col="darkred", lwd=1, lty=2)
# dev.off()
# 
# #### Plotting Explained variance of grouping v.s. # of the clusters ####
# pdf(paste(path,"Figure_report/hiercluster/R2_hc_ward_no_bio.pdf", sep=""))
# plot(hc_ward_out$R2, xlab="Number of clusters", ylab="Proportion of variance explained by the grouping")
# abline(v=hc_ward_hat_k, col="darkred")
# dev.off()
# 
# 
# #### Heatmap of the summarised data ####
# pdf(paste(path,"Figure_report/hiercluster/heatmap_hc_ward_no_bio.pdf", sep=""))
# pheatmap(cor(hc_ward_xsummarised, method = "pearson"), border = NA, show_rownames = F, show_colnames = F,
#          breaks = seq(-1, 1, length.out = 100))
# dev.off()
# 
# #### Plotting Legends grouping ####
# legend = hc_ward_mysummary$description[,1:2]
# legend$group = sort(legend$group)
# legend_1 = legend[1:79,]
# legend_2 = legend[80:160,]
# nb.cols <- max(legend$group)
# 
# pdf(paste(path,"Figure_report/hiercluster/legend_grouping_part1.pdf", sep=""),width=7, height=6)
# par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
# legend("topleft",
#        legend=c(paste0(legend_1$group,"-",legend_1$feature)),
#        bty='n', cex=0.6, ncol=2, text.width = 0.06)
# dev.off()
# 
# pdf(paste(path,"Figure_report/hiercluster/legend_grouping_part2.pdf", sep=""),width=6, height=6)
# par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
# legend("topleft",
#        legend=c(paste0(legend_2$group,"-",legend_2$feature)),
#        bty='n', cex=0.6, ncol=2, text.col,text.width = 0.09)
# dev.off()
# 
# #### Plotting maximum correlation between summarised features ####
# corr = hc_ward_mysummary$description[,c(1:4)]
# corr = corr[order(corr$max_cor),]
# corr$max_cor = sort(corr$max_cor, decreasing = TRUE)
# pdf(paste(path,"Figure_report/hiercluster/maxcorr_hc_ward_no_bio.pdf", sep=""))
# plot(x = corr$group,y = corr$max_cor, xlab="Number of clusters", ylab="Maximum and minimum correlation", pch = 18, col = "navy")
# points(x = corr$group,y = corr$min_cor, pch= 20, col = "lightgrey")
# legend("bottomright",cex = 0.7, text.col=c("navy", "lightgrey"),pch = c(18,20),col =c("navy", "lightgrey"), xpd=TRUE,legend=c("Maximum correlation", "Minimum correlation"))
# #abline(v=hc_ward_hat_k, col="darkred")
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 




# -------------2) Using HC (ward) with original function------------------------------------------------------------
df = metab
seeds_df_sc <- as.data.frame(scale(df))
summary(seeds_df_sc)

x <- t(seeds_df_sc)

dist_mat <- dist(x, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward')
plot(hclust_avg)