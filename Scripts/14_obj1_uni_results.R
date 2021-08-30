### Summmer Project -- Objective 1 : Each exposure v.s. each metabolomic feature (Univariate)
### 20th July 2021 - Alicia 

## Q: Which metabolites are statistically correlated with which exposures in the data?
## --> Univariate linear regression between all possible pairs: setting a given exposure as the outcome and a given metabolite as the predictor 

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))

# Load packages
library(openxlsx)
library(data.table)
library(dplyr)
library(tidyverse)

## 24003: NO2(2010)
## 24004: NOx(2010)
## 24005: PM10(2010)
## 24006: PM2.5(2010) 
## 24007: PM2.5 absorbance(2010) 
## 24008: PM2.5-10 (2010) 
## 24014: Close to major road

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

# -------------------------------------------------------------------------

group1_med = F
met_no_bio = T

if (group1_med){
  metab_cluster=readRDS(paste(path, "Results_report/cluster_manual/denoised_group1_med_base.rds", sep=""))
  head(metab_cluster)
  colnames(metab_cluster)
  combine = readRDS(paste(path, "Results_report/cluster_manual/combined_group1_med_base.rds", sep =""))
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj1/cluster_manual/group1_med/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Univariate_obj1/cluster_manual/group1_med/"
}

# if(met_base){
#   biobank=readRDS(paste(path, "Results_report/updates/matched_biobank.rds", sep=""))
#   biobank = biobank %>% sample_frac(.05)
#   metab = readRDS(paste(path, "Results_report/matched_metab_base.rds", sep=""))
#   metab = filter(metab, rownames(metab) %in% biobank$eid)
#   metab_cluster = readRDS(paste(path, "Results_report/matched_clus_metab_base.rds", sep=""))
#   metab_cluster = filter(metab_cluster, rownames(metab_cluster) %in% biobank$eid)
#   path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj1/Air_met_base/"
#   path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Univariate_obj1/Air_met_base/"
#   air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
#   colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")
# }

if(met_no_bio){
  metab_clus = readRDS(paste(path, "Results_report/updates/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  biobank=readRDS(paste(path, "Results_report/updates/matched_biobank.rds", sep=""))
  biomarker = readRDS(paste(path, "Results_report/updates/denoised_biomarkers.rds", sep=""))
  combine = readRDS(paste(path, "Results_report/updates/combined_biometab.rds", sep=""))
  combine = combine[order(as.numeric(row.names(combine))),] 
  path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj1/Air_clus_bio/"
  path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Univariate_obj1/Air_clus_bio/"
  air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
  colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")
}


# if(met_first){
#   biobank=readRDS(paste(path, "Results_report/matched_bb_first.rds", sep=""))
#   metab = readRDS(paste(path, "Results_report/matched_metab_first.rds", sep=""))
#   path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj1/Air_met_first/"
#   path_to_save = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Univariate_obj1/Air_met_first/"
#   air = select(biobank, c("24003-0.0", "24005-0.0":"24006-0.0"))
#   colnames(air) <- c("NO2","PM10", "PM2.5")
# }


# -------------TOYYYY-----------------------------------------------------------
# biobank = biobank %>% as.data.frame() %>% sample_frac(.05)
# metab_clus = metab_clus %>% filter(rownames(metab_clus) %in% biobank$eid)
# air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0"))
# colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad")



# -------------------------------------------------------------------------
cluster = F
comb = T
bio = F

## Metabolites cluster ##
if (cluster){
  metab_clus_glm = metab_clus
  metab_clus=scale(metab_clus)
  metab_clus_glm$MajorRoad = air$MajorRoad
  metab_clus_glm$MajorRoad = as.factor(metab_clus_glm$MajorRoad)
  air[,1:6] = scale(air[,1:6])
  air$MajorRoad = as.factor(air$MajorRoad)
  df = metab_clus
  df_glm = metab_clus_glm
}

## Combined dataset ## 
if (comb){
  combine_glm = combine
  combine_glm$MajorRoad = air$MajorRoad
  combine_glm$MajorRoad = as.factor(combine_glm$MajorRoad)
  air[,1:6] = scale(air[,1:6])
  air$MajorRoad = as.factor(air$MajorRoad)
  df = combine
  df_glm = combine_glm
}

## Biomarkers dataset ## 
if (bio){
  bio_glm = biomarker
  biomarker=scale(biomarker)
  bio_glm$MajorRoad = air$MajorRoad
  bio_glm$MajorRoad = as.factor(bio_glm$MajorRoad)
  air[,1:6] = scale(air[,1:6])
  air$MajorRoad = as.factor(air$MajorRoad)
  df = biomarker
  df_glm = bio_glm
}

# Exploring pairwise correlations metabolites clusters v.s air --------

df = as.data.frame(scale(df))
print(nrow(air)==nrow(df))

Beta = Pvalues = matrix(NA, nrow = ncol(air),ncol = ncol(df))
print(ncol(air) * ncol(df)) 
# cluster : 350
# combine : 546
# biomarker : 196

for (i in 1:6) {
  print(i)
  for (j in 1:ncol(df)) {
    model1 = lm(air[, i] ~ df[, j])
    Beta[i, j] = coefficients(model1)["df[, j]"]
    Pvalues[i, j] = summary(model1)$coefficients["df[, j]",
                                                 "Pr(>|t|)"]
  }
}

rownames(Pvalues) = rownames(Beta) = colnames(air)
colnames(Pvalues) = colnames(Beta) = colnames(df)

# logistic beta
model2=glm(air$MajorRoad~., family = "binomial", data = df)
beta = coefficients(model2)
beta = beta[-1]
Beta[7,] = beta

# logistic pvalues
pval = summary(model2)$coefficients[,4]
pval = pval[-1]
Pvalues[7,] = pval
Pvalues[7,1:39] = pval[1:39]
Pvalues[7,40] = NA
Pvalues[7,41:78] = pval[40:77]

if (cluster){
  saveRDS(Pvalues, paste(path_to_Results_report, "pvalues_cluster_uni.rds", sep=""))
  saveRDS(Beta, paste(path_to_Results_report, "betas_cluster_uni.rds", sep =""))
  
}
if (comb){
  saveRDS(Pvalues, paste(path_to_Results_report, "pvalues_combine_uni.rds", sep=""))
  saveRDS(Beta, paste(path_to_Results_report, "betas_combine_uni.rds", sep =""))
  
}

if (bio){
  saveRDS(Pvalues, paste(path_to_Results_report, "pvalues_bio_uni.rds", sep=""))
  saveRDS(Beta, paste(path_to_Results_report, "betas_bio_uni.rds", sep =""))
  
}


Pvalues = readRDS(paste(path_to_Results_report, "pvalues_bio_uni.rds", sep=""))


bonf_threshold = 0.05/546
Pvalues_update = Pvalues[,-40]
print(sum(p.adjust(as.vector(Pvalues), method = "bonf") < 0.05))
# metabolites : 141
# combine : 161
# biomarker : 36
print(sum(p.adjust(as.vector(Pvalues)), method = "BH") < 0.05))
# metabolites : 0
# combine : 0
# biomarker : 0
print(sum(p.adjust(as.vector(!is.na(Pvalues)), method = "none") < 0.05))
# metabolites : 21
# combine : 162
# biomarker : 31

## Major road logistic ##

model2=glm(combine_glm$MajorRoad~., family = "binomial", data = combine_glm)

pvals=rep(NA, ncol(df))
for (k in 1:ncol(df)){
  X=df[,k]
  model=glm(df_glm$MajorRoad~X, family = "binomial")
  model0=glm(df_glm$MajorRoad~1, family = "binomial")
  pvals[k]=anova(model, model0, test = "Chisq")$`Pr(>Chi)`[2]
}

if(cluster) {
  saveRDS(pvals, paste(path_to_Results_report, "pvalues_clus_uni_glm.rds", sep=""))
}
if(comb) {
  saveRDS(pvals, paste(path_to_Results_report, "pvalues_combine_uni_glm.rds", sep=""))
}
if(bio) {
  saveRDS(pvals, paste(path_to_Results_report, "pvalues_bio_uni_glm.rds", sep=""))
}

saveRDS(coefs, paste(path_to_Results_report, "betas_bio_uni_glm.rds", sep =""))


# Look at which are overlap  ----------------------------------------------
library(igraph)
thr = max(as.vector(Pvalues)[p.adjust(as.vector(Pvalues),
                                      method = "bonf") < 0.05])
pairs = which(Pvalues < thr, arr.ind = TRUE)
edges = data.frame(air = colnames(air)[pairs[,1]], metabolites = colnames(df)[pairs[, 2]])
head(edges)

# Setting network characteristics
network = graph_from_data_frame(d = edges, directed = FALSE)
V(network)$color = ifelse(V(network)$name %in% colnames(air),
                          yes = "tomato", no = "skyblue")
V(network)$frame.color = V(network)$color
V(network)$label.family = "Helvetica"
V(network)$label.color = "black"
V(network)$label.cex = ifelse(V(network)$name %in% rownames(Pvalues),
                              yes = 0.5, no = 0.5)
V(network)$label.font = ifelse(V(network)$name %in%
                                 colnames(air), yes = 2, no = 1)
V(network)$size = 1.3 * (degree(network)) + 5
V(network)$size = ifelse(V(network)$name %in% rownames(Pvalues),
                         yes = 11, no = 5)
V(network)$label = ifelse(V(network)$name %in% rownames(Pvalues),
                          yes = rownames(Pvalues), no = colnames(Pvalues))
E(network)$color = "grey30"
dir.create("Figures", showWarnings = FALSE)
pdf(paste(path_to_figure, "network_univ_bonf.pdf", sep =""), )
set.seed(1)
plot(network, layout = layout_in_circle(network))
dev.off()



# Correlation between significant ones  -----------------------------------
sp_NO2 = clus_NO2_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_NO2)[2]) 
sp_NO2_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_NO2)))
mycor = cor(sp_NO2_df)
pdf(paste(path_to_figure, "corr_0.9_NO2.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, show_rownames = T, show_colnames = F, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), fontsize_row = 5)
dev.off()
sum(clus_NO2_sp>=Argmax(out_clus_NO2)[2]) 

combine_bonf = pval %>% filter(-log10(NO2) >= bonf_threshold)
combine_bonf_df = combine %>% select(which(colnames(combine) %in% rownames(combine_bonf)))
mycor = cor(combine_bonf_df)
pdf(paste(path_to_figure, "supp_uni_heatmap.pdf",sep=""))
pheatmap(mycor, show_rownames = FALSE, show_colnames = FALSE,
         breaks = seq(-1, 1, length.out = 100))
dev.off()



# # Exploring pairwise correlations between individual metabolites  -----------------------------------------
# 
# 
# Beta = Pvalues = matrix(NA, nrow = ncol(air),ncol = ncol(metab))
# print(ncol(air) * ncol(metab)) # 384
# 
# for (i in 1:ncol(air)) {
#   print(i)
#   for (j in 1:ncol(metab)) {
#     model1 = lm(air[, i] ~ metab[, j])
#     Beta[i, j] = coefficients(model1)["metab[, j]"]
#     Pvalues[i, j] = summary(model1)$coefficients["metab[, j]",
#                                                  "Pr(>|t|)"]
#   }
# }
# 
# rownames(Pvalues) = rownames(Beta) = colnames(air)
# colnames(Pvalues) = colnames(Beta) = colnames(metab)
# 
# saveRDS(Pvalues, paste(path_to_Results_report, "pvalues_uni.rds", sep=""))
# saveRDS(Beta, paste(path_to_Results_report, "betas_uni.rds", sep =""))
# 
# # Major road logistic ##
# coefs = rep(0, ncol(metab))
# pval <- rep(0, ncol(metab))
# for (j in 1:ncol(metab)) {
#   model2 <- glm(metab_glm$MajorRoad ~ metab[,j], family = "binomial")
#   coefs <- summary(model2)$coefficients
#   pval[j] <- coefs[2, "Pr(>|z|)"]
# }
# 
# pvals=rep(NA, ncol(metab))
# for (k in 1:ncol(metab)){
#   X=metab[,k]
#   model=glm(metab_glm$MajorRoad~X, family = "binomial")
#   model0=glm(metab_glm$MajorRoad~1, family = "binomial")
#   pvals[k]=anova(model, model0, test = "Chisq")$`Pr(>Chi)`[2]
# }
# 
# saveRDS(pvals, paste(path_to_Results_report, "pvalues_uni_glm.rds", sep=""))
# saveRDS(coefs, paste(path_to_Results_report, "betas_uni_glm.rds", sep =""))
# 
# 
# 
# print(sum(p.adjust(as.vector(Pvalues), method = "bonf") < 0.05))
# # all : 0
# # base : 0
# # first : 0
# print(sum(p.adjust(as.vector(Pvalues), method = "BH") < 0.05))
# # all : 0
# # base : 0
# # first : 0
# print(sum(p.adjust(as.vector(Pvalues), method = "none") < 0.05))
# # all : 21
# # base : 162
# # first : 31
# 
# 
# 

# # Exploring uni_Results_report ---------------------------------------------------
# 
# path_to_Results_report = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj1/Air_met_base/"
# pval= readRDS(paste(path_to_Results_report, "pvalues_uni.rds",sep=""))
# pval_clus = readRDS(paste(path_to_Results_report, "pvalues_cluster_uni.rds",sep=""))
# pval_glm = readRDS(paste(path_to_Results_report, "pvalues_uni_glm.rds",sep=""))
# pval_clus_glm = readRDS(paste(path_to_Results_report, "pvalues_cluster_uni_glm.rds",sep=""))
# 
# print(min(as.vector(Pvalues)))# 0.000127649
# 0.05/1008 # 4.960317e-05
# print(min(as.vector(pval_clus))) # 0.0003239692
# 0.05/384 # 0.0001302083
# print(min(as.vector(pval_glm)))# 0.0430449
# 0.05/168 # 0.000297619
# print(min(as.vector(pval_clus_glm)))# 0.0430449
# 0.05/64
# 
# plot(-log10(pval_clus_glm), pch = 16, ylab = "-log(p-values)",
#      xlab = "Metabolites") # Manhattan plot
# abline(h = -log10(0.05/64), col = "red", lty = 2) # p-val = 0.05/p



# ---------Permutation Univariate----------------------------------------------------------------
# 
# #### GLM major road ####
# ### Individual metabolites ### 
# pvals=rep(NA, ncol(metab))
# for (k in 1:ncol(metab)){
#   X=metab[,k]
#   model=glm(metab_glm$MajorRoad~X, family = "binomial")
#   model0=glm(metab_glm$MajorRoad~1, family = "binomial")
#   pvals[k]=anova(model, model0, test = "Chisq")$`Pr(>Chi)`[2]
# }
# 
# saveRDS(pvals, paste(path_to_Results_report, "pvalues_uni_glm.rds", sep=""))
# 
# niter=500
# pvals_perm=matrix(NA, nrow=niter, ncol=ncol(metab))
# for (i in 1:niter){
#   Y_perm=sample(metab_glm$MajorRoad) # Permutation of the outcome
#   for (k in 1:ncol(metab)){
#     X=metab[,k]
#     model=glm(Y_perm~X, family = "binomial")
#     model0=glm(Y_perm~1, family = "binomial")
#     pvals_perm[i,k]=anova(model, model0, test = "Chisq")$`Pr(>Chi)`[2]
#   }
# }
# saveRDS(pvals_perm, paste(path_to_Results_report, "pvalues_uni_glm_perm.rds", sep=""))
# 
# pvals_perm = readRDS(paste(path, "Results_report/Univariate_obj1/Air_met_base/pvalues_uni_glm_perm.rds", sep =""))
# dim(pvals_perm)
# 
# ## Extracting the min(p_val)
# pval_thr=apply(pvals_perm, 1, min)
# 
# ## Plotting distribution of the p-values 
# pval_ranked=sort(pval_thr)
# plot(-log10(pval_ranked), pch=19, las=1, xaxt="n", col="navy",
#      xlab="Rank", ylab=expression(-log[10](p-value)))
# axis(side = 1, at = 1:500, las=2)
# abline(h=-log10(0.05/ncol(metab)), col="red", lty=2)
# abline(v=, col="red", lty=2)
# # abline(h=-log10(0.05/ENT), col="orange", lty=2)
# dev.off()
# 
# # The p-value threshold ensuring that there will be less than 5% of False Positives among the 28 univariate regressions can be computed as 5th percentile of the smallest p-values:
# pval_thr=quantile(apply(pvals_perm, 1, min), 0.05)
# print(paste0("P-value threshold: ", pval_thr, sep =""))
# 
# # ENT 
# ENT=0.05/pval_thr
# print(paste0("ENT:", ENT, sep =""))
# 
# # Plotting graph 
# pdf(paste(path_to_save,"metab_road_perm.pdf",sep =""), width = 5.5, height = 5.5)
# plot(-log10(pvals), pch=19, las=1, xaxt="n", col="navy",
#      xlab="", ylab=expression(-log[10](p-value)))
# axis(side = 1, at = 1:ncol(metab), colnames(metab), las=2)
# abline(h=-log10(0.05/ncol(metab)), col="red", lty=2)
# abline(h=-log10(0.05/ENT), col="orange", lty=2)
# dev.off()
# 
# ### Clustered metabolites ###
# pvals=rep(NA, ncol(metab_cluster))
# for (k in 1:ncol(metab_cluster)){
#   X=metab_cluster[,k]
#   model=glm(metab_clus_glm$MajorRoad~X, family = "binomial")
#   model0=glm(metab_clus_glm$MajorRoad~1, family = "binomial")
#   pvals[k]=anova(model, model0, test = "Chisq")$`Pr(>Chi)`[2]
# }
# 
# saveRDS(pvals, paste(path_to_Results_report, "pvalues_cluster_uni_glm.rds", sep=""))
# 
# # Number of tests #
# n_test = ncol(metab_cluster) *1
# 
# # Permutation #
# niter=500
# pvals_perm=matrix(NA, nrow=niter, ncol=ncol(metab_cluster))
# for (i in 1:niter){
#   Y_perm=sample(metab_clus_glm$MajorRoad) # Permutation of the outcome
#   for (k in 1:ncol(metab_cluster)){
#     X=metab_cluster[,k]
#     model=glm(Y_perm~X, family = "binomial")
#     model0=glm(Y_perm~1, family = "binomial")
#     pvals_perm[i,k]=anova(model, model0, test = "Chisq")$`Pr(>Chi)`[2]
#   }
# }
# colnames(pvals_perm) = colnames(metab_cluster)
# 
# ## Record the minimum p-value for each permutation 
# pvals_min = apply(pvals_perm, 1, min)
# pvals_min = sort(pvals_min)
# 
# 
# ## The p-value threshold ensuring that there will be less than 5% of False Positives among the 28 univariate regressions can be computed as 5th percentile of the smallest p-values:
# pval_thr=quantile(apply(pvals_perm, 1, min), 0.05) 
# print(paste0("P-value threshold: ", pval_thr, sep ="")) ## MWSL (metabolome-wide significance level)
# 
# ## Bonferroni ENT 
# ENT=0.05/pval_thr #The ENT estimate measures the extent that the M markers are non-redundant. 
# print(paste0("ENT:", ENT, sep =""))
# ratio_ENT = ENT/n_test * 100
# print(paste0(ENT,"% of the effective and the actual number of tests " ,sep =""))
# 
# ## Plotting 
# 
# pdf(paste(path_to_save,"clus_road_perm.pdf",sep =""), width = 5.5, height = 5.5)
# plot(-log10(pvals), pch=19, las=1, xaxt="n", col="navy",
#      xlab="", ylab=expression(-log[10](p-value)))
# axis(side = 1, at = 1:ncol(pvals_perm), colnames(pvals_perm), las=2)
# abline(h=-log10(0.05/ncol(pvals_perm)), col="red", lty=2)
# abline(h=-log10(pval_thr), col="orange", lty=2)
# dev.off()
# 
# ## linear regression ##
# Beta = Pvalues = matrix(NA, nrow = ncol(air),ncol = ncol(metab))
# print(ncol(air) * ncol(metab)) # 384
# 
# for (i in 1:ncol(air)) {
#   print(i)
#   for (j in 1:ncol(metab)) {
#     model1 = lm(air[, i] ~ metab[, j])
#     Beta[i, j] = coefficients(model1)["metab[, j]"]
#     Pvalues[i, j] = summary(model1)$coefficients["metab[, j]",
#                                                  "Pr(>|t|)"]
#   }
# }
# 
# rownames(Pvalues) = rownames(Beta) = colnames(air)
# colnames(Pvalues) = colnames(Beta) = colnames(metab)