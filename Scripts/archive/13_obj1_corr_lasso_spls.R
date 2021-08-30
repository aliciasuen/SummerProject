### Summmer Project -- Objective 1 : Correlation heatmap to see the relationship between LASSO selected and sPLS selected metabolites 
### 17th July  - Alicia 


# Load packages
library(tidyverse)
library(colorspace)
library(plotrix)
library(maptools)
library(ggplot2)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

## -----------------------------------------------------------------------------------------------------------------------
met_no_bio = T
met_base = F
met_first= F

## -----------------------------------------------------------------------------------------------------------------------
if(met_no_bio){
  biomarker = readRDS(paste(path, "Results/denoised_biomarkers.rds", sep=""))
  biomarker = biomarker[order(as.numeric(row.names(biomarker))), ] 
  metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
  metab_clus = metab_clus[order(as.numeric(row.names(metab_clus))), ] 
  path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Multivariate_obj1/Air_clus_bio/correlation/"
  path_to_spls_results= "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_clus_bio/sPLS/"
  path_to_lasso_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/Multivariate_obj1/Air_clus_bio/lasso/"
  ## sel prop ##
  bio_NO2_sp_spls = readRDS(paste(path_to_spls_results, "selprop_bio_NO2.rds",sep=""))
  bio_PM10_sp_spls = readRDS(paste(path_to_spls_results, "selprop_bio_PM10.rds",sep=""))
  bio_PM2.5_sp_spls = readRDS(paste(path_to_spls_results, "selprop_bio_PM2.5.rds",sep=""))
  bio_NOx_sp_spls = readRDS(paste(path_to_spls_results, "selprop_bio_NOx.rds",sep=""))
  bio_PM2.5_10_sp_spls = readRDS(paste(path_to_spls_results, "selprop_bio_PM2.5_10.rds",sep=""))
  bio_abs_sp_spls = readRDS(paste(path_to_spls_results, "selprop_bio_PM_abs.rds",sep=""))
  bio_road_sp_spls = readRDS(paste(path_to_spls_results, "selprop_bio_road.rds",sep=""))
  clus_NO2_sp_spls = readRDS(paste(path_to_spls_results, "selprop_metab_clus_NO2.rds",sep=""))
  clus_PM10_sp_spls = readRDS(paste(path_to_spls_results, "selprop_metab_clus_PM10.rds",sep=""))
  clus_PM2.5_sp_spls = readRDS(paste(path_to_spls_results, "selprop_metab_clus_PM2.5.rds",sep=""))
  clus_NOx_sp_spls = readRDS(paste(path_to_spls_results, "selprop_metab_clus_NOx.rds",sep=""))
  clus_PM2.5_10_sp_spls = readRDS(paste(path_to_spls_results, "selprop_metab_clus_PM2.5_10.rds",sep=""))
  clus_abs_sp_spls = readRDS(paste(path_to_spls_results, "selprop_metab_clus_PM_abs.rds",sep=""))
  clus_road_sp_spls = readRDS(paste(path_to_spls_results, "selprop_metab_clus_road.rds",sep=""))
  combine_NO2_sp_spls = readRDS(paste(path_to_spls_results, "selprop_combine_NO2.rds",sep=""))
  combine_PM10_sp_spls = readRDS(paste(path_to_spls_results, "selprop_combine_PM10.rds",sep=""))
  combine_PM2.5_sp_spls = readRDS(paste(path_to_spls_results, "selprop_combine_PM2.5.rds",sep=""))
  combine_NOx_sp_spls = readRDS(paste(path_to_spls_results, "selprop_combine_NOx.rds",sep=""))
  combine_PM2.5_10_sp_spls = readRDS(paste(path_to_spls_results, "selprop_combine_PM2.5_10.rds",sep=""))
  combine_abs_sp_spls = readRDS(paste(path_to_spls_results, "selprop_combine_PM_abs.rds",sep=""))
  combine_road_sp_spls = readRDS(paste(path_to_spls_results, "selprop_combine_road.rds",sep=""))
  ## out ## 
  out_spls_bio_NO2 = readRDS(paste(path_to_spls_results, "out_bio_NO2.rds",sep=""))
  out_spls_bio_PM10 = readRDS(paste(path_to_spls_results, "out_bio_PM10.rds",sep=""))
  out_spls_bio_PM2.5 = readRDS(paste(path_to_spls_results, "out_bio_PM2.5.rds",sep=""))
  out_spls_bio_NOx = readRDS(paste(path_to_spls_results, "out_bio_NOx.rds",sep=""))
  out_spls_bio_PM2.5_10 = readRDS(paste(path_to_spls_results, "out_bio_PM2.5_10.rds",sep=""))
  out_spls_bio_abs = readRDS(paste(path_to_spls_results, "out_bio_PM_abs.rds",sep=""))
  out_spls_bio_road = readRDS(paste(path_to_spls_results, "out_bio_road.rds",sep=""))
  out_spls_clus_NO2 = readRDS(paste(path_to_spls_results, "out_metab_clus_NO2.rds",sep=""))
  out_spls_clus_PM10 = readRDS(paste(path_to_spls_results, "out_metab_clus_PM10.rds",sep=""))
  out_spls_clus_PM2.5 = readRDS(paste(path_to_spls_results, "out_metab_clus_PM2.5.rds",sep=""))
  out_spls_clus_NOx = readRDS(paste(path_to_spls_results, "out_metab_clus_NOx.rds",sep=""))
  out_spls_clus_PM2.5_10 = readRDS(paste(path_to_spls_results, "out_metab_clus_PM2.5_10.rds",sep=""))
  out_spls_clus_abs = readRDS(paste(path_to_spls_results, "out_metab_clus_PM_abs.rds",sep=""))
  out_spls_clus_road = readRDS(paste(path_to_spls_results, "out_metab_clus_road.rds",sep=""))
  out_spls_combine_NO2 = readRDS(paste(path_to_spls_results, "out_combine_NO2.rds",sep=""))
  out_spls_combine_PM10 = readRDS(paste(path_to_spls_results, "out_combine_PM10.rds",sep=""))
  out_spls_combine__PM2.5 = readRDS(paste(path_to_spls_results, "out_combine_PM2.5.rds",sep=""))
  out_spls_combine_NOx = readRDS(paste(path_to_spls_results, "out_combine_NOx.rds",sep=""))
  out_spls_combine_PM2.5_10 = readRDS(paste(path_to_spls_results, "out_combine_PM2.5_10.rds",sep=""))
  out_spls_combine_abs = readRDS(paste(path_to_spls_results, "out_combine_PM_abs.rds",sep=""))
  out_spls_combine_road = readRDS(paste(path_to_spls_results, "out_combine_road.rds",sep=""))
    ## sel prop ##
    bio_NO2_sp = readRDS(paste(path_to_lasso_results, "selprop_bio_NO2.rds",sep=""))
    bio_PM10_sp = readRDS(paste(path_to_lasso_results, "selprop_bio_PM10.rds",sep=""))
    bio_PM2.5_sp = readRDS(paste(path_to_lasso_results, "selprop_bio_PM2.5.rds",sep=""))
    bio_NOx_sp = readRDS(paste(path_to_lasso_results, "selprop_bio_NOx.rds",sep=""))
    bio_PM2.5_10_sp = readRDS(paste(path_to_lasso_results, "selprop_bio_PM2.5_10.rds",sep=""))
    bio_abs_sp = readRDS(paste(path_to_lasso_results, "selprop_bio_PM_abs.rds",sep=""))
    bio_road_sp = readRDS(paste(path_to_lasso_results, "selprop_bio_road.rds",sep=""))
    clus_NO2_sp = readRDS(paste(path_to_lasso_results, "selprop_metab_clus_NO2.rds",sep=""))
    clus_PM10_sp = readRDS(paste(path_to_lasso_results, "selprop_metab_clus_PM10.rds",sep=""))
    clus_PM2.5_sp = readRDS(paste(path_to_lasso_results, "selprop_metab_clus_PM2.5.rds",sep=""))
    clus_NOx_sp = readRDS(paste(path_to_lasso_results, "selprop_metab_clus_NOx.rds",sep=""))
    clus_PM2.5_10_sp = readRDS(paste(path_to_lasso_results, "selprop_metab_clus_PM2.5_10.rds",sep=""))
    clus_abs_sp = readRDS(paste(path_to_lasso_results, "selprop_metab_clus_PM_abs.rds",sep=""))
    clus_road_sp = readRDS(paste(path_to_lasso_results, "selprop_metab_clus_road.rds",sep=""))
    combine_NO2_sp = readRDS(paste(path_to_lasso_results, "selprop_combine_NO2.rds",sep=""))
    combine_PM10_sp = readRDS(paste(path_to_lasso_results, "selprop_combine_PM10.rds",sep=""))
    combine_PM2.5_sp = readRDS(paste(path_to_lasso_results, "selprop_combine_PM2.5.rds",sep=""))
    combine_NOx_sp = readRDS(paste(path_to_lasso_results, "selprop_combine_NOx.rds",sep=""))
    combine_PM2.5_10_sp = readRDS(paste(path_to_lasso_results, "selprop_combine_PM2.5_10.rds",sep=""))
    combine_abs_sp = readRDS(paste(path_to_lasso_results, "selprop_combine_PM_abs.rds",sep=""))
    combine_road_sp = readRDS(paste(path_to_lasso_results, "selprop_combine_road.rds",sep=""))
    ## out ## 
    out_bio_NO2 = readRDS(paste(path_to_lasso_results, "out_bio_NO2.rds",sep=""))
    out_bio_PM10 = readRDS(paste(path_to_lasso_results, "out_bio_PM10.rds",sep=""))
    out_bio_PM2.5 = readRDS(paste(path_to_lasso_results, "out_bio_PM2.5.rds",sep=""))
    out_bio_NOx = readRDS(paste(path_to_lasso_results, "out_bio_NOx.rds",sep=""))
    out_bio_PM2.5_10 = readRDS(paste(path_to_lasso_results, "out_bio_PM2.5_10.rds",sep=""))
    out_bio_abs = readRDS(paste(path_to_lasso_results, "out_bio_PM_abs.rds",sep=""))
    out_bio_road = readRDS(paste(path_to_lasso_results, "out_bio_road.rds",sep=""))
    out_clus_NO2 = readRDS(paste(path_to_lasso_results, "out_metab_clus_NO2.rds",sep=""))
    out_clus_PM10 = readRDS(paste(path_to_lasso_results, "out_metab_clus_PM10.rds",sep=""))
    out_clus_PM2.5 = readRDS(paste(path_to_lasso_results, "out_metab_clus_PM2.5.rds",sep=""))
    out_clus_NOx = readRDS(paste(path_to_lasso_results, "out_metab_clus_NOx.rds",sep=""))
    out_clus_PM2.5_10 = readRDS(paste(path_to_lasso_results, "out_metab_clus_PM2.5_10.rds",sep=""))
    out_clus_abs = readRDS(paste(path_to_lasso_results, "out_metab_clus_PM_abs.rds",sep=""))
    out_clus_road = readRDS(paste(path_to_lasso_results, "out_metab_clus_road.rds",sep=""))
    out_combine_NO2 = readRDS(paste(path_to_lasso_results, "out_combine_NO2.rds",sep=""))
    out_combine_PM10 = readRDS(paste(path_to_lasso_results, "out_combine_PM10.rds",sep=""))
    out_combine__PM2.5 = readRDS(paste(path_to_lasso_results, "out_combine_PM2.5.rds",sep=""))
    out_combine_NOx = readRDS(paste(path_to_lasso_results, "out_combine_NOx.rds",sep=""))
    out_combine_PM2.5_10 = readRDS(paste(path_to_lasso_results, "out_combine_PM2.5_10.rds",sep=""))
    out_combine_abs = readRDS(paste(path_to_lasso_results, "out_combine_PM_abs.rds",sep=""))
    out_combine_road = readRDS(paste(path_to_lasso_results, "out_combine_road.rds",sep=""))
  }


# --------------NO2 clusters correlation -----------------------------------------------------------

sp_NO2_spls = clus_NO2_sp_spls %>% data.frame() %>% filter (. >= Argmax(out_spls_clus_NO2)[2]) 
sp_NO2_df_spls = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_NO2_spls)))

sp_NO2 = clus_NO2_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_NO2)[2]) 
sp_NO2_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_NO2)))

mycor = cor(sp_NO2_df_spls,sp_NO2_df)
pdf(paste(path_to_figure, "corr_clus_NO2.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, 
         show_rownames = T, show_colnames = T, 
         cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), 
         fontsize_row = 5,fontsize_col = 5,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)
dev.off()
# --------------PM10 clusters correlation -----------------------------------------------------------

sp_PM10_spls = clus_PM10_sp_spls %>% data.frame() %>% filter (. >= Argmax(out_spls_clus_PM10)[2]) 
sp_PM10_df_spls = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_PM10_spls)))

sp_PM10 = clus_PM10_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_PM10)[2]) 
sp_PM10_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_PM10)))

mycor = cor(sp_PM10_df_spls,sp_PM10_df)
pdf(paste(path_to_figure, "corr_clus_PM10.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, 
         show_rownames = T, show_colnames = T, 
         cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), 
         fontsize_row = 5,fontsize_col = 5,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)
dev.off()

# --------------PM2.5 clusters correlation -----------------------------------------------------------

sp_PM2.5_spls = clus_PM2.5_sp_spls %>% data.frame() %>% filter (. >= Argmax(out_spls_clus_PM2.5)[2]) 
sp_PM2.5_df_spls = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_PM2.5_spls)))

sp_PM2.5 = clus_PM2.5_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_PM2.5)[2]) 
sp_PM2.5_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_PM2.5)))

mycor = cor(sp_PM2.5_df_spls,sp_PM2.5_df)
pdf(paste(path_to_figure, "corr_clus_PM2.5.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, 
         show_rownames = T, show_colnames = T, 
         cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), 
         fontsize_row = 5,fontsize_col = 5,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)
dev.off()

# --------------Major road clusters correlation -----------------------------------------------------------

sp_road_spls = clus_road_sp_spls %>% data.frame() %>% filter (. >= Argmax(out_spls_clus_road)[2]) 
sp_road_df_spls = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_road_spls)))

sp_road = clus_road_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_road)[2]) 
sp_road_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_road)))

mycor = cor(sp_road_df_spls,sp_road_df)
pdf(paste(path_to_figure, "corr_clus_road.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, 
         show_rownames = T, show_colnames = T, 
         cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), 
         fontsize_row = 5,fontsize_col = 5,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)
dev.off()

# --------------NO2 combined correlation -----------------------------------------------------------

sp_NO2_spls = combine_NO2_sp_spls %>% data.frame() %>% filter (. >= Argmax(out_spls_bio_NO2)[2]) 
sp_NO2_df_spls = biomarker %>% select(which(colnames(biomarker) %in% rownames(sp_NO2_spls)))

sp_NO2 = clus_NO2_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_NO2)[2]) 
sp_NO2_df = metab_clus %>% select(which(colnames(biomarker) %in% rownames(sp_NO2)))

mycor = cor(sp_NO2_df_spls,sp_NO2_df)
pdf(paste(path_to_figure, "corr_clus_NO2.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, 
         show_rownames = T, show_colnames = T, 
         cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), 
         fontsize_row = 5,fontsize_col = 5,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)
dev.off()
# --------------PM10 clusters correlation -----------------------------------------------------------

sp_PM10_spls = clus_PM10_sp_spls %>% data.frame() %>% filter (. >= Argmax(out_spls_clus_PM10)[2]) 
sp_PM10_df_spls = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_PM10_spls)))

sp_PM10 = clus_PM10_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_PM10)[2]) 
sp_PM10_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_PM10)))

mycor = cor(sp_PM10_df_spls,sp_PM10_df)
pdf(paste(path_to_figure, "corr_clus_PM10.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, 
         show_rownames = T, show_colnames = T, 
         cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), 
         fontsize_row = 5,fontsize_col = 5,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)
dev.off()

# --------------PM2.5 clusters correlation -----------------------------------------------------------

sp_PM2.5_spls = clus_PM2.5_sp_spls %>% data.frame() %>% filter (. >= Argmax(out_spls_clus_PM2.5)[2]) 
sp_PM2.5_df_spls = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_PM2.5_spls)))

sp_PM2.5 = clus_PM2.5_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_PM2.5)[2]) 
sp_PM2.5_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_PM2.5)))

mycor = cor(sp_PM2.5_df_spls,sp_PM2.5_df)
pdf(paste(path_to_figure, "corr_clus_PM2.5.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, 
         show_rownames = T, show_colnames = T, 
         cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), 
         fontsize_row = 5,fontsize_col = 5,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)
dev.off()

# --------------Major road clusters correlation -----------------------------------------------------------

sp_road_spls = clus_road_sp_spls %>% data.frame() %>% filter (. >= Argmax(out_spls_clus_road)[2]) 
sp_road_df_spls = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_road_spls)))

sp_road = clus_road_sp %>% data.frame() %>% filter (. >= Argmax(out_clus_road)[2]) 
sp_road_df = metab_clus %>% select(which(colnames(metab_clus) %in% rownames(sp_road)))

mycor = cor(sp_road_df_spls,sp_road_df)
pdf(paste(path_to_figure, "corr_clus_road.pdf", sep =""), width = 8, height = 5)
pheatmap(mycor, 
         show_rownames = T, show_colnames = T, 
         cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100), 
         fontsize_row = 5,fontsize_col = 5,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)
dev.off()



