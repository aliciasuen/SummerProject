### Summmer Project -- sparse PCA metabolites 
### 26th July 2021 

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
path_to_results <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/pca/"
LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}
LoadPackages(c("pheatmap","corpcor","abind","parallel",
               "RColorBrewer","ppcor","mvtnorm",
               "pROC","stabs","huge","pulsar",
               "QUIC","igraph","glasso","glassoFast",
               "colorspace","glmnet","dplyr","tidyverse","devtools", "focus"))

metab_no_bio = readRDS(paste(path, "Results/metab_no_bio.rds",sep=""))

stab <- BiSelection(
  xdata = metab_no_bio,
  ncomp = 5,
  LambdaX = 1:(ncol(metab_no_bio) - 1),
  implementation = SparsePCA)

saveRDS(stab, paste(path_to_results, "out_sPCA_metab.rds", sep=""))

