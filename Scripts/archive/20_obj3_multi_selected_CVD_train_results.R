### Summer project Project -- CVD outcome training 
## 26th Aug 2021 


# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))


LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}
LoadPackages(c("caret","pheatmap","corpcor","abind","parallel",
               "RColorBrewer","ppcor","mvtnorm",
               "pROC","stabs","huge","pulsar",
               "QUIC","igraph","glasso","glassoFast",
               "colorspace","glmnet","dplyr", "devtools", "focus"))

LoadPackages(c("pheatmap", "RColorBrewer", "colorspace","parallel",
               "mixOmics", "sgPLS", "tidyverse", "pROC"))


# path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
# path_to_data <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/updates/"
# path_to_results <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj3/lasso/training/"
# path_to_figure <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj3/lasso/training/"
# 
# ## Parameters
# args=commandArgs(trailingOnly=TRUE)
# m=as.numeric(args[1])
# 
# ## Load datasets
# combined_all = readRDS(paste(path_to_data, "combined_biometab.rds",sep =""))
# combined_selected = readRDS(paste(path_to_data, "denoised_selected_combined.rds",sep=""))
# cvd = readRDS(paste(path_to_data, "matched_cvd.rds",sep=""))
# 
# 
# X=combined_all
# Y=cvd$cvd_status
# print(all(rownames(X)==rownames(Y)))
# 
# ### Iterate
# niter=50
# starting_seed = seq(1,1000,50)
# selprop.total = NULL
# loadings.total = data.frame(name = colnames(X), check.names = F)
# fpr.total = NULL
# tpr.total = NULL
# auc.list = NULL
# mr_controls.list = NULL
# mr_cases.list = NULL
# 
# for(k in seq(starting_seed, length.out=niter)){
#   ## Split
#   print(paste0("Round: ",k))
#   set.seed(k)
#   s0=sample(which(Y=="0"), size=0.7*sum(Y=="0"), replace=FALSE)
#   s1=sample(which(Y=="1"), size=0.7*sum(Y=="1"), replace=FALSE)
#   train=c(s0,s1)
#   y_train=Y[train]
#   x_train=X[train,]
#   y_test=Y[-train]
#   x_test=X[-train,]
#   
#   t0=Sys.time()
#   ## Calibration
#   out = VariableSelection(xdata = x_train, ydata = y_train, family="binomial",PFER_thr =20 , PFER_method = "MB",
#                             K=100, verbose=FALSE)
#   t1=Sys.time()
#   print(t1-t0)
#   
#   # Save
#   saveRDS(out, paste0(path_to_results,"out/out_",k,".rds"))
#   
#   # Calibrated selection proportions 
#   selprop.total=cbind(selprop.total,SelectionProportions(out, plot=FALSE))
#   
#   # multi-omics
#   selected = SelectedVariables(out)
#   selected = names(selected[selected == 1])
#   mylasso = glmnet(X=x_train[,selected], Y=y_train, alpha = 1, family = "binomial")
#   
#   # Prediction
#   predictions=predict(mylasso, newx=x_test[,selected])
#   
#   # Loadings
#   tmp=as.data.frame(mylasso$beta, check.names = F)
#   tmp$name = rownames(tmp)
#   loadings.total = merge(loadings.total,tmp, by = "name", all = TRUE)
#   
#   # ROC
#   myroc=roc(response=y_test, predictor=predictions)
#   fpr.total=cbind(fpr.total,1-myroc$specificities)
#   tpr.total=cbind(tpr.total,tpr = myroc$sensitivities)
#   
#   # AUC
#   auc.list=c(auc.list,as.numeric(myroc$auc))
#   
#   # Misclassification rates based on max dist
#   predictions_class <- ifelse(predictions > 0.5, "pos", "neg")
#   mr_controls.list=c(mr_controls.list,sum(predictions_class==1)/length(predictions_class))
#   mr_cases.list=c(mr_cases.list,sum(predictions_class==0)/length(predictions_class))
# }
# 
# saveRDS(selprop.total, paste0(path_to_results,"selprop/selprop_",m,".rds"))
# saveRDS(loadings.total, paste0(path_to_results,"loadings/loadings_",m,".rds"))
# saveRDS(fpr.total, paste0(path_to_results,"fpr/fpr_",m,".rds"))
# saveRDS(tpr.total, paste0(path_to_results,"tpr/tpr_",m,".rds"))
# saveRDS(auc.list, paste0(path_to_results,"auc/auc_",m,".rds"))
# saveRDS(mr_controls.list, paste0(path_to_results,"mr_controls/mr_controls_",m,".rds"))
# saveRDS(mr_cases.list, paste0(path_to_results,"mr_cases/mr_cases_",m,".rds"))
# 


# -------------------------------------------------------------------------


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
path_to_data <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/updates/"
path_to_results <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj3/selected_lasso/training/"
path_to_figure <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj3/selected_lasso/training/"

## Load datasets
combined_all = readRDS(paste(path_to_data, "combined_biometab.rds",sep =""))
combined_selected = readRDS(paste(path_to_data, "denoised_selected_combined.rds",sep=""))
cvd = readRDS(paste(path_to_data, "matched_cvd.rds",sep=""))


X=combined_selected
Y=cvd$cvd_status
print(all(rownames(X)==rownames(Y)))

set.seed(100)
train=createDataPartition(Y,p=0.7,list=F)
y_train=Y[train]
x_train=X[train,]
y_test=Y[-train]
x_test=X[-train, ]
print(all(rownames(y_test)==rownames(x_test)))

t0=Sys.time()
# Running stability selection
out = VariableSelection(xdata = x_train, ydata = y_train, family="binomial",PFER_thr =20 , PFER_method = "MB",
                        K=100, verbose=T)
t1=Sys.time()
print(t1-t0)

selprop=SelectionProportions(out)

saveRDS(out, paste(path_to_results, "out_combine_train.rds", sep =""))
saveRDS(selprop, paste(path_to_results, "selprop_combine_train.rds", sep =""))

pdf(paste(path_to_figure,"CalibrationPlot/combine_train.pdf",sep =""))
CalibrationPlot(out)
dev.off()

# Extracting ID of calibrated lambda
hat_lambda_id.1=ArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load.1=apply(out$Beta[hat_lambda_id.1,,],1,FUN=function(x){mean(x[x!=0])})
# Save
saveRDS(average_load.1, paste(path_to_results, "beta_combine_train.rds", sep =""))

selprop_nonzero=selprop[selprop>0]
myorder=names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]

### Using average beta coefficients
# Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
myaucs=NULL
myaucs_table=NULL

t0=Sys.time()
x_test = x_test %>% as.matrix()
for (k in 1:length(myorder)){
  # Using the average beta as a beta, only computing the intercept in the model below (no impact on AUC)
  mymodel=glm(y_test~offset(x_test[,myorder[1:k],drop=FALSE]%*%
                              matrix(average_load.1[myorder[1:k]],ncol=1)),
              family="binomial")
  myroc=roc(response=y_test, predictor=mymodel$fitted.values)
  myaucs=c(myaucs, myroc$auc)
  myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f",
                                           digits=4))
}
t1=Sys.time()
print(t1-t0)

rownames(myaucs_table)=myorder
colnames(myaucs_table)=c("li","auc","ui")
saveRDS(myaucs_table, paste(path_to_results, "auc_average_beta.rds", sep =""))

### Recalibration
# Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
myaucs=NULL
myaucs_table=NULL

calib=sum(SelectedVariables(out)) # Number of selected variables

t0=Sys.time()
for (k in 1:length(myorder)){
  # Recalibration of the beta coefficients on the training set
  mymodel_recalib=glm(y_train~as.matrix(x_train[,myorder[1:k],drop=FALSE]), family="binomial")
  # Prediction using a logistic model with recalibrated beta coefficients
  mymodel=glm(y_test~offset(x_test[,myorder[1:k],drop=FALSE] %*% matrix(coef(mymodel_recalib)[-1], ncol=1)), family="binomial")
  myroc=roc(response=y_test, predictor=mymodel$fitted.values)
  if(k==calib){
    saveRDS(myroc, paste(path_to_results, "roc_recalib_beta.rds", sep =""))
  }
  myaucs=c(myaucs, myroc$auc)
  myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f", digits=4))
}
t1=Sys.time()
print(t1-t0)
rownames(myaucs_table)=myorder
colnames(myaucs_table)=c("li","auc","ui")
saveRDS(myaucs_table, paste(path_to_results, "auc_recalib_beta.rds", sep =""))

# Misclassification rates
mr_controls=mr_cases=NULL
thr_list=seq(0.1,0.9,by=0.1)
for (thr in thr_list){
  predicted_cat=ifelse(mymodel$fitted.values[y_test==0]>=thr, yes=1, no=0)
  mr_controls=c(mr_controls, sum(predicted_cat==1)/length(predicted_cat))
  predicted_cat=ifelse(mymodel$fitted.values[y_test==1]>=thr, yes=1, no=0)
  mr_cases=c(mr_cases, sum(predicted_cat==0)/length(predicted_cat))
}
saveRDS(myaucs_table, paste(path_to_results, "mr_controls.rds", sep =""))
saveRDS(myaucs_table, paste(path_to_results, "mr_cases.rds", sep =""))

