### Summmer Project -- Objective3 : Associations between air exposure and CVD
### 15th July  2021 - Alicia 

LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}
LoadPackages(c("pheatmap","corpcor","abind","parallel",
               "RColorBrewer","ppcor","mvtnorm",
               "pROC","stabs","huge","pulsar",
               "QUIC","igraph","glasso","glassoFast",
               "colorspace","glmnet","dplyr", "devtools", "focus"))
# Load packages
library(tidyverse)
library(dplyr)
library(ROSE)
library(pROC)
library(ROCR)
library(caret)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
path_to_Results_report <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj4/"
path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj4/"

# Error metric function ---------------------------------------------------

err_metric=function(CM)
{
  TN =CM[1,1]
  TP =CM[2,2]
  FP =CM[1,2]
  FN =CM[2,1]
  precision =(TP)/(TP+FP)
  recall_score =(FP)/(FP+TN)
  
  f1_score=2*((precision*recall_score)/(precision+recall_score))
  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
  False_positive_rate =(FP)/(FP+TN)
  False_negative_rate =(FN)/(FN+TP)
  
  print(paste("Precision value of the model: ",round(precision,2)))
  print(paste("Accuracy of the model: ",round(accuracy_model,2)))
  print(paste("Recall value of the model: ",round(recall_score,2)))
  print(paste("False Positive rate of the model: ",round(False_positive_rate,2)))
  
  print(paste("False Negative rate of the model: ",round(False_negative_rate,2)))
  
  print(paste("f1 score of the model: ",round(f1_score,2)))
}





# Load data set-------------------------------------------------------------------------
biobank= readRDS("../Results_report/updates/matched_biobank.rds")
cvd = readRDS("../Results_report/updates/matched_cvd.rds")
air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0","21022-0.0","31-0.0", "6138-0.0", "20116-0.0","1558-0.0","21001-0.0"))
air$`31-0.0` = as.factor(air$`31-0.0`)
colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad", 
                   "age","sex","education","smoking","alcohol","bmi")
confounder = c("age","sex","education","smoking","alcohol","bmi")
# Univariate - NO2 ---------------------------------------------------------------------
X=air
Y=as.factor(cvd$cvd_status)
print(all(rownames(X)==rownames(Y)))

set.seed(100)
train=createDataPartition(Y,p=0.7,list=F)
y_train=Y[train]
x_train=data.frame(X[train,]) 
y_test=Y[-train]
x_test=data.frame(X[-train, ])
print(all(rownames(y_train)==rownames(x_train)))


log_reg_pred = glm(y_train ~ NO2 + age+sex+education+smoking+alcohol+bmi ,data = x_train, family="binomial")
# newdata = data.frame(x_test$NO2) 
# colnames(newdata)[1] = "NO2"
predictTest = predict(log_reg_pred, type="response", newdata=x_test)
pred <- prediction(predictTest, y_test)
saveRDS(pred,paste(path_to_Results_report, "pred_log_no2.rds", sep =""))

cm_log= table(y_test, predictTest > 0.5)
print(cm_log)
err_metric(cm_log)

## ROC curve - Logistic ##
roc_obj_log=roc(y_test, predictTest)
saveRDS(roc_obj_log, paste(path_to_Results_report,"roc_log_no2.rds",sep=""))

auc_log <- round(auc(y_test, predictTest),3) # AUC = 0.511

ggroc(roc_obj_log, colour = 'steelblue', size = 1.5) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc_log, ')'))

test_df = cbind(x_test, y_test)

print((eval_results(y_test,predictTest, test_df)))



# Univariate - PM2.5 ---------------------------------------------------------------------
X=air
Y=as.factor(cvd$cvd_status)
print(all(rownames(X)==rownames(Y)))

set.seed(100)
train=createDataPartition(Y,p=0.7,list=F)
y_train=Y[train]
x_train=data.frame(X[train,]) 
y_test=Y[-train]
x_test=data.frame(X[-train, ])
print(all(rownames(y_train)==rownames(x_train)))

log_reg_pred = glm(y_train ~ PM2.5 + age+sex+education+smoking+alcohol+bmi ,data = x_train, family="binomial")
# newdata = data.frame(x_test$PM2.5) 
# colnames(newdata)[1] = "PM2.5"
predictTest = predict(log_reg_pred, type="response", newdata=x_test)
pred <- prediction(predictTest, y_test)
saveRDS(pred,paste(path_to_Results_report, "pred_log_pm2.5.rds", sep =""))

cm_log= table(y_test, predictTest > 0.5)
print(cm_log)
err_metric(cm_log)

## ROC curve - Logistic ##
roc_obj_log=roc(y_test, predictTest)
saveRDS(roc_obj_log, paste(path_to_Results_report,"roc_log_pm2.5.rds",sep=""))

auc_log <- round(auc(y_test, predictTest),3) # AUC = 0.511

ggroc(roc_obj_log, colour = 'steelblue', size = 1.5) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc_log, ')'))

# LASSO - all air pollutants ----------------------------------------------
X=air
Y=as.factor(cvd$cvd_status)
print(all(rownames(X)==rownames(Y)))
X[1:6] = scale(X[1:6])
X$MajorRoad = as.factor(X$MajorRoad)
X$MajorRoad = as.numeric(X$MajorRoad)
X$sex = as.numeric(X$sex)
X$education = as.numeric(X$education)
X$smoking = as.numeric(X$smoking)
X$alcohol = as.numeric(X$alcohol)


penalty <- rep(1,1,ncol(X)) # all 1s
forced <- which(colnames(X) %in% confounder) # find sex, age and bmi index
penalty[forced] <- 0 # force age,sex to be included

set.seed(100)
train=createDataPartition(Y,p=0.7,list=F)
y_train=Y[train]
x_train=data.frame(X[train,]) 
y_test=Y[-train]
x_test=data.frame(X[-train, ])
print(all(rownames(y_train)==rownames(x_train)))
# Running stability selection
x_train[1:6] = scale(x_train[1:6])
x_train$MajorRoad = as.factor(x_train$MajorRoad)
x_train$MajorRoad = as.numeric(x_train$MajorRoad)
x_train$sex = as.numeric(x_train$sex)
x_train$education = as.numeric(x_train$education)
x_train$smoking = as.numeric(x_train$smoking)
x_train$alcohol = as.numeric(x_train$alcohol)

x_test[1:6] = scale(x_test[1:6])
x_test$MajorRoad = as.factor(x_test$MajorRoad)
x_test$MajorRoad = as.numeric(x_test$MajorRoad)
x_test$sex = as.numeric(x_test$sex)
x_test$education = as.numeric(x_test$education)
x_test$smoking = as.numeric(x_test$smoking)
x_test$alcohol = as.numeric(x_test$alcohol)

out=VariableSelection(xdata=x_train, ydata=as.factor(y_train), K=100, tau=0.5,PFER_method = "MB", PFER_thr = 20,
                        penalty.factor = penalty, verbose=T, family="binomial")
saveRDS(out, paste(path_to_Results_report, "out_lasso_train.rds", sep =""))

# Calibrated selection proportions 
selprop=SelectionProportions(out)
print(selprop)

# Calibrated parameters
hat_params=Argmax(out)

## Visualisation of selection proportions by stability selection models 
pdf(paste(path_to_figure, "selprop_air_cvd_train.pdf", sep=""))
par(mar=c(10,5,1,1))
plot(selprop, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(selprop>=hat_params[2], yes="red", no="grey"), cex.lab=1.5)
abline(h=hat_params[2], lty=2, col="darkred")
for (i in 1:length(selprop)){
  axis(side=1, at=i, labels =names(selprop)[i], las=2,cex.axis = 0.7,
       col=ifelse(selprop[i]>=hat_params[2], yes="red", no="grey"),
       col.axis=ifelse(selprop[i]>=hat_params[2], yes="red", no="grey"))
}
dev.off()

out = readRDS(paste(path_to_Results_report, "out_lasso_train.rds", sep =""))
hat_params=Argmax(out)

lasso_model <- glmnet(x_train, as.factor(y_train), alpha = 1, penalty.factor = penalty,lambda = hat_params[1],family ="binomial", standardize = TRUE)
predictions_train <- predict(lasso_model, s = hat_params[1], newx = data.matrix(x_test), type = "response")
cm_log= table(y_test, predictions_train > .5)
print(cm_log)
err_metric(cm_log)
pred <- prediction(predictions_train, y_test)
saveRDS(pred,paste(path_to_Results_report, "pred_lasso_all_air.rds", sep =""))

## ROC curve ##
roc_obj_log=roc(y_test, as.numeric(predictions_train))
saveRDS(roc_obj_log, paste(path_to_Results_report,"roc_lasso_all_air.rds",sep=""))

auc_log <- round(auc(y_test, predictions_train),3) # AUC = 0.511

ggroc(roc_obj_log, colour = 'steelblue', size = 1.5) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc_log, ')'))

test_df = cbind(x_test, y_test)

print((eval_results(y_test,predictTest, test_df)))



