### Summmer Project -- Objective3 : Associations between predicted score of NO2 in relate to lasso-selected biomarkers and CVD
### 15th July  2021 - Alicia 

# Load packages
library(tidyverse)
library(dplyr)
library(ROSE)
library(pROC)
library(ROCR)
library(caret)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
path_to_Results_report <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj4/"

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


# Score of NO2 v.s. cvd ---------------------------------------------------------------------
## Load data set
score = readRDS(paste(path_to_Results_report, "linear_cvd_no2_selected_score.rds",sep=""))
score = score %>% as.data.frame()
colnames(score)[1] = "value"
cvd = readRDS("../Results_report/updates/matched_cvd.rds")


X=score
Y=as.factor(cvd$cvd_status)
print(all(rownames(X)==rownames(Y)))

set.seed(100)
train=createDataPartition(Y,p=0.7,list=F)
y_train=Y[train]
x_train=X[train,] %>% as.data.frame()
y_test=Y[-train]
x_test=X[-train, ]%>% as.data.frame()
print(all(rownames(y_test)==rownames(x_test)))




log_reg_pred = glm(y_train ~., data = x_train, family="binomial")
predictTest = predict(log_reg_pred, type="response", newdata=x_test[1])
pred <- prediction(predictTest, y_test)
saveRDS(pred,paste(path_to_Results_report, "pred_log_score.rds"))

cm_log= table(y_test, predictTest > 0.5)
print(cm_log)
err_metric(cm_log)

## ROC curve - Logistic ##
roc_obj_log=roc(y_test, predictTest)
saveRDS(roc_obj_log, paste(path_to_Results_report, "roc_log_score.rds", sep =""))

auc_log <- round(auc(y_test, predictTest),3) # AUC = 0.66

ggroc(roc_obj_log, colour = 'steelblue', size = 1.5) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc_log, ')'))

test_df = cbind(x_test, y_test)

print((eval_results(y_test,predictTest, test_df)))



# Score of PM2.5 v.s. cvd ---------------------------------------------------------------------
## Load data set
score = readRDS(paste(path_to_Results_report, "linear_cvd_pm2.5_selected_score.rds",sep=""))
score = score %>% as.data.frame()
colnames(score)[1] = "value"
cvd = readRDS("../Results_report/updates/matched_cvd.rds")


X=score
Y=as.factor(cvd$cvd_status)
print(all(rownames(X)==rownames(Y)))

set.seed(100)
train=createDataPartition(Y,p=0.7,list=F)
y_train=Y[train]
x_train=X[train,] %>% as.data.frame()
y_test=Y[-train]
x_test=X[-train, ]%>% as.data.frame()
print(all(rownames(y_test)==rownames(x_test)))

log_reg_pred = glm(y_train ~., data = x_train, family="binomial")
predictTest = predict(log_reg_pred, type="response", newdata=x_test[1])
pred <- prediction(predictTest, y_test)
saveRDS(pred,paste(path_to_Results_report, "pred_log_score_pm2.5.rds"))

cm_log= table(y_test, predictTest > 0.5)
print(cm_log)
err_metric(cm_log)

## ROC curve - Logistic ##
roc_obj_log=roc(y_test, predictTest)
saveRDS(roc_obj_log, paste(path_to_Results_report, "roc_log_score_pm2.5.rds", sep =""))

auc_log <- round(auc(y_test, predictTest),3) # AUC = 0.66

ggroc(roc_obj_log, colour = 'steelblue', size = 1.5) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc_log, ')'))

test_df = cbind(x_test, y_test)

print((eval_results(y_test,predictTest, test_df)))


