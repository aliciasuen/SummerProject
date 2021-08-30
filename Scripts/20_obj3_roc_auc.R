### Summer project Project -- CVD outcome training  - ROC curve
## 26th Aug 2021 


# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))

rm(list=ls())

library(dplyr)
library(ROSE)
library(pROC)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
path_to_Results_report <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Multivariate_obj4/"
path_to_figure = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figure_report/Multivariate_obj4/"


# Loading data ------------------------------------------------------------


# pred_log_no2<-readRDS(paste(path_to_Results_report, "pred_log_no2.rds",sep =""))
# pred_lasso<- readRDS(paste(path_to_Results_report, "pred_lasso_all_air.rds",sep =""))
pred_log_pm2.5<-readRDS(paste(path_to_Results_report, "pred_log_pm2.5.rds",sep =""))
pred_log_score<-readRDS(paste(path_to_Results_report, " pred_log_score_pm2.5.rds",sep =""))


# perf_auc_lasso = performance(pred_lasso, measure = "auc")
# perf_roc_lasso =  performance(pred_lasso, measure = "tpr", x.measure = "fpr")
# auc_lasso = perf_auc_lasso@y.values[[1]]
# 
# perf_auc_log_no2 = performance(pred_log_no2, measure = "auc")
# perf_roc_log_no2 =  performance(pred_log_no2, measure = "tpr", x.measure = "fpr")
# auc_log_no2 = perf_auc_log_no2@y.values[[1]]

perf_auc_log_pm2.5 = performance(pred_log_pm2.5, measure = "auc")
perf_roc_log_pm2.5 =  performance(pred_log_pm2.5, measure = "tpr", x.measure = "fpr")
auc_log_pm2.5 = perf_auc_log_pm2.5@y.values[[1]]

perf_auc_log_score = performance(pred_log_score , measure = "auc")
perf_roc_log_score  =  performance(pred_log_score, measure = "tpr", x.measure = "fpr")
auc_log_score = perf_auc_log_score @y.values[[1]]

# roc_glm_no2 = data.frame(fpr = perf_roc_log_no2@x.values, tpr = perf_roc_log_no2@y.values)
# roc_lasso = data.frame(fpr = perf_roc_lasso@x.values, tpr = perf_roc_lasso@y.values)
roc_glm_pm2.5 = data.frame(fpr = perf_roc_log_pm2.5@x.values, tpr = perf_roc_log_pm2.5@y.values)
roc_glm_score = data.frame(fpr = perf_roc_log_score@x.values, tpr = perf_roc_log_score@y.values)


mycolours = c("purple", "green")
pdf(paste(path_to_figure, "ROC_all.pdf", sep =""))
par(mar=c(5,5,2,1))
plot(roc_glm_pm2.5, ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC curves")
abline(0,1, lty = 3, col = "grey")
# lines(roc_glm_no2, col = mycolours[1], type="l", lwd = 2)
# lines(roc_lasso, col = mycolours[2], type="l", lwd = 2)
lines(roc_glm_pm2.5, col = mycolours[1], type="l", lwd = 2)
lines(roc_glm_score, col = mycolours[2], type="l", lwd = 2)

legend("bottomright",
       legend = c(paste0("Logistic regression of PM2.5; AUC = ", round(auc_log_pm2.5,3)),
                  paste0("Logistic regression of score ; AUC = ", round(auc_log_score,3))),
       lty = 1, lwd = 2, col = mycolours, cex = 0.65, bg = "white")
dev.off()
