### Summmer Project -- Objective3 : Associations between air exposure and CVD
### 15th July  2021 - Alicia 

# Load packages
library(tidyverse)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
path_to_Results_report <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results_report/Univariate_obj3/"

## Load data set
biobank= readRDS("../Results_report/updates/matched_biobank.rds")
cvd = readRDS("../Results_report/updates/matched_cvd.rds")
air = select(biobank, c("24003-0.0":"24008-0.0", "24014-0.0","21022-0.0","31-0.0", "6138-0.0", "20116-0.0","1558-0.0","21001-0.0"))
air$`31-0.0` = as.factor(air$`31-0.0`)
colnames(air) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10", "MajorRoad", 
                   "age","sex","education","smoking","alcohol","bmi")
confounder = c("age","sex","education","smoking","alcohol","bmi")

X=air
Y=as.factor(cvd$cvd_status)
print(all(rownames(X)==rownames(Y)))

### Iterate
betas = pval = NULL

for(k in 1:7){
  ## Split
  print(paste0("Feature: ",colnames(X)[k]))
  ## logistic
  glm = glm(Y ~ X[,k]+age+sex+education+smoking+alcohol+bmi, data = X, family = "binomial")
  betas = c(betas, glm$coefficients[2])
  pval=c(pval, summary(glm)$coefficients[2,4])
}
names(pval)=names(betas)=colnames(X)[1:7]

## Save
saveRDS(betas,paste(path_to_Results_report, "obj3_univar_air_betas.rds", sep =""))
saveRDS(pval,paste(path_to_Results_report, "obj3_univar_air_pval.rds", sep =""))

sum(p.adjust(pval, method = "bonf") < 0.05)
# 4 

