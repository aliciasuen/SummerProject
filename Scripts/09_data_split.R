
### Summmer Project -- Metabolomics Data Imputaion
### 23/08/2021
# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library"))


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
path_to_data <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/updates/"
path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/updates/split/"
bb= readRDS(paste(path_to_data, "matched_biobank.rds", sep=""))
biomarker =readRDS(paste(path_to_data, "matched_biomarker.rds", sep=""))
cvd = readRDS(paste(path_to_data, "matched_cvd.rds", sep=""))



df <- readRDS(paste(path_to_data, "matched_metab_base.rds", sep="")) 
df$eid = rownames(df)
df = df[order(as.numeric(row.names(df))), ]

## 70% of the sample size
smp_size <- floor(0.70 * nrow(df))

## set the seed to make your partition reproducible
seed <- 123
set.seed(seed)

train_ind <- sample(seq_len(nrow(df)), size = smp_size) # sample indices
train <- df[train_ind, ]
test <- df[-train_ind, ]



saveRDS(train, paste(path_to_results,"train70_metab.rds", sep=""))
saveRDS(test, paste(path_to_results,"test30_metab.rds", sep=""))



# matching ----------------------------------------------------------------

eid70 =train$eid
eid30 = test$eid

train = bb[bb$eid %in% eid70,]
test = bb[bb$eid %in% eid30,]

saveRDS(train, paste(path_to_results,"train70_biobank.rds", sep=""))
saveRDS(test, paste(path_to_results,"test30_biobank.rds", sep=""))

train = biomarker[biomarker$eid %in% eid70,]
test = biomarker[biomarker$eid %in% eid30,]

saveRDS(train, paste(path_to_results,"train70_biomarker.rds", sep=""))
saveRDS(test, paste(path_to_results,"test30_biomarker.rds", sep=""))

train = cvd[cvd$eid %in% eid70,]
test = cvd[cvd$eid %in% eid30,]

saveRDS(train, paste(path_to_results,"train70_cvd.rds", sep=""))
saveRDS(test, paste(path_to_results,"test30_cvd.rds", sep=""))




