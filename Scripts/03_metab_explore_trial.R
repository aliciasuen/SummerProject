### Summmer Project -- Metabolomics Data description
### 23rd May 2021 - Alicia 

# Load packages
library(openxlsx)
library(data.table)
library("plyr") 
library(dplyr)
library(imputeLCMD)


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

# Load dataset
metab=readRDS(paste(path, "Data/Metabolomics/ukb46678_19266.rds", sep=""))
head(metab)
colnames(metab) # Instance 0 :  recorded by Initial assessment visit 
                # Instance 1 :  recorded by First repeat assessment visit


# Metabolomics dataset cleaning (save repeated measurements separately) -------------------------------------------

# Extract colnames of metab dataset
metab_columns <- colnames(metab)
head(metab_columns)
length(metab_columns) # 337 total
sum(grepl("^\\d+-\\d+.\\d+$", metab_columns)) # all have pattern number-number.number (336), first is eid
sum(grepl("^\\d+-0.\\d+$", metab_columns)) # out of which 168 are baseline
sum(grepl("^\\d+-1.\\d+$", metab_columns)) # out of which 168 are baseline


metab_base <- grepl("^\\d+-0.\\d+$", metab_columns)
metab_first <-grepl("^\\d+-1.\\d+$", metab_columns)
metab_base_columns <- metab_columns[metab_base]
metab_first_columns <- metab_columns[metab_first]


# Save selected colnames 
write.table(metab_base_columns, 
            "../Dictionaries/metab_base_columns.csv", 
            row.names = FALSE, col.names=FALSE, sep = "\t")

write.table(metab_first_columns, 
            "../Dictionaries/metab_first_columns.csv", 
            row.names = FALSE, col.names=FALSE, sep = "\t")

# load column names
metab_base_cols <- read.csv(paste(path, "Dictionaries/metab_base_columns.csv",sep=""), check.names = F, header = F)
metab_first_cols <- read.csv(paste(path, "Dictionaries/metab_first_columns.csv",sep=""), check.names = F, header = F)

# Matching with metab to UK biobank  
metab_base_cols <- as.character(metab_base_cols[[1]]) # extract that single vector and cast into strings
metab_base_data <- select(metab, "eid_52569", all_of(metab_base_cols))

metab_first_cols <- as.character(metab_first_cols[[1]]) # extract that single vector and cast into strings
metab_first_data <- select(metab, "eid_52569", all_of(metab_first_cols))

# Delete participants with all NAs 
metab_base_final = metab_base_data[rowSums(is.na(metab_base_data)) != ncol(metab_base_data)-1, ] # 117413
metab_first_final = metab_first_data[rowSums(is.na(metab_first_data)) != ncol(metab_first_data)-1, ] # 5128

saveRDS(metab_base_final, paste(path,"Results/selected_metab_base.rds",sep = ""))
saveRDS(metab_first_final, paste(path,"Results/selected_metab_first.rds",sep = ""))


# Metabolomics data description -------------------------------------------

metab=readRDS(paste(path, "Results/selected_metab_base.rds", sep=""))
 
missing<-colSums(is.na(metab))
percent<-missing/nrow(metab)
    ## missing data less than 10% --> no need to exclude 

metab=readRDS(paste(path, "Results/selected_metab_first.rds", sep=""))

missing<-colSums(is.na(metab))
percent<-missing/nrow(metab)
## missing data less than 10% --> no need to exclude 



# Metabolomics - save repeated as a one dataset  --------------------------
metab_columns <- colnames(metab)

metab_base=readRDS(paste(path,"Results/selected_metab_base.rds",sep = ""))
metab_first=readRDS(paste(path,"Results/selected_metab_first.rds",sep = ""))

colnames(metab_base) <- sub("-0.0", "", colnames(metab_base))
colnames(metab_first) <- sub("-1.0", "", colnames(metab_first))

metab_all= rbind(metab_base, metab_first) 
missing_col<-sort(colSums(is.na(metab_all)),decreasing = T)
col_missing_percent<- sort(round(colSums(is.na(metab_all))/nrow(metab_all),3),decreasing = T)

missing_row<-sort(rowSums(is.na(metab_all)), decreasing = T)
row_missing_percent<- sort(round(rowSums(is.na(metab_all))/ncol(metab_all),3),decreasing = T)

saveRDS(metab_all, paste(path,"Results/selected_metab_all.rds",sep = ""))
