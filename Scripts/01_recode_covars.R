### Summmer Project -- Extracting and Recoding covariates from UK Biobank
### 21st May 2021 - Alicia 

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library" ))

# Load packages
library(openxlsx)
library(data.table)
library(dplyr)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"


# Extract variables of interest -------------------------------------------

# Load annotation
covars=read.csv(paste(path, "Dictionaries/covar_annot.csv", sep=""))
ids_vector=covars$field_id

# Extract dataset
path_to_biobank <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Data/ukb26390.csv"
biobank_columns <- colnames(data.frame(fread(path_to_biobank, nrows=0), check.names = F))
head(biobank_columns)

length(biobank_columns) # 6289 total
sum(grepl("^\\d+-\\d+.\\d+$", biobank_columns)) # all have pattern number-number.number (6288), first is eid
sum(grepl("^\\d+-0.\\d+$", biobank_columns)) # out of which 2013 are baseline

# Create combined regex for each possible string match
## e.g. ^123-0.\\d+$ means that we are looking for a string 
### (^ means start) 123-0.[any one or more digits] ($ means end)
patterns <- paste("^",ids_vector,"-0.\\d+$", sep="", collapse = "|")

biobank_matches <- grepl(patterns, biobank_columns)
selected_biobank_columns <- biobank_columns[biobank_matches]

# Convert to number and check if some are missing
selected_ids_vector_biobank <- as.numeric(gsub(pattern="-\\d+.\\d+","",selected_biobank_columns))
setdiff(ids_vector,selected_ids_vector_biobank) # check if any are missing

head(selected_biobank_columns)
length(selected_biobank_columns)

# Save selected colnames 
write.table(selected_biobank_columns, 
            "../Dictionaries/biobank_selected_col.csv", 
            row.names = FALSE, col.names=FALSE, sep = "\t")

# load column names
biobank_cols <- read.csv("../Dictionaries/biobank_selected_col.csv", check.names = F, header = F)

# Active consent 
withdrawn=as.character(read.csv("../Data/w19266_20200204.csv")[,1])
withdrawn_update=as.character(read.csv("../Data/w19266_20210809.csv")[,1])
biobank_cols <- as.character(biobank_cols[[1]]) # extract that single vector and cast into strings
biobank <- data.frame(fread(path_to_biobank), check.names = F)[, unlist(c("eid",biobank_cols))]
biobank_1 <- filter(biobank, !(eid %in% withdrawn)) # remove no consent
biobank_2<- filter(biobank, !(eid %in% withdrawn_update)) # remove no consent
metab = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
biobank_3 = biobank_2 %>% filter(biobank_2$eid %in% rownames(metab))
saveRDS(biobank, paste(path,"Results/selected_biobank.rds",sep = ""))

# Recoding variables ------------------------------------------------------

# Load extracted data
mydata=readRDS(paste(path, "Results/selected_biobank.rds", sep =""))
# Load coding ids
mycoding=read.csv(paste(path,"Dictionaries/Codings_Showcase.csv", sep = ""))

# Make copy of data
recoded_covar=mydata

# Iterating to give recoded categories 
for (i in 1:nrow(covars)){
  if (!is.na(covars$coding_id[i])){
    myid=covars$coding_id[i]
    
    mycoding_field=mycoding[which(mycoding[,1]==myid),]
    mycoding_field=mycoding_field[,-1]
    rownames(mycoding_field) <- mycoding_field[,1]
    
    # Recoding categories
    mygrep=grep(paste0(covars$field_id[i],"-"), fixed=TRUE, colnames(recoded_covar))
    for (g in 1:length(mygrep)){
      recoded_covar[,mygrep[g]]=ifelse(
        mydata[,mygrep[g]] %in% rownames(mycoding_field),
        as.character(mycoding_field[as.character(mydata[,mygrep[g]]),"Meaning"]),
        mydata[,mygrep[g]])
    }
  }
}
# structure of original data
str(mydata)
# structure of recoded data
str(recoded_covar)
# check missing values for first instances
recoded_covar %>% select(contains(".0.")) %>% apply(., 2, function(x) sum(is.na(x)))

saveRDS(recoded_covar, paste(path,"Results/recoded_covar.rds", sep=""))



