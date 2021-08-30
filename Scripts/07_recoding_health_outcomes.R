### Summmer Project -- Recoding Health Outcomes 
### 15th July 2021 - Alicia 


# Load packages
library(openxlsx)
library(data.table)
library(dplyr)
library(tidyverse)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

## Extract data set----
data=data.frame(fread("../Data/ukb26390.csv",nrows=1))
myfields=list("53","21022","20001","20002") # fields of data attending ac, age at recruit, self-report case of cancer, self-report non-cancer illness

# Extracting the column ids
column_id=grep("eid", colnames(data))
found_fieldids=NULL
for (k in 1:length(myfields)){
mygrep=grep(paste0("X",myfields[k],"."), fixed=TRUE, colnames(data))
if (length(mygrep)>0){
found_fieldids=c(found_fieldids, myfields[k])
}
column_id=c(column_id, mygrep)
}

# Extracting required columns from dataset
extracted=data.frame(fread("../Data/ukb26390.csv", select=column_id))
withdrawn=as.character(read.csv("../Data/w19266_20200204.csv")[,1])
mydata=subset(extracted, !extracted$eid %in% withdrawn)
saveRDS(mydata, "../Results/extract_health_outcomes.rds")

### Define diseases----
# Load data
mydata=readRDS("../Results/extract_health_outcomes.rds")
clus_no_bio = readRDS("../Results/metab_no_bio.rds")
biobank=readRDS(paste(path, "Results/biobank_clean.rds", sep=""))

# Set diseases
diseases=c("cardiovascular", "hypertension", "diabetes", "respiratory", "autoimmune")
names(diseases)=diseases

# Create dataset
x=matrix(0,nrow=nrow(biobank),ncol=length(diseases))
colnames(x)=names(diseases)
comorbid=cbind(biobank, x)
rownames(comorbid)=comorbid$eid




# Annotation of disease ---------------------------------------------------


# Cardiovascular (all circulatory except hypertension)

icd10_cardiovascular=c(paste0("I0",0:2), # acute rheumatic fever
                       paste0("I0",5:9), # chronic rheumatic
                       paste0("I",20:25), # ischemic heart
                       paste0("I",26:28), # pulmonary
                       paste0("I",30:52), # other heart disease
                       paste0("I",60:69), # cerebrovascular
                       paste0("I",70:76), # arteries -- excluding I77.6 (in autoimmune)
                       paste0("I77", 0:5),
                       paste0("I77", 7:9),
                       paste0("I",78:79), 
                       paste0("I",81:82), # vein thrombosis
                       paste0("I",95:99)) # other circulatory

icd9_cardiovascular=c(paste0(390:392), # acute rheumatic fever
                      paste0(393:398), # chronic rheumatic
                      paste0(410:414), # ischemic heart
                      paste0(415:417), # pulmonary
                      paste0(420:429), # other heart disease
                      paste0(430:438), # cerebrovascular
                      paste0(440:445), # arteries -- excluding 446 and 4476 (in autoimmune)
                      paste0(447, 0:5), 
                      paste0(447, 7:9),
                      paste0(448:449), 
                      "453") #other vein thrombosis 

# Hypertension

icd10_hypertension=c(paste0("I", 10:15))

icd9_hypertension=paste0(401:405)


# Diabetes
icd10_diabetes=c("E11", "E12", "E13", "E14")

# icd9_diabetes="250"
icd9_diabetes=c(paste0(paste0("250", 0:9), 0),paste0(paste0("250", 0:9), 2))


# Respiratory (all)
icd10_respiratory=c(paste0("J", 30:39), # other upper
                    paste0("J", 40:47), # chronic lower
                    paste0("J", 60:70), # lung 
                    paste0("J", 80:84), # other interstitium
                    paste0("J", 85:86), # lower
                    paste0("J", 90:94), # other pleura
                    paste0("J", 95:99)) # other respiratory

icd9_respiratory=c(paste0(470:478), # other upper
                   paste0(480:488), # influenza and pneumonia
                   paste0(490:496), # chronic
                   paste0(500:508), # lung
                   paste0(510:519)) # other

# Autoimmune
icd10_autoimmune=readLines("../Dictionaries/icd10_autoimmune.txt")
icd9_autoimmune=readLines("../Dictionaries/icd9_autoimmune.txt")

## Self-reported illness codes
# Cardiovascular (all circulatory except hypertension)
self_cardiovascular=as.character(c(1066:1068,
                                   1074:1083,
                                   1086:1088,
                                   1093:1094))

# Hypertension
self_hypertension=as.character(c(1065,1072))

# Diabetes
self_diabetes=as.character(c(1220,1223))

# Respiratory (all)
self_respiratory=as.character(c(1111:1115,
                                1117,
                                1120:1126))

# Autoimmune
self_autoimmune=readLines("../Dictionaries/self_report_autoimmune.txt")


# HES dataset -------------------------------------------------------------


# HES: Hospital Episode Statistics
hesin_diag <- data.frame(fread("../Data/hesin_diag.txt"))
hesin <- data.frame(fread("../Data/hesin.txt")) # Diagnosis data
colnames(hesin)

# Select unique identifiers and episode start date
hesin_sub=hesin %>% na_if("") %>%
mutate(epistart=coalesce(epistart, admidate)) %>%
select(eid,ins_index,epistart, disdate)

# Join by eid and ins_index
hes=inner_join(hesin_diag, hesin_sub, by=c("eid","ins_index"))
head(hes)
sum(is.na(hes$epistart)) # Successfully replaced missing values
saveRDS(hes, "../Results/joined_hes.rds")

# Format date
hes=readRDS("../Results/joined_hes.rds")
hes=hes %>% mutate(epistart=as.Date(epistart,"%d/%m/%Y"))
hes=hes %>% mutate(disdate=as.Date(disdate,"%d/%m/%Y"))
summary(hes$disdate)

# HES table with all instances for defined study population
myhes=hes %>% filter(eid %in% rownames(clus_no_bio))
saveRDS(myhes, "../Results/myhes.rds")

myhes=readRDS("../Results/myhes.rds")

# Basline data
baseline=biobank %>% select(eid,"21022-0.0")

# Joining hes table with baseline data
hes_baseline=left_join(myhes, baseline, by="eid") %>%
  # filter(epistart <= date_baseline) %>%
  select(eid, diag_icd9, diag_icd10)

# Self-reported illness
mygrep=c(1,grep("X20002.0.", fixed=TRUE, colnames(mydata)))
self_data=mydata[mydata$eid %in% biobank$eid,mygrep]

# Recode comorbidities
for (d in 1:length(diseases)){
  print(names(diseases)[d])
  disease=diseases[d]
  icd10_list=eval(parse(text=paste0("icd10_",disease)))
  icd9_list=eval(parse(text=paste0("icd9_",disease)))
  self_list=eval(parse(text=paste0("self_",disease)))
  
  # ICD10 codes
  print("Diagnosed")
  diag_eids=hes_baseline %>%
    filter(grepl(paste0(paste0("^", icd10_list),collapse="|"), diag_icd10)|
             grepl(paste0(paste0("^", icd9_list),collapse="|"), diag_icd9)) %>%
    .$eid
  
  # Self-reported codes
  print("Self-reported")
  self_eids=apply(self_data[,-1], 2,
                  function(x) self_data$eid[grep(paste0(self_list,collapse="|"),x)]) %>%
    unlist
  
  # Recode
  myeids=c(diag_eids, self_eids) %>% unique %>% as.character
  print(table(comorbid[myeids,names(diseases)[d]]))
  comorbid[myeids,names(diseases)[d]]=1
  print(table(comorbid[,names(diseases)[d]]))
  cat("\n")
}



# CVD ---------------------------------------------------------------------
# Extract date and age at baseline
sum(is.na(mydata$X53.0.0)) # No missing
sum(is.na(mydata$X21022.0.0)) # No missing
baseline=mydata %>%
  select(eid, X53.0.0, X21022.0.0) %>%
  mutate(X53.0.0=as.Date(X53.0.0,"%Y-%m-%d"))
colnames(baseline)=c("eid","date_baseline","age_baseline")

# Find first instance of lung cancer
cvd=myhes %>%
  filter(grepl(paste0(paste0("^", icd9_cardiovascular),collapse="|"), diag_icd9)|
           grepl(paste0(paste0("^", icd10_cardiovascular),collapse="|"), diag_icd10)) %>%
  group_by(eid) %>%
  filter(epistart==min(epistart)) %>%
  distinct(eid, .keep_all=T) %>%
  select(eid, diag_icd9, diag_icd10, epistart)
colnames(cvd)=c("eid",paste0(c("diag_icd9", "diag_icd10", "epistart"),"_cvd"))
# Add to data set
outcome=left_join(baseline,cvd,by="eid")
colnames(outcome)



# Self-reported CVD
self_cardiovascular=as.character(c(1066:1068,
                                   1074:1083,
                                   1086:1088,
                                   1093:1094))
# Self-reported illness
mygrep=c(1,grep("X20002.0.", fixed=TRUE, colnames(mydata)))
self_data=mydata[mydata$eid %in% biobank$eid,mygrep]
colnames(self_data)=c("eid",paste0("self",1:33))

# Add to data set
outcome=inner_join(outcome,self_data,by="eid")
colnames(outcome)

### Find prevalent cases ---
### If a participant was diagnosed with cvd before baseline

# Remove all prevalent CVD
def_pop=outcome[-which(outcome$date_baseline >= outcome$epistart_cvd),]
nrow(outcome)-nrow(def_pop) # 7086

## Case/control status
def_pop$cvd_status=0
def_pop$cvd_status[!is.na(def_pop$epistart_cvd)]=1
table(def_pop$cvd_status)

# def_pop2=def_pop %>%
#   mutate(diag_icd9=coalesce(diag_icd9_lung,diag_icd9_bladder,diag_icd9_other),
#          diag_icd10=coalesce(diag_icd10_lung,diag_icd10_bladder,diag_icd10_other),
#          epistart=coalesce(epistart_lung,epistart_bladder,epistart_other))
# str(def_pop4)
# Calculate time to diagnosis and age at diagnosis
def_pop$time_diag_days=difftime(def_pop$epistart,
                                def_pop$date_baseline,
                                 units = "days") %>% as.numeric()
def_pop$time_diag_years=def_pop$time_diag_days/365.25
def_pop$age_diag=def_pop$age_baseline+def_pop$time_diag_years

### Restructure ---
case_control=def_pop %>% select(eid:age_baseline,cvd_status:age_diag) %>%
  mutate(case_status=factor(cvd_status,levels = c(0:1),labels = c("control","cvd")))
str(case_control)
table(case_control$case_status)

saveRDS(case_control, "../Results/case_control.rds")





# #### Final check that there is no prevalent cancers ####
# ## Disease definition - ICD10
# icd10_cancer=c(c(paste0("C0", 0:9), paste0("C", 10:14)), # lip, oral cavity, larynx
#                paste0("C", 15:26), # digestive
#                paste0("C", 30:39), # respiratory
#                paste0("C", 40:41), # bone, cartilage
#                paste0("C", 43:44), # skin
#                paste0("C", 45:49), # mesothelial
#                paste0("C", 50), # breast
#                paste0("C", 51:58), # female genital
#                paste0("C", 60:63), # male genital
#                paste0("C", 64:68), # urinary
#                paste0("C", 69:72), # nervous
#                paste0("C", 73:75), # endocrin
#                paste0("C", 76:80), # other sites
#                paste0("C", 81:96), # lymphoid
#                paste0("C", 97), # multiple
#                paste0("D0", 0:9), # in situ
#                paste0("D", 37:48)) # uncertain behaviour
# 
# icd9_cancer=c(paste0(140:149), # lip, oral cavity, larynx
#               paste0(150:159), # digestive
#               paste0(160:165), # respiratory
#               paste0(170:176), # bone, cartilage
#               paste0(179:189), # genital
#               paste0(190:199), # unspecified
#               paste0(200:209), # lymphoid
#               paste0(230:234), # in situ
#               paste0(235:238), # uncertain 
#               paste0(239)) # unspecified
# 
# # Self-reported cancer
# mygrep=c(1,grep("X20001.0.", fixed=TRUE, colnames(mydata)))
# self_data=mydata[mydata$eid %in% biobank$eid,mygrep]
# 
# # ICD10 codes
# diag_eids=hes_baseline %>%
#   filter(grepl(paste0(paste0("^", icd10_cancer),collapse="|"), diag_icd10)|
#            grepl(paste0(paste0("^", icd9_cancer),collapse="|"), diag_icd9)) %>%
#   .$eid
# 
# # Self-reported codes
# self_eids=self_data$eid[rowSums(!is.na(self_data[,-1]))>0]
# 
# # Recode
# myeids=c(diag_eids,self_eids) %>% unique %>% as.character
# biobank$cancer=ifelse(comorbid$eid %in% myeids,1,0)
# sum(comorbid$cancer)==0
# 
# # Restructure
# str(comorbid)
# comorbid=comorbid %>%
#   select(eid,diseases) %>%
#   mutate_at(vars(diseases), as.factor)
# summary(comorbid)
# 
# saveRDS(comorbid, "../Results/comorbid.rds")
# saveRDS(biobank, "../Results/biobank_final.rds")
# 
# 
# 



