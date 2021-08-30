### Summmer Project -- Recoding covariates from UK Biobank
### 26th May 2021 - Alicia 

# set path to include my libraries
.libPaths(c("/rds/general/user/cls1017/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library" ))

# Load packages
library(openxlsx)
library(data.table)
library(dplyr)
library(plyr)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

mydata = readRDS(paste(path, "Results/recoded_covar.rds",sep=""))
df=mydata
# Employment status (MC) //6142// -----------------------------------------
# Unemployed: Unemployed , Unable to work because of sickness or disability, Doing unpaid or voluntary work
# -7,5,4,6,3 -> 5
# -3 -> NA
employment_cols <- c("6142-0.0","6142-0.1","6142-0.2","6142-0.3",
                     "6142-0.4","6142-0.5","6142-0.6")
df[,employment_cols] <- 
  lapply(df[,employment_cols], 
         mapvalues, # plyr function
         from=c("Unable to work because of sickness or disability","Doing unpaid or voluntary work", "Full or part-time student","Looking after home and/or family"),
         to=c("Unemployed","Unemployed", "Unemployed","Unemployed"))

df$`6142-0.0` <- apply(df[,employment_cols], 1, function(row){
  min(row, na.rm = T) # minimum is highest because 1 == employed
})

df <- df[,!(names(df) %in% c("6142-0.1","6142-0.2","6142-0.3",
                             "6142-0.4","6142-0.5","6142-0.6"))]
df$`6142-0.0`[df$`6142-0.0` %in% c("None of the above","Prefer not to answer")] = NA
table(df$`6142-0.0`)
df$`6142-0.0` = factor(df$`6142-0.0`)
levels(df$`6142-0.0`)=c("Actively employed","Retired","Not actively employed")

# Parental history  -------------------------------------------------------

df %>% select(matches("20107-|20110-")) %>% unlist %>% table

group1=c("Chronic bronchitis/emphysema","Diabetes","High blood pressure","Stroke","Heart disease")
names(group1)=c("COPD", "diabetes","hypertension","stroke","CHD")
group1NA=c("Prefer not to answer (group 1)", "Do not know (group 1)")
group2=c("Breast cancer","Bowel cancer","Lung cancer","Prostate cancer")
names(group2)=c("breast","bowel","lung","prostate")
group2NA=c("Prefer not to answer (group 2)", "Do not know (group 2)")

# Make empty columns
tmp=df %>% select(eid)
diseases=c(group1,group2)
x=matrix(0,nrow=nrow(tmp),ncol=length(diseases))
colnames(x)=paste0("parent","_",names(diseases))
tmp=cbind(tmp,x)

# Subset columns with information
parent_data=df %>% select(eid,matches("20107-|20110-"))
missing=parent_data$eid[rowSums(is.na(parent_data)) == ncol(parent_data)] # No missing information

# Find eids with parental history for each diseases
for (d in 1:length(diseases)){
  print(names(diseases)[d])
  disease=diseases[d]
  myeids=apply(parent_data[,-1],2,function(x) parent_data$eid[grep(disease, x)]) %>% unlist
  if (disease %in% group1){
    tomatch=group1NA
  }
  if (disease %in% group2){
    tomatch=group2NA
  }
  myNA_father=apply(parent_data[,2:11],2,function(x) parent_data$eid[x %in% tomatch]) %>% unlist
  myNA_mother=apply(parent_data[12:22],2,function(x) parent_data$eid[x %in% tomatch]) %>% unlist
  myNA=intersect(myNA_father,myNA_mother)
  myeids=unique(myeids)
  colindex=grep(names(diseases)[d], colnames(tmp))
  tmp[tmp$eid %in% myeids,colindex]=1
  tmp[tmp$eid %in% myNA,colindex]=NA
}

# Check and add to mydata
summary(tmp) # Missing data: 28766 for Group 1; 26339 for Group 2 diseases
df=inner_join(df,tmp,by="eid")
df <- df %>% select(!matches("20107-|20110-"))


# Education  --------------------------------------------------------------

# Qualification (MC) //6138// 
# Re-categorise them into 
# High : College or University degree
# Intermediate : CSEs or equivalent, NVQ or HND or HNC or equivalent,O levels/GCSEs or equivalent,A levels/AS levels or equivalent,Other professional qualifications eg: nursing, teaching
# Low : None of the above
# 2,3,4,5,6 -> 2
# 5,6 -> 6
#-3 -> NA

education_cols <- c("6138-0.0", "6138-0.1", "6138-0.2","6138-0.3", "6138-0.4", "6138-0.5")
df[,education_cols] <- 
  lapply(df[,education_cols], 
         mapvalues, # plyr function
         from=c("O levels/GCSEs or equivalent","CSEs or equivalent","NVQ or HND or HNC or equivalent","Other professional qualifications eg: nursing, teaching"),
         to=c("A levels/AS levels or equivalent","A levels/AS levels or equivalent","A levels/AS levels or equivalent","A levels/AS levels or equivalent"))

df$`6138-0.0` <- apply(df[,education_cols], 1, function(row){
  min(row, na.rm = T) # minimum is highest because 1 == university
})

df <- df[,!(names(df) %in% c("6138-0.1", "6138-0.2","6138-0.3", "6138-0.4", "6138-0.5"))]
df$`6138-0.0` = na_if(df$`6138-0.0`, 'Prefer not to answer')
df$`6138-0.0` = factor(df$`6138-0.0`, levels=c("A levels/AS levels or equivalent","College or University degree","None of the above"))
table(df$`6138-0.0`)
levels(df$`6138-0.0`) = c("Intermediate","Low","High")


# Ethnic background //21000// ---------------------------------------------

# white/other
# white 1, 1001, 1002, 1003 -> 1
# non-white  -3, -1, 6, 5, 3004, 2004, 3003, 2003, 3, 3002, 2002, 2, 3001, 2001, 4, 4003, 4002, 4001-> 6
df$`21000-0.0`[df$`21000-0.0` %in% c("Any other white background","British","Irish","White")] = "White"
df$`21000-0.0`[df$`21000-0.0` %in% c("African","Any other Asian background","Any other Black background","Any other mixed background",
                                     "Asian or Asian British","Bangladeshi","Caribbean","Chinese","Indian","Pakistani","White and Asian",
                                     "White and Black African","White and Black Caribbean","Black or Black British","Mixed","Other ethnic group")] = "Others"
df$`21000-0.0`[df$`21000-0.0` %in% c("Do not know","Prefer not to answer")] = NA
df$`21000-0.0`<-factor(df$`21000-0.0`, levels=c("White","Others"))
table(df$`21000-0.0`)


# Smoking status ----------------------------------------------------------

# Combining the smoking status with tobacco 
## 3 =  Current - On most or all day 
## 2 = Current - only occasionally 
## 1 = Previous
## 0 = Never 
## smoking status //20116//
df$`20116-0.0`<-na_if(df$`20116-0.0`, 'Prefer not to answer')
df$`20116-0.0` <- ifelse(df$`20116-0.0` =='Never' , 'Never',
                         ifelse(df$`20116-0.0` == 'Previous', 'Previous',
                                ifelse((df$`20116-0.0`=='Current') & (df$`1239-0.0` =="Only occasionally"),'Current occasionally', 
                                       ifelse((df$`20116-0.0`=="Current") & (df$`1239-0.0` =="Yes, on most or all days"),'Current on most/all days', NA))))

df$`20116-0.0`<-factor(df$`20116-0.0`, levels=c("Never","Previous","Current occasionally","Current on most/all days"))
table(df$`20116-0.0`)


# Alcohol frequency  1558 ------------------------------------------------------
df$`1558-0.0`<-na_if(df$`1558-0.0`, 'Prefer not to answer')

df$`1558-0.0`[df$`1558-0.0` %in% c("Special occasions only","One to three times a month","Never")] = "Less than once a week"
df$`1558-0.0`<-factor(df$`1558-0.0`, levels=c("Less than once a week","Once or twice a week","Three or four times a week","Daily or almost daily"))

table(df$`1558-0.0`)


# -------------------------------------------------------------------------

saveRDS(df, paste(path,"Results/recoded_covar_clean.rds", sep=""))

