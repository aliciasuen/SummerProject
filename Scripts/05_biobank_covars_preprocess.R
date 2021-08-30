### Summmer Project -- Covariates pre-processing and description from UK Biobank
### 26th May 2021 - Alicia 


# Load packages
library(openxlsx)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

# Load dataset
biobank=readRDS(paste(path, "Results/recoded_covar_clean.rds", sep=""))
head(biobank)
colnames(biobank)

metab=readRDS(paste(path, "Results/selected_metab_base.rds", sep=""))
head(metab)
colnames(metab)


# Removing missing values  ------------------------------------------------

# remove participants without air pollutants and confounding variables 
complete_air= biobank[complete.cases(biobank[c("24003-0.0","24004-0.0","24005-0.0","24006-0.0","24007-0.0","24008-0.0","24014-0.0", "21022-0.0","31-0.0", "6138-0.0", "20116-0.0","1558-0.0","21001-0.0", "eid")]),]

# find data missingness percentage for each column
col_missing <- sort(round(colSums(is.na(complete_air))/nrow(complete_air),3),decreasing = T)
col_missing
# select columns with 15% or less missing data
col_valid <- names(col_missing[col_missing <= 0.15])
df <- complete_air[,col_valid[order(col_valid)]] # sort column names just for easier reading

saveRDS(df, paste(path, "Results/biobank_clean.rds",sep=""))

# Match metab to UK biobank participants 
metab_eid = rownames(metab)
matched_bb=filter(df, eid %in% metab_eid) 


# Data description of Air Pollutants exposures ----------------------------

## 24003: NO2(2010)
## 24004: NOx(2010)
## 24005: PM10(2010)
## 24006: PM2.5(2010) 
## 24007: PM2.5 absorbance(2010) 
## 24008: PM2.5-10 (2010) 
## 24014: Close to major road 

# Correlation between each air pollutant exposure 
air_pollutant = select(matched_bb, c("24003-0.0":"24008-0.0"))
colnames(air_pollutant) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10")
cormat = round(cor(air_pollutant, use = "complete.obs", method = "pearson"),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)


# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggheatmap<- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x=element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank())+
  ggsave(paste(path, "Figures/Preliminary/airpoll_corr.pdf", sep =""))

# Density plots 
bb = readRDS(paste(path, "Results/biobank_clean.rds",sep =""))
metab = readRDS(paste(path,"Results/denoised_clus_no_bio.rds",sep=""))
bb = filter(bb, bb$eid %in% rownames(metab))
air_pollutant = select(bb, c("24003-0.0":"24008-0.0"))
colnames(air_pollutant) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10")

## NO2
length( which(air_pollutant$NO2 >40 ) )
length( which(air_pollutant$NO2 >40 ) )/(length(air_pollutant$NO2)-sum(is.na(air_pollutant$NO2)))*100
  ## "4% over WHO guided threshold"

pdf(paste(path,"Figures/Preliminary/Density_NO2.pdf", sep=""))
NO2_den <- density(air_pollutant$NO2, na.rm = TRUE) # returns the density data
xmin = min(air_pollutant$NO2, na.rm = TRUE)
xmax = max(air_pollutant$NO2, na.rm = TRUE)
plot(NO2_den, xlab=expression(paste("NO2","(",mu,"g/",m^3,")")), ylab="",main="", xlim=c(xmin,xmax))
abline(v=40, col="darkred")
dev.off()

## PM2.5 
length( which( air_pollutant$PM2.5 >10) )
length( which( air_pollutant$PM2.5 >10) )/(length(air_pollutant$PM2.5)-sum(is.na(air_pollutant$PM2.5)))*100
  ## "47.16% over WHO guided threshold"

pdf(paste(path,"Figures/Preliminary/Density_PM2.5.pdf", sep=""))
PM2.5_den <- density(air_pollutant$PM2.5, na.rm = TRUE) # returns the density data
xmin = min(air_pollutant$PM2.5, na.rm = TRUE)
xmax = max(air_pollutant$PM2.5, na.rm = TRUE)
plot(PM2.5_den, xlab=expression(paste("PM2.5","(",mu,"g/",m^3,")")), ylab="",main="", xlim=c(xmin,xmax))
abline(v=10, col="darkred")
dev.off()

## PM10
length( which( air_pollutant$PM10 >20) )
length( which( air_pollutant$PM10 >20) )/(length(air_pollutant$PM10)-sum(is.na(air_pollutant$PM10)))*100
  ## 5.72% over WHO guided threshold

pdf(paste(path,"Figures/Preliminary/Density_PM10.pdf", sep=""))
PM10_den <- density(air_pollutant$PM10, na.rm = TRUE) # returns the density data
xmin = min(air_pollutant$PM10, na.rm = TRUE)
xmax = max(air_pollutant$PM10, na.rm = TRUE)
plot(PM10_den, xlab=expression(paste("PM10","(",mu,"g/",m^3,")")), ylab="", main ="", xlim=c(xmin,xmax))
abline(v=20, col="darkred")
dev.off()


