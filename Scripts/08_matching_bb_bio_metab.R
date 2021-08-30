### Summmer Project -- Matching biomarkers, metabolites, and biobank participants
### 20th July 2021 - Alicia 

# Load packages
library(openxlsx)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(pheatmap)


# Setting pathways and datasets
path_to_results ="/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/updates/"
path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
biobank=readRDS(paste(path, "Results/biobank_clean.rds", sep=""))
biomarker = readRDS(paste(path, "Results/biomarker_master.rds",sep=""))
cvd = readRDS(paste(path, "Results/case_control.rds",sep=""))
metab = readRDS(paste(path, 'Results/metab_no_bio.rds',sep=""))



# Matching  ---------------------------------------------------------------

matched_metab = metab[rownames(metab) %in% biomarker$eid, ]
matched_bb = biobank[biobank$eid %in% rownames(matched_metab),]
matched_bio = biomarker[biomarker$eid %in% rownames(matched_metab),]
matched_cvd = cvd[cvd$eid %in% rownames(matched_metab),]

matched_metab = matched_metab[rownames(matched_metab) %in% matched_cvd$eid,]
matched_bb = matched_bb[matched_bb$eid %in% matched_cvd$eid,]
matched_bio = matched_bio[matched_bio$eid %in% matched_cvd$eid,]

saveRDS(matched_metab,paste(path_to_results, 'matched_metab_base.rds',sep=""))
saveRDS(matched_bb,paste(path_to_results, 'matched_biobank.rds',sep=""))
saveRDS(matched_bio,paste(path_to_results, 'matched_biomarker.rds',sep=""))
saveRDS(matched_cvd,paste(path_to_results, 'matched_cvd.rds',sep=""))








# old ---------------------------------------------------------------------

# -------------------------------------------------------------------------

group1_med = F
group1_cen = F
group2_med = F
group2_cen = F
met_base = F
met_first= F

# -------------------------------------------------------------------------

if (group1_med){
  metab_no_bio=readRDS(paste(path, "Results/cluster_manual/cluster_group1_medoid_base.rds", sep=""))%>% as.data.frame()
  head(metab_no_bio)
  colnames(metab_no_bio)
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/cluster_manual/"
}

if (group1_cen){
  metab_no_bio=readRDS(paste(path, "Results/cluster_manual/cluster_group1_centroid_base.rds", sep=""))%>% as.data.frame()
  head(metab_no_bio)
  colnames(metab_no_bio)
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/cluster_manual/"
}

if (group2_med){
  metab_no_bio=readRDS(paste(path, "Results/cluster_manual/cluster_group2_medoid_base.rds", sep=""))%>% as.data.frame()
  head(metab_no_bio)
  colnames(metab_no_bio)
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/cluster_manual/"
}

if (group2_cen){
  metab_no_bio=readRDS(paste(path, "Results/cluster_manual/cluster_group2_centroid_base.rds", sep=""))%>% as.data.frame()
  head(metab_no_bio)
  colnames(metab_no_bio)
  path_to_results = "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/cluster_manual/"
}

if (met_base){
  metab=readRDS(paste(path, "Results/imputed_metab_base.rds", sep=""))
  metab_cluster = readRDS(paste(path, "Results/cluster_hc_ward_metab_base.rds", sep="")) %>% as.data.frame()
  metab_no_bio = readRDS(paste(path, "Results/cluster_hc_ward_metab_no_bio.rds", sep ="")) %>% as.data.frame()
  head(metab)
  colnames(metab)
  path_to_results ="/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/"
}

if (met_first){
  metab=readRDS(paste(path, "Results/imputed_metab_first.rds", sep=""))
  metab_cluster = readRDS(paste(path, "Results/cluster_metab_first.rds", sep=""))
  head(metab)
  colnames(metab)
  path_to_save =  "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Figures/Preliminary/Air_met_first/"
}



# -----------------Clustered metabolites with no repeated biomarkers --------------------------------------------------------

# Match biomarkers and metab to UK biobank participants 
matched_bio = filter(biomarker, eid %in% rownames(metab_no_bio))
matched_bb=filter(biobank, eid %in% rownames(metab_no_bio))

if (met_base){saveRDS(matched_bb, paste(path,"Results/matched_bb_no_bio_base.rds", sep=""))}
if (met_first){saveRDS(matched_bb, paste(path,"Results/matched_bb_no_bio_first.rds", sep=""))}

if (group1_med){saveRDS(metab_no_bio, paste(path_to_results, "matched_group1_med_base.rds", sep =""))}
if (group1_cen){saveRDS(metab_no_bio, paste(path_to_results, "matched_group1_cen_base.rds", sep =""))}
if (group2_med){saveRDS(metab_no_bio, paste(path_to_results, "matched_group2_med_base.rds", sep =""))}
if (group2_cen){saveRDS(metab_no_bio, paste(path_to_results, "matched_group2_cen_base.rds", sep =""))}
if (met_base){saveRDS(metab_no_bio, paste(path,"Results/matched_clus_no_bio_base.rds", sep=""))}
if (met_first){saveRDS(metab_no_bio, paste(path,"Results/matched_clus_no_bio_first.rds", sep=""))}

if (met_base){saveRDS(matched_bio, paste(path,"Results/matched_biomarkers_base.rds", sep=""))}
if (met_first){saveRDS(matched_bio, paste(path,"Results/matched_biomarkers_first.rds", sep=""))}


# -----------------Metabolites individuals --------------------------------------------------------
matched_metab=filter(metab, rownames(metab) %in% rownames(metab_no_bio))

# Match metab to UK biobank participants 
metab_eid = rownames(metab)
matched_bb=filter(biobank, eid %in% metab_eid) 
matched_metab=filter(metab, rownames(metab) %in% matched_bb$eid) 

metab_cluster_eid = rownames(metab_cluster)
matched_metab_cluster=filter(metab_cluster, rownames(metab_cluster) %in% matched_bb$eid) 



if (met_all){saveRDS(matched_bb, paste(path,"Results/matched_bb_all.rds", sep=""))}
if (met_base){saveRDS(matched_bb, paste(path,"Results/matched_bb_base.rds", sep=""))}
if (met_first){saveRDS(matched_bb, paste(path,"Results/matched_bb_first.rds", sep=""))}

if (met_all){saveRDS(matched_metab, paste(path,"Results/matched_metab_all.rds", sep=""))}
if (met_base){saveRDS(matched_metab, paste(path,"Results/matched_metab_base.rds", sep=""))}
if (met_first){saveRDS(matched_metab, paste(path,"Results/matched_metab_first.rds", sep=""))}

if (met_all){saveRDS(matched_metab_cluster, paste(path,"Results/matched_clus_metab_all.rds", sep=""))}
if (met_base){saveRDS(matched_metab_cluster, paste(path,"Results/matched_clus_metab_base.rds", sep=""))}
if (met_first){saveRDS(matched_metab_cluster, paste(path,"Results/matched_clus_metab_first.rds", sep=""))}




# Data description of Air Pollutants exposures ----------------------------

## 24003: NO2(2010)
## 24004: NOx(2010)
## 24005: PM10(2010)
## 24006: PM2.5(2010) 
## 24007: PM2.5 absorbance(2010) 
## 24008: PM2.5-10 (2010) 
## 24014: Close to major road 

bb = readRDS(paste(path, "Results/matched_bb_base.rds", sep =""))
# Correlation between each air pollutant exposure 
air_pollutant = select(bb, "24003-0.0":"24008-0.0")
colnames(air_pollutant) <- c("NO2","NOx","PM10", "PM2.5", "PM2.5_absorbance","PM2.5-PM10")
cormat = round(cor(air_pollutant, use = "complete.obs", method = "pearson"),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

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
upper_tri

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
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())+
  ggsave(paste(path, "Figures/Preliminary/Air_met_base/airpoll_corr.pdf", sep =""))

# Density plots 
## NO2
length( which( air_pollutant$NO2 >40 ) )
length( which( air_pollutant$NO2 >40 ) )/(length(air_pollutant$NO2)-sum(is.na(air_pollutant$NO2)))*100

pdf(paste(path_to_save,"Density_NO2.pdf", sep=""))
NO2_den <- density(air_pollutant$NO2, na.rm = TRUE) # returns the density data
xmin = min(air_pollutant$NO2, na.rm = TRUE)
xmax = max(air_pollutant$NO2, na.rm = TRUE)
plot(NO2_den, xlab=expression(paste("NO2",mu,"g/m3")), ylab="", main= "4.66% over WHO guided threshold", xlim=c(xmin,xmax))
abline(v=40, col="darkred")
dev.off()

## PM2.5 
length( which( air_pollutant$PM2.5 >10) )
length( which( air_pollutant$PM2.5 >10) )/(length(air_pollutant$PM2.5)-sum(is.na(air_pollutant$PM2.5)))*100

pdf(paste(path_to_save,"Density_PM2.5.pdf", sep=""))
PM2.5_den <- density(air_pollutant$PM2.5, na.rm = TRUE) # returns the density data
xmin = min(air_pollutant$PM2.5, na.rm = TRUE)
xmax = max(air_pollutant$PM2.5, na.rm = TRUE)
plot(PM2.5_den, xlab=expression(paste("PM2.5",mu,"g/m3")), ylab="", main= "47.23% over WHO guided threshold", xlim=c(xmin,xmax))
abline(v=10, col="darkred")
dev.off()

## PM10
length( which( air_pollutant$PM10 >20) )
length( which( air_pollutant$PM10 >20) )/(length(air_pollutant$PM10)-sum(is.na(air_pollutant$PM10)))*100

pdf(paste(path_to_save,"Density_PM10.pdf", sep=""))
PM10_den <- density(air_pollutant$PM10, na.rm = TRUE) # returns the density data
xmin = min(air_pollutant$PM10, na.rm = TRUE)
xmax = max(air_pollutant$PM10, na.rm = TRUE)
plot(PM10_den, xlab=expression(paste("PM10",mu,"g/m3")), ylab="", main= "5.72% over WHO guided threshold", xlim=c(xmin,xmax))
abline(v=20, col="darkred")
dev.off()


# Data description of metabolomics ----------------------------------------

if (met_all){
  metab= readRDS(paste(path, "Results/matched_metab_all.rds",sep=""))
}

if (met_base){
  metab= readRDS(paste(path, "Results/matched_metab_base.rds",sep=""))
  metab_cluster = readRDS(paste(path, "Results/matched_clus_metab_base.rds", sep =""))
}

if (met_first){
  metab= readRDS(paste(path, "Results/matched_metab_first.rds",sep=""))
}


# Look at the distribution of randomly selected metabolites:
par(mfrow = c(3, 3), mar = c(4, 4, 1, 1))
set.seed(1)
s = sample(ncol(metab_cluster), size = 9)
for (i in 1:9) {
  plot(density(metab_cluster[, i]), lwd = 2, col = "navy", main = "",
       las = 1, xlab = "", ylab = "")
}

par(mfrow = c(3, 3), mar = c(4, 4, 1, 1))
metab_log = log(metab)
for (i in 1:9) {
  plot(density(metab_log[, i]), lwd = 2, col = "navy",
       main = "", las = 1, xlab = "", ylab = "")
}

