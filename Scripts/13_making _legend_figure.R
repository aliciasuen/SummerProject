
### Summmer Project - Making legend Figure_report for all the plots 
### 17th Aug 2021 - Alicia 

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

# --------# making the legend for biomarkers and metabolites in combined dataset #-----------------------------------------------------------------

combine = readRDS(paste(path, "Results_report/updates/combined_biometab.rds", sep=""))
combine = combine[order(as.numeric(row.names(combine))),] 
combine_col = colnames(combine) %>% as.data.frame()
biomarker = combine_col[1:28,] %>% as.data.frame()
biomarker$dataset<-rep("biomarker",times=nrow(biomarker))
colnames(biomarker)[1] = "features"
metab = combine_col[29:78,] %>% as.data.frame()
metab$dataset<-rep("metab",times=nrow(metab))
colnames(metab)[1] = "features"
combine_col<-rbind(biomarker,metab)
combine_col$features <- chartr(".", " ", combine_col$features)
combine_col$number = rownames(combine_col)
combine_col$mycolour = c(rep("brown",28), rep("blue",50))
bio_col = combine_col[1:28,]
metab_col = combine_col[29:78,]

pdf(paste(path,"Figure_report/legend_bio.pdf", sep=""),width=2, height=4)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c("Biomarkers",paste0(bio_col$number,"-",bio_col$features)), text.col = "brown",
       bty='n', cex=0.6)
dev.off()

pdf(paste(path,"Figure_report/legend_metab_clus.pdf", sep=""),width=7, height=4)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c("Metabolites",paste0(metab_col$number,"-",metab_col$features)), text.col= "blue",
       bty='n', cex=0.6, ncol=2, text.width = 0.06)
dev.off()

pdf(paste(path,"Figure_report/legend_combined.pdf", sep=""), width=9, height=4)
par(mar=c(0,0,0,0), mfrow=c(1,2),pty = "s")
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,1), ylim=c(0,1))
legend("topleft",
       legend=c("Biomarkers",paste0(bio_col$number,"-",bio_col$features)), text.col = "brown",
       bty='n', ncol=1, cex=0.6)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,1), ylim=c(0,1))
legend("topleft",
       legend=c("Metabolites",paste0(metab_col$number,"-",metab_col$features)), text.col= "blue",
       bty='n', cex=0.6, ncol=2, text.width = 0.05)
dev.off()




plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("top",
       legend=c("Biomarkers", paste0(combine_col[1:28,]$number,"-",combine_col[1:28,]$features), 
                "Metabolites",paste0(combine_col[29:74,]$number,"-",combine_col[29:74,]$features),
       bty="n",text.col = c("black", darken(rep("brown", time = 28),0.5), 
                            "black", darken(rep("blue" , time = 46), 0.5)), cex = 0.6))
dev.off()

legend("top",
       legend=c("Sociodemographic", mapply(foo, plot_annot$label_ref[1:18], 1:18),
                "Health risk",mapply(foo, plot_annot$label_ref[19:38], 19:38),
                "Environmental", mapply(foo, plot_annot$label_ref[39:46], 39:46),
                "Medical",mapply(foo, plot_annot$label_ref[47:62], 47:62),
                "Biomarker",mapply(foo, plot_annot$label_ref[63:90], 63:90)),
       bty="n",text.col = c("black",darken(rep(mycolours[1],18),0.5),
                            "black",darken(rep(mycolours[2],20),0.5),
                            "black",darken(rep(mycolours[3],time=8),0.5),
                            "black",darken(rep(mycolours[4],time=16),0.5),
                            "black",darken(rep(mycolours[5],28),0.5)), cex=0.7)

# ------- # making the legend for clusters -------------------------------------

# Loading metab_no_bio datasets #
hc_ward_out = readRDS(paste(path, "Results_report/updates/hc_ward_out.rds", sep =""))
hc_ward_mysummary = readRDS(paste(path, "Results_report/updates/hc_ward_mysummary.rds",sep=""))
hc_ward_xsummarised = readRDS(paste(path,"Results_report/updates/cluster_hc_ward_metab_no_bio.rds",sep = ""))
hc_ward_grouping = readRDS(paste(path,"Dictionaries/grouping_hc_ward_metab_no_bio.rds",sep = ""))
metab_clus = readRDS(paste(path,"Results_report/updates/denoised_clus_no_bio.rds",sep = ""))

## Plotting Legends grouping ##
summarised_name = colnames(hc_ward_xsummarised)
legend = hc_ward_mysummary$description[,1:2]
legend$main = ifelse(legend$feature %in% summarised_name, "yes","no")
legend <- legend[order(legend$group),]
legend_1 = legend[1:79,]
legend_2 = legend[80:160,]
nb.cols = max(legend$group)


pdf(paste(path,"Figure_report/hiercluster/legend_grouping_part1.pdf", sep=""),width=7, height=5)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c(paste0(legend_1$group,"-",legend_1$feature)),
       bty='n', cex=0.6, ncol=2, text.width = 0.09, 
       text.col = c(ifelse(legend_1$main=="yes","red","black")))
dev.off()

pdf(paste(path,"Figure_report/hiercluster/legend_grouping_part2.pdf", sep=""),width=7.5, height=5)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
legend("topleft",
       legend=c(paste0(legend_2$group,"-",legend_2$feature)),
       bty='n', cex=0.6, ncol=2, text.width = 0.12,
       text.col = c(ifelse(legend_2$main=="yes","red","black")))
dev.off()


