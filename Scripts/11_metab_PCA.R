### Summmer Project -- PCA metabolites 
### 26th July 2021 

library(dplyr)
library(xtable)
library(cluster)    # clustering algorithms
library(factoextra)
library(FactoMineR)
library(pheatmap)
library(ggplot2)
library(grDevices)
library(RColorBrewer)


path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"

# Load dataset of imputed metabolite
metab_base = readRDS(paste(path,"Results_report/updates/imputed_metab_base.rds",sep = ""))
df = metab_base
# df = df %>% sample_frac(.05)

source(paste(path,"Functions/barbara_clustering.R", sep=""))

### Composite Metabolites  ----

# Run PCA
set.seed(123)
mypca <- prcomp(df, scale.=T)
loadings = mypca$rotation
saveRDS(loadings, paste(path, "Results_report/pca/pca_loadings.rds", sep=""))
out=summary(mypca)
ev=out$importance[2,]
ev
saveRDS(ev, paste(path, "Results_report/pca/pca_ev.rds", sep=""))

#### Plots ####
# Scree plot
loadings = readRDS(paste(path, "Results_report/pca/pca_loadings.rds", sep=""))
ev = readRDS(paste(path, "Results_report/pca/pca_ev.rds", sep=""))
pdf(paste(path, "Figure_report/Preliminary/pca/composite_score_ev.pdf",sep =""), width=5, height=5)
par(mar=c(5,5,1,1))
plot(ev, pch=19, col="navy", las=1, type="b", ylim=c(0,1),
     ylab="Proportion of explained variance", xlab="PCs")
points(cumsum(ev), pch=19, col="red", type="b")
legend("right", pch=19, col=c("navy", "red"),
       legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
dev.off()

# Loadings
mycor=out$rotation
pdf(paste(path, "Figure_report/Preliminary/pca/composite_score_cor_2.pdf",sep =""), width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,1:2], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[2]*" ("*a*"% e.v.)", list(a=round(ev[2]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,2], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,2]+sign(mycor[,2])*0.1, cex = 0.7,
     labels=1:10, col="navy")
# legend("topright", cex=0.5, text.col="navy", bg="white",
#        legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
#                                   "Processed meat", "Beef", "Lamb", "Pork",
#                                   "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()

pdf(paste(path, "Figure_report/Preliminary/pca/composite_score_cor_3.pdf",sep =""), width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,3)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[3]*" ("*a*"% e.v.)", list(a=round(ev[3]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,3], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,3]+sign(mycor[,3])*0.1, cex = 0.7,
     labels=1:10, col="navy")
# legend("topright", cex=0.5, text.col="navy", bg="white",
#        legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
#                                   "Processed meat", "Beef", "Lamb", "Pork",
#                                   "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()

pdf(paste(path, "Figure_report/Preliminary/pca/composite_score_cor_4.pdf",sep =""), width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,4)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[4]*" ("*a*"% e.v.)", list(a=round(ev[4]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,4], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,4]+sign(mycor[,4])*0.1, cex = 0.7,
     labels=1:10, col="navy")
# legend("topleft", cex=0.5, text.col="navy", bg="white",
#        legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
#                                   "Processed meat", "Beef", "Lamb", "Pork",
#                                   "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()

pdf(paste(path, "Figure_report/Preliminary/pca/composite_score_cor_5.pdf",sep =""), width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,5)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[5]*" ("*a*"% e.v.)", list(a=round(ev[5]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,5], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,5]+sign(mycor[,5])*0.1, cex = 0.7,
     labels=1:10, col="navy")
# legend("topright", cex=0.5, text.col="navy", bg="white",
#        legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
#                                   "Processed meat", "Beef", "Lamb", "Pork",
#                                   "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()

pdf(paste(path, "Figure_report/Preliminary/pca/composite_score_cor_6.pdf",sep =""), width=5, height=5)
par(mar=c(5,5,1,1))
plot(mycor[,c(1,6)], xlim=c(-1,1), ylim=c(-1,1), cex=0.1, pch=19, las=1,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=round(ev[1]*100, digits=2))), 
     ylab=substitute(PC[6]*" ("*a*"% e.v.)", list(a=round(ev[6]*100, digits=2))))
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,6], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.1, mycor[,6]+sign(mycor[,6])*0.1, cex = 0.7,
     labels=1:10, col="navy")
# legend("topleft", cex=0.5, text.col="navy", bg="white",
#        legend=c(paste(1:10, "-",c("Oily fish","Non-oily fish", "Poultry",
#                                   "Processed meat", "Beef", "Lamb", "Pork",
#                                   "Cooked vegetable","Salad/raw vegetable","Fresh fruit"))))
dev.off()


res=mypca$x # scores
dim(res)
saveRDS(res, paste(path, "Results_report/pca/pca_scores.rds", sep=""))

## taking 8 PCs to explain more than 90% of the variance ## 
df$metab_PC1=res[match(rownames(df),rownames(res)),1]
df$metab_PC2=res[match(rownames(df),rownames(res)),2]
df$metab_PC3=res[match(rownames(df),rownames(res)),3]
df$metab_PC4=res[match(rownames(df),rownames(res)),4]
df$metab_PC5=res[match(rownames(df),rownames(res)),5]
df$metab_PC6=res[match(rownames(df),rownames(res)),6]
df$metab_PC7=res[match(rownames(df),rownames(res)),7]
df$metab_PC8=res[match(rownames(df),rownames(res)),8]

summary(df[c("metab_PC1","metab_PC2","metab_PC3","metab_PC4","metab_PC5","metab_PC6","metab_PC7","metab_PC8")])

df = df[,161:168]
saveRDS(df, paste(path, "Results_report/pca/pca_PC8.rds", sep =""))

### Metabolites table ----
# Load dataset
loadings=readRDS(paste(path, "Results_report/pca/pca_loadings.rds", sep=""))
ev=readRDS(paste(path, "Results_report/pca/pca_ev.rds", sep=""))
scores = readRDS(paste(path, "Results_report/pca/pca_scores.rds", sep=""))

metabolites = rbind(loadings,ev)

# change row names
rownames(metabolites)[161] = "Explained Variance"

# round decimals
metabolites = round(metabolites,2)

# taking only the first 10 PCs 
metabolites = metabolites[, 1:10]

# Save
sink(paste(path, "Results_report/pca/metab_tab.txt", sep =""))
print(xtable(metabolites,auto = TRUE),
      include.rownames = TRUE, include.colnames = TRUE,
      tabular.environment = "longtable",
      floating = FALSE,
      hline.after = c(0,nrow(metabolites)-1))
sink()
