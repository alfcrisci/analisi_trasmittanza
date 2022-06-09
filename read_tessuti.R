library(openxlsx)
library(knitr)
library(dplyr)
library(tidyr)
library(clustertend)
library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(ClusterR)
library(clusterSim)
library(smacof)
library(factoextra)
library(gridExtra)
library(MASS)
library(paran)
library(BBmisc)
library(stats)
library(ClustGeo)
library(ggcorrplot)
library(kableExtra)
library(ggbiplot)

############################################################################################




############################################################################################

standardize <- function(x) {(x - mean(x))/sd(x)}

############################################################################################

setwd("/home/alf/Scrivania/lav_manteco")


tess=read.xlsx("Comfort_termico_rev.xlsx")
saveRDS(tess,"tess.RDS")
names(tess)
vars=c("ID","WO","armatura","peso_mq","fili_cm","colpi_cm","titolo_ordito","titolo_trama","spessore")


mat_work=tess[,vars]

dim(mat_work)
mat_work=mat_work[-c(27,31,33,37),]
label_work=as.factor(mat_work$armatura)
mat_work=mat_work[,4:9]
pca_tess=prcomp(mat_work,center=T,scale=T)

g <- ggbiplot(pca_tess,
              obs.scale = 1,
              var.scale = 1,
              groups = label_work,
              ellipse = TRUE,
              circle = FALSE,
              # draw ellipse around points (+/-) 1 standard deviation
              ellipse.prob = 0.68) +
  scale_color_discrete(name = '') +
  theme(legend.direction = "horizontal", legend.position = "top", aspect.ratio = 0.5)

g



