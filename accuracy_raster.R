conf_mat <- function(v1,v2){
  v1F <- levels(as.factor(v1))
  v2F <- levels(as.factor(v2))
  fList<- base::union(v1F,v2F)
  cm <- table(factor(v1,levels=fList),factor(v2,levels=fList))
  return(cm)
}


calc_pop_ct <- function(ct, pop) {
  # Below uses the notation of Pontius and Millones (2011)
  nijsum <- matrix(rowSums(ct), nrow=nrow(ct), ncol=ncol(ct))
  Ni <- matrix(pop, nrow=nrow(ct), ncol=ncol(ct))
  # pop_ct is the population contigency table
  pop_ct <- (ct / nijsum) * (Ni / sum(pop))
  dimnames(pop_ct)[[1]] <- dimnames(ct)[[1]]
  dimnames(pop_ct)[[2]] <- dimnames(ct)[[2]]
  class(pop_ct) <- 'table'
  return(pop_ct)
}


calc_Q <- function(pop_ct) {
  # Calculate quantity disagreement (Pontius and Millones, 2011, eqns 2-3)
  qg_mat = abs(rowSums(pop_ct) - colSums(pop_ct))
  return(sum(qg_mat) / 2)
}

calc_A <- function(pop_ct) {
  # Calculate allocation disagreement (Pontius and Millones, 2011, eqns 4-5)
  diag_indices <- which(diag(nrow(pop_ct)) == TRUE)
  ag_mat = 2 * apply(cbind(rowSums(pop_ct) - pop_ct[diag_indices],
                           colSums(pop_ct) - pop_ct[diag_indices]), 1, min)
  return(sum(ag_mat) / 2)
}

# Adds margins to contingency table
add_ct_margins <- function(ct) {
  # For user's, producer's, and overall accuracy formulas, see Table 
  # 21.3 in Foody, G.M., Stehman, S.V., 2009. Accuracy Assessment, in: 
  # Warner, T.A., Nellis, M.D., Foody, G.M. (Eds.), The SAGE Handbook of 
  # Remote Sensing. SAGE.
  diag_indices <- which(diag(nrow(ct)) == TRUE)
  users_acc <- ct[diag_indices] / colSums(ct)
  prod_acc <- ct[diag_indices] / rowSums(ct)
  overall_acc <- sum(ct[diag_indices]) / sum(ct)
  ct <- addmargins(ct)
  dimnames(ct)[[1]][nrow(ct)] <- "Total"
  dimnames(ct)[[2]][nrow(ct)] <- "Total"
  ct <- rbind(ct, Producers=c(users_acc, NA))
  ct <- cbind(ct, Users=c(prod_acc, NA, overall_acc))
  ct <- round(ct, digits=4)
  dimnames(ct) <- list(predicted=dimnames(ct)[[1]],
                       observed=dimnames(ct)[[2]])
  class(ct) <- 'table'
  return(list(cmMargins = ct, OA =overall_acc ))
}

accuracy_assessment <- function(reference , classification , filename = FALSE){ 
  cellID <- which(!is.na(values(reference))) 
  ext <- extract(stack(reference,classRst), cellID)
  ct <- conf_mat(ext[,2],ext[,1])
  pop <- rowSums(ct)
  pop_ct <- calc_pop_ct(ct, pop)
  Q <- calc_Q(pop_ct)
  A <- calc_A(pop_ct)
  ct_margins <- add_ct_margins(ct) 
  out <- list(cm = ct, OA = ct_margins$OA, Q=Q, A=A, cmMargins =  ct_margins$cmMargins)
  return(out)
}

cm2tex <- function(cm,caption ='', filename= ''){
  library(xtable)
  tab<-xtable(cm$cmMargins , caption= caption,caption.placement ="top")
  print(tab,file=filename,append=T,table.placement = "h",caption.placement="top", hline.after=seq(from=-1,to=nrow(tab),by=1))
}


dateFromL8 <- function(filename){
  date <- strsplit(filename,'_')[[1]][2]
  date <- paste(substr(date,1,4),substr(date,5,6),substr(date,7,8),sep='-')
  return(date)
}

dateFromMulti <- function(filename){
  date <- strsplit(strsplit(filename,'_')[[1]][3],'.t')[[1]][1]
  date <- paste(substr(date,1,4),substr(date,5,6),substr(date,7,8),sep='-')
  return(date)
}


library(raster)
library(tools)
library(Hmisc)

site <- 'france'
rootDir <- '/export/synology-nfs/imagines/WorkingData/sites'
refDir  <- file.path(rootDir, site,'01_field_data')
texDir  <- file.path(rootDir, site,'multi/03-accuracy')
clDir   <- file.path(rootDir, site,'multi/02-classification')
plotDir <- '/export/synology-nfs/imagines/WorkingDocuments/plot'

if(site == 'ukraine'){
  setwd('/export/synology-nfs/imagines/WorkingData/sites/ukraine/01_field_data')
  
  refRst <- raster('ukraine_reference_data_class_S4T5.tif')  
  idRst <- raster('ukraine_reference_data_S4T5.tif')

  refCsv <- read.table('ukraine_reference_data_S4T5.csv',header = TRUE, sep=',')
  
  idRef <- subset(refCsv,sample==1,select=zone)
  refRst[!idRst %in% unique(idRef$zone)]<-NA
  plot(refRst)
  
  # RF on bands
  inRstList <- list.files('/export/synology-nfs/imagines/WorkingData/sites/ukraine/spot4/06-classification', pattern= 'RF_all',full.names=TRUE)
  oaRFbands<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    cm2tex(acc,caption=caption,filename=texName)
    
    oaRFbands<-rbind(oaRFbands,acc$OA)
  }
 
  
  # kmeans on NDVI
  inRstList <- list.files('/export/synology-nfs/imagines/WorkingData/sites/ukraine/spot4/06-classification/kmeans', pattern= 'classif',full.names=TRUE)
  oaKmNDVI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    cm2tex(acc,caption=caption,filename=texName)
    
    oaKmNDVI<-rbind(oaKmNDVI,acc$OA)
  }

  
  # ML on NDVI
  inRstList <- list.files('/export/synology-nfs/imagines/WorkingData/sites/ukraine/spot4/06-classification/', pattern= 'ML_all',full.names=TRUE)
  oaMLNDVI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    cm2tex(acc,caption=caption,filename=texName)
    
    oaMLNDVI<-rbind(oaMLNDVI,acc$OA)
  }
  
  plotName <- file.path(plotDir,paste(site,'_overallacc_vs_time.pdf',sep=''))
  pdf(plotName)
  plot(oaRFbands,type='o',pch=20,ylim=c(0,1), ann=FALSE)
  lines(oaKmNDVI,type='o',pch=17)
  lines(oaMLNDVI,type='o',pch=22, lty=2)
  title(main=paste('Evolution of the overall accuracy for',capitalize(site)), xlab= '',ylab='Overall Accuracy')
  legend(1,1,c('RF - bands', 'Kmeans NDVI', 'ML NDVI'),pch=c(20,17,22), lty=c(1,1,2))
  dev.off()
  
} else if (site == 'usa-maricopa') {
  setwd(refDir)
  refRst <- raster('maricopa_reference_data_class_S4T5.tif')
  idRst  <- raster('maricopa_reference_data_S4T5.tif')
  plot(refRst)
  
  refCsv <- read.table('maricopa_reference_data_S4T5.csv',header = TRUE, sep=',')
  idRef <- subset(refCsv,sample==1,select=zone)
  refRst[!idRst %in% unique(idRef$zone)]<-NA
  plot(refRst)
  
  dateList <- 
  
    
  # RF abnds
  inRstList <- list.files(clDir, pattern= 'RF_all',full.names=TRUE)
  oaRFbands<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    cm2tex(acc,caption=caption,filename=texName)
    
    oaRFbands<-rbind(oaRFbands,acc$OA)
  }
  plot(oaRFbands)
  
  # kmeans on NDVI
  inRstList <- list.files(file.path(clDir,'kmeans'), pattern= 'class',full.names=TRUE)
  oaKmNDVI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    cm2tex(acc,caption=caption,filename=texName)
    
    oaKmNDVI<-rbind(oaKmNDVI,acc$OA)
  }
  plot(oaKmNDVI)
  
  
  # ML on NDVI
  inRstList <- list.files(clDir, pattern= 'ML',full.names=TRUE)
  oaMLNDVI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    cm2tex(acc,caption=caption,filename=texName)
    
    oaMLNDVI<-rbind(oaMLNDVI,acc$OA)
  }
  plot(oaMLNDVI)
  
  plotName <- file.path(plotDir,paste(site,'_overallacc_vs_time.pdf',sep=''))
  pdf(plotName)
  plot(oaRFbands,type='o',pch=20,ylim=c(0,1), ann=FALSE)
  lines(oaKmNDVI,type='o',pch=17)
  lines(oaMLNDVI,type='o',pch=22, lty=2)
  title(main=paste('Evolution of the overall accuracy for',capitalize(site)), xlab= '',ylab='Overall Accuracy')
  legend(1,1,c('RF - bands', 'Kmeans NDVI', 'ML NDVI'),pch=c(20,17,22), lty=c(1,1,2))
  dev.off()
  
} else if (site == 'usa-greatsaltplains') {
  
  setwd(refDir)
  refRst <- raster('gslp_reference_data_class_S4T5.tif')
  idRst  <- raster('gslp_reference_data_S4T5.tif')
  plot(refRst)
  
  refCsv <- read.table('gslp_reference_data_S4T5.csv',header = TRUE, sep=',')
  idRef <- subset(refCsv,sample==1,select=zone)
  refRst[!idRst %in% unique(idRef$zone)]<-NA
  plot(refRst)
  
  inRstList <- list.files(clDir, pattern= 'RF_all',full.names=TRUE)
  
  oaRFbands<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    cm2tex(acc,caption=caption,filename=texName)
    
    oaRFbands<-rbind(oaRFbands,acc$OA)
  }
  plot(oaRFbands)
  
  # kmeans on NDVI
  inRstList <- list.files(file.path(clDir,'kmeans'), pattern= 'class',full.names=TRUE)
  oaKmNDVI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    cm2tex(acc,caption=caption,filename=texName)
    
    oaKmNDVI<-rbind(oaKmNDVI,acc$OA)
  }
  plot(oaKmNDVI)
  
  
  # ML on NDVI
  inRstList <- list.files(clDir, pattern= 'ML',full.names=TRUE)
  oaMLNDVI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    cm2tex(acc,caption=caption,filename=texName)
    
    oaMLNDVI<-rbind(oaMLNDVI,acc$OA)
  }
  plot(oaMLNDVI)
  
  plotName <- file.path(plotDir,paste(site,'_overallacc_vs_time.pdf',sep=''))
  pdf(plotName)
  plot(oaRFbands,type='o',pch=20,ylim=c(0,1), ann=FALSE)
  lines(oaKmNDVI,type='o',pch=17)
  lines(oaMLNDVI,type='o',pch=22, lty=2)
  title(main=paste('Evolution of the overall accuracy for',capitalize(site)), xlab= '',ylab='Overall Accuracy')
  legend(1,1,c('RF - bands', 'Kmeans NDVI', 'ML NDVI'),pch=c(20,17,22), lty=c(1,1,2))
  dev.off()
  
}else if (site == 'france') {
  setwd(refDir)
  refRst <- raster('france_reference_data_class_spot4.tif')
  idRst  <- raster('france_reference_data_zone_spot4.tif')
  plot(refRst)
  
  refCsv <- read.table('france_reference_data_multi.csv',header = TRUE, sep=',')
  idRef <- subset(refCsv,sample==1,select=zone)
  refRst[!idRst %in% unique(idRef$zone)]<-NA
  plot(refRst)
  
  #get date 
  inRstList <- list.files(file.path(clDir), pattern= 'classificationRF_bpv.*tif$',full.names=TRUE)
  t <- as.Date(unlist(lapply(inRstList,FUN=dateFromMulti)),'%Y-%m-%d')
  
  # accuracy for rf
  inRstList <- list.files(file.path(clDir,'RF'), pattern= 'RF_all',full.names=TRUE)[grepl('masked',list.files(file.path(clDir,'RF'), pattern= 'RF_all',full.names=TRUE))]
  oaRFbands<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    #caption <- basename(file_path_sans_ext(inRstName))
    #texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    #cm2tex(acc,caption=caption,filename=texName)
    
    oaRFbands<-rbind(oaRFbands,acc$OA)
  }
  plot(t,as.numeric(oaRFbands))
  
  # kmeans on NDVI
  inRstList <- list.files(file.path(clDir,'kmeans'), pattern= 'class',full.names=TRUE)[grepl('masked',list.files(file.path(clDir,'kmeans'), pattern= 'class',full.names=TRUE))]
  oaKmNDVI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    caption <- basename(file_path_sans_ext(inRstName))
    texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    #cm2tex(acc,caption=caption,filename=texName)
    
    oaKmNDVI<-rbind(oaKmNDVI,acc$OA)
  }
  plot(t,as.numeric(oaKmNDVI))
  
  # ML on NDVI
  inRstList <- list.files(file.path(clDir,'ml'), pattern= 'NDVI',full.names=TRUE)[grepl('masked',list.files(file.path(clDir,'ml'), pattern= 'NDVI',full.names=TRUE))]
  oaMLNDVI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    #caption <- basename(file_path_sans_ext(inRstName))
    #texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    #cm2tex(acc,caption=caption,filename=texName)
    
    oaMLNDVI<-rbind(oaMLNDVI,acc$OA)
  }
  plot(t,as.numeric(oaMLNDVI))
  
  # RF on NDVI
  inRstList <- list.files(file.path(clDir,'RF'), pattern= 'NDVI',full.names=TRUE)[grepl('masked',list.files(file.path(clDir,'RF'), pattern= 'NDVI',full.names=TRUE))]
  oaRFNDVI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    #caption <- basename(file_path_sans_ext(inRstName))
    #texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    #cm2tex(acc,caption=caption,filename=texName)
    
    oaRFNDVI<-rbind(oaRFNDVI,acc$OA)
  }
  plot(t,as.numeric(oaRFNDVI))
  
  # RF on FAPAR
  inRstList <- list.files(file.path(clDir), pattern= 'RF_FAPAR.*tif$',full.names=TRUE)
  oaRFFAPAR<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    #caption <- basename(file_path_sans_ext(inRstName))
    #texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    #cm2tex(acc,caption=caption,filename=texName)
    
    oaRFFAPAR<-rbind(oaRFFAPAR,acc$OA)
  }
  plot(t, as.numeric(oaRFFAPAR))
  
  inRstList <- list.files(file.path(clDir), pattern= 'RF_LAI.*tif$',full.names=TRUE)
  oaRFLAI<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    #caption <- basename(file_path_sans_ext(inRstName))
    #texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    #cm2tex(acc,caption=caption,filename=texName)
    
    oaRFLAI<-rbind(oaRFLAI,acc$OA)
  }
  plot(t,as.numeric(oaRFLAI))
  
  # RF on FCOVER
  inRstList <- list.files(file.path(clDir), pattern= 'RF_FCOVER.*tif$',full.names=TRUE)
  oaRFFCOVER<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    #caption <- basename(file_path_sans_ext(inRstName))
    #texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    #cm2tex(acc,caption=caption,filename=texName)
    
    oaRFFCOVER<-rbind(oaRFFCOVER,acc$OA)
  }
  plot(t,as.numeric(oaRFFCOVER))
  
  # RF on LAI
  
  
  # RF on BPV
  inRstList <- list.files(file.path(clDir), pattern= 'RF_bpv.*tif$',full.names=TRUE)
  oaRFBPV<-c()
  for (inRstName in inRstList){
    classRst <- raster(inRstName)
    acc <- accuracy_assessment(reference = refRst, classification = classRst)
    
    #caption <- basename(file_path_sans_ext(inRstName))
    #texName <- file.path(texDir,paste(basename(file_path_sans_ext(inRstName)),'.tex',sep=''))
    #cm2tex(acc,caption=caption,filename=texName)
    
    oaRFBPV<-rbind(oaRFBPV,acc$OA)
  }
  plot(t,as.numeric(oaRFBPV))
  
  plotName <- file.path(plotDir,paste(site,'_overallacc_vs_time.pdf',sep=''))
  pdf(plotName)
  plot(t,as.numeric(oaRFbands),type='o',pch=20,ylim=c(0,1), ann=FALSE)
  lines(t,as.numeric(oaKmNDVI),type='o',pch=17)
  lines(t,as.numeric(oaMLNDVI),type='o',pch=22, lty=2)
  lines(t,as.numeric(oaRFNDVI),type='o',pch=17, lty=2)
  title(main=paste('Evolution of the overall accuracy for',capitalize(site)), xlab= '',ylab='Overall Accuracy')
  legend(mean(t),0.3,c('RF - bands', 'Kmeans NDVI', 'ML NDVI','RF NDVI'),pch=c(20,17,22,17), lty=c(1,1,2,2))
  dev.off()
  
  plotName <- file.path(plotDir,paste(site,'_overallacc_vs_time_bpv.pdf',sep=''))
  pdf(plotName)
  plot(t,as.numeric(oaRFbands),type='o',pch=22,ylim=c(0,1), ann=FALSE, col="#3F74B0")
  lines(t,as.numeric(oaRFLAI),type='o',pch=23,col="#F988F9")
  lines(t,as.numeric(oaRFFAPAR),type='o',pch=17,col="#497F33")
  lines(t,as.numeric(oaRFFCOVER),type='o',pch=20, lty=2,col="#519988")
  lines(t,as.numeric(oaRFNDVI),type='o',pch=16, lty=2,col="#177AFC")
  lines(t,as.numeric(oaRFBPV),type='o',pch=15, lty=2,col="#994B3D")
  title(main=paste('Evolution of the overall accuracy for',capitalize(site)), xlab= '',ylab='Overall Accuracy')
  legend(mean(t),0.3,c('RF - all bands', 'LAI', 'FAPAR','FCOVER', 'NDVI','All BPV'),pch=c(20,23,17,22,16,15), lty=c(1,1,1,2,2,2), col = c("skyblue",'slategray','seagreen','black','sienna1','violetred4'))
  dev.off()
  
}


maskRst <- raster("/export/synology-nfs/imagines/WorkingData/sites/france/multi/aoi_france.tif")
maskRst[maskRst!=-10000]<-1
maskRst[is.na(maskRst)]<-1
maskRst[maskRst==-10000]<-0
plot(maskRst)
writeRaster(maskRst,"/export/synology-nfs/imagines/WorkingData/sites/france/multi/aoi_france_S4res.tif")

inRstList <- list.files("/export/synology-nfs/imagines/WorkingData/sites/france/multi/02-classification/kmeans", "classif",full.names=T)

for(inRst in inRstList){
  r <- raster(inRst)
  r[maskRst==0]<-NA
  outRst<-paste(strsplit(inRst,'.tif')[[1]][1],'_masked.tif',sep="")
  writeRaster(r,outRst)
}
