maxLabeling <- function(reference = labelRst, cluster = clustRst, clustCentroid = CmistCsv, filename=""){
  print('Computing cross tabulation')
  ct <- na.omit(crosstab(labelRst,clustRst, useNA=FALSE, progress='text'))
  ct <- as.data.frame(ct)
  colnames(ct)[1:2] <- c('reference','cluster')
  
  # Listing the number of clusters
  clusterList <- as.numeric(levels(unique(ct[,2])))
  # Listing the number of non-zero clusters
  ct.nonzero <- ct[-which(ct$Freq==0),]
  nonzero.Clust <- as.numeric(as.character(unique(ct.nonzero[,2])))
  zeroClust <- setdiff(clusterList, nonzero.Clust)
  
  if(length(zeroClust>0)){
    print('Merging Zero-populated clusters')
    for(i in zeroClust){
      # look for the closest cluster and merge them
      v <- clustCsv[which(clustCsv$cluster == i),c(1:nvar)]
      y <- clustCsv[which(!clustCsv$cluster %in% c(i,zeroClust)),]
      newCluster <- y[as.numeric(which.min((apply(y[,c(1:nvar),drop=F],1,function(x,v) sum(abs(x-v)), v=v)))),'cluster']
      clustRst[clustRst == i ] <- newCluster
    }
  }
  print('Assigning  classes to clusters')
  
  rcl <- matrix(,nrow=0,ncol=2)
  
  for(i in nonzero.Clust){
    ct.i <- subset(ct, ct[,2]==i)
    ct.i$Freq <- ct.i$Freq/sum(ct.i$Freq)*100
    print(paste(i,max(ct.i$Freq)))
    rcl <- rbind(rcl,ct.i[which.max(ct.i$Freq),c(2,1)])
  }
  
  beginCluster(n=2)
  rc <- clusterR(clustRst, reclassify, args=list(rcl=rcl))
  endCluster()
  
  if (filename != "") {
    out <-   writeRaster(rc,filename,overwrite=TRUE,options=c("COMPRESS=LZW",datatype='INT2S' ))
  } 
}