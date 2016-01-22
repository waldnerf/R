varImpAnalysis <- function(rf.mdl, plotname="", filename=""){
  importance <- as.data.frame(rf.mdl$importance)
  Gini <- importance[order(importance$MeanDecreaseGini),"MeanDecreaseGini", drop = FALSE]
  criterion <- mean(importance$MeanDecreaseGini)
  bestFeatList <- rownames(Gini)[which(importance$MeanDecreaseGini >= criterion)]
  if(filename!=""){
    write.csv(bestFeatList, filename)
  }
  if(plotname!=""){
    pdf(plotname)
    par(mai=c(1,3,1,1)) 
    barplot(Gini$MeanDecreaseGini,beside=TRUE,horiz=TRUE,
            density=NA,
            xlab="Mean Decrease Gini",
            axes=TRUE, names.arg=rownames(Gini), cex.names=0.75, las=1)
    abline(v=criterion, col='red', lty='longdash', lwd=2)
    dev.off()
  }
  return(bestFeatList)
}