writeQML <- function(inShp, col, label, value, filename){
  require(RColorBrewer)
  require(XML)
  inNames <- names(inShp)
  names(inShp)[which(names(inShp)==value)] <- "Value"
  names(inShp)[which(names(inShp)==col)] <- "v"
  names(inShp)[which(names(inShp)==label)] <- "Label"
  
  inShp$v <- unlist(lapply(as.character(inShp$v),function(x) paste(c(col2rgb(x),'255'),collapse = ',')))
  inShp$Label <- as.character(inShp$Label)
  
  inShp <- inShp[-which(duplicated(inShp@data[,c('Value','v','Label')])),]
  
  # set alpha
  alpha = 1
  
  base = newXMLNode("qgis")
  addAttributes(base,version="2.8.1-Wien", minimumScale="0", maximumScale="1e+08", simplifyDrawingHints="0", minLabelScale="0", maxLabelScale="1e+08",simplifyDrawingTol="1" ,simplifyMaxScale="1", hasScaleBasedVisibilityFlag="0", simplifyLocal="1", scaleBasedLabelVisibilityFlag="0")
  trans <- newXMLNode("transparencyLevelInt", 255)
  rend <- newXMLNode("renderer-v2", attrs = c(attr=value,symbollevels="0",type="categorizedSymbol"))
  
  # sort the categories
  categories <- newXMLNode("categories")
  category <- lapply(seq_along(inShp$Value),function(x){newXMLNode("category", attrs = c(symbol = as.character(x-1), value = inShp$Value[x], label = inShp$Label[x]))
  })
  addChildren(categories,category)
  
  # sort the symbols
  symbols <- newXMLNode("symbols")
  symbol <- lapply(seq_along(inShp$Value),function(x){dum.sym <- newXMLNode("symbol", attrs = c(outputUnit="MM",alpha=alpha,type="marker",name=as.character(x-1)))
  layer <- newXMLNode("layer", attrs =c(pass="0",class="SimpleMarker",locked="0"))
  prop <- newXMLNode("prop", attrs =c(k="color",v= inShp$v[x]))
  addChildren(layer, prop)
  addChildren(dum.sym, layer)
  }) 
  addChildren(symbols, symbol)
  
  # add categories and symbols to rend
  addChildren(rend, list(categories, symbols))
  addChildren(base, list(trans, rend))
  
  # save to qml-file
  writeLines(saveXML(base, prefix = "<!DOCTYPE qgis >"), filename)
}

