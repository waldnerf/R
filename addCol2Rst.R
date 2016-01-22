writeColorTable <- function(inRst, outRstName, df,naCol = '#000000',NAVal=15){
  # attT is either a vector with hexadecimal color values 
  # a nx3 R G B matrix
  require(rgdal)
  if( all(colnames(df)%in% c('numT','colT','attT') == TRUE )){
    inRst <- as(inRst, 'SpatialGridDataFrame')
    df.out <- data.frame(colT= rep(naCol,256), attT = rep('NA',256), numT = 0:255,stringsAsFactors=F)
    df.out[na.omit(match(df$numT,df.out$numT)),'attT'] <- as.character(df$attT)
    df.out[na.omit(match(df$numT,df.out$numT)),'colT'] <- as.character(df$colT)
    writeGDAL(inRst, outRstName, type="Byte", colorTable=list(df.out$colT), catNames=list(df.out$attT), mvFlag=NAVal,options=c("INTERLEAVE=BAND", "COMPRESS=LZW","datatype=INT1S"))
    r<- raster(outRstName)
  } else{
    print(' The data.frame df must have three columns: colT with hexadecimal color codes, attT the class label corresponding to each label
          and numT with the numeric value at which the color and the label has to be coded')
  }
  return(r)
}
