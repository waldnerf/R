addColorTable <- function(inRstName, outRstName, colT, attT){
  # attT is either a vector with hexadecimal color values 
  # a nx3 R G B matrix
  library(rgdal)
  r<- readGDAL(inRstName)
  writeGDAL(r, outRstName, type="Byte", colorTable=list(colT), catNames=list(attT), mvFlag=11L)
}

writeColorTable <- function(inRst, outRstName, df,naCol = '#000000'){
  # attT is either a vector with hexadecimal color values 
  # a nx3 R G B matrix
  require(rgdal)
  if( all(colnames(df)%in% c('numT','colT','attT') == TRUE )){
    inRst <- as(inRst, 'SpatialGridDataFrame')
    df.out <- data.frame(colT= rep(naCol,256), attT = rep('NA',256), numT = 0:255,stringsAsFactors=F)
    df.out[na.omit(match(df$numT,df.out$numT)),'attT'] <- as.character(df$attT)
    df.out[na.omit(match(df$numT,df.out$numT)),'colT'] <- as.character(df$colT)
    writeGDAL(inRst, outRstName, type="Byte", colorTable=list(df.out$colT), catNames=list(df.out$attT), mvFlag=11L)
    r<- raster(outRstName)
  } else{
    print(' The data.frame df must have three columns: colT with hexadecimal color codes, attT the class label corresponding to each label
          and numT with the numeric value at which the color and the label has to be coded')
  }
  return(r)
}

library(rgdal)
library(raster)

setwd("/home/waldner/Documents")
r <- raster(nrow=10, ncol=10)
r[] = 1
r[51:100] = 2
r[3:6, 1:5] = 3
r[6:8, 5:10] = 5

writeRaster(r,'test1.tif',overwrite=T)

colT <-  c("#FF0000", "#FF9900" ,"#CCFF00","#CC9900")
attT <- c('a','b','c','d')
df <- data.frame(colT=colT,attT=attT, numT=c(5,1,2,3))



WriteColorTable(r, 'supertest.tif',df=df)
addColorTable('test1.tif', 'supertest.tif', colT, attT)



colT<-t(as.matrix(col2rgb(c(palette = 1:4))


writeGDAL(r, 'test2.tif', type="Byte", colorTable=list(colT), catNames=list(attT), mvFlag=11L)
Ginfo <- GDALinfo('test2.tif', returnColorTable=TRUE, returnCategoryNames=TRUE)
CT <- attr(Ginfo, "ColorTable")[[1]]
CT
attr(Ginfo, "CATlist")[[1]]
fn<- 'test2.tif'
with <- readGDAL(fn)
with <- readGDAL(fn, silent=TRUE)
table(with$band1)
table(as.numeric(with$band1))
with1 <- readGDAL(fn, as.is=TRUE)
table(with1$band1)
spplot(with, col.regions=CT)
tf <- tempfile()
cN <- levels(with$band1)
with$band1 <- as.integer(with$band1)-1


setwd("/export/synology-nfs/imagines/WorkingData/sites/france/multi/02-classification/RF")

inRstList<-list.files('.','classificationRF_allbands-.*masked.tif')
r <-raster(inRstList[34])
ggplot(r)

inLabel<- i
#inLabel<- inLabel[with(inLabel,order('nclass')),]
#label<-unique(inLabel$crop)


colvec <-rep('#000000',256)

addColorTable(inRstList[34], '/home/waldner/Documents/test.tif', colT=cbPalette, attT=label)


theme_set(theme_bw())
gplot(r) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = 'white', high = 'blue') +
  coord_equal()

## The displayable colors from four planes of Lab space
ab <- expand.grid(a = (-10:15)*10,
                  b = (-15:10)*10)
require(graphics); require(stats) # for na.omit
par(mfrow = c(2, 2), mar = .1+c(3, 3, 3, .5), mgp = c(2,  .8,  0))

Lab <- cbind(L = 20, ab)
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
clipped <- attr(na.omit(srgb), "na.action")
srgb[clipped, ] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
image((-10:15)*10, (-15:10)*10, matrix(1:(26*26), ncol = 26), col = cols,
      xlab = "a", ylab = "b", main = "Lab: L=20")

Lab <- cbind(L = 40, ab)
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
clipped <- attr(na.omit(srgb), "na.action")
srgb[clipped, ] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
image((-10:15)*10, (-15:10)*10, matrix(1:(26*26), ncol = 26), col = cols,
      xlab = "a", ylab = "b", main = "Lab: L=40")

Lab <- cbind(L = 60, ab)
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
clipped <- attr(na.omit(srgb), "na.action")
srgb[clipped, ] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
image((-10:15)*10, (-15:10)*10, matrix(1:(26*26), ncol = 26), col = cols,
      xlab = "a", ylab = "b", main = "Lab: L=60")

Lab <- cbind(L = 80, ab)
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
clipped <- attr(na.omit(srgb), "na.action")
srgb[clipped, ] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
image((-10:15)*10, (-15:10)*10, matrix(1:(26*26), ncol = 26), col = cols,
      xlab = "a", ylab = "b", main = "Lab: L=80")

cols <- t(col2rgb(palette())); rownames(cols) <- palette(); cols
zapsmall(lab <- convertColor(cols, from = "sRGB", to = "Lab", scale.in = 255))
stopifnot(all.equal(cols, # converting back.. getting the original:
                    round(convertColor(lab, from = "Lab", to = "sRGB", scale.out = 255)),
                    check.attributes = FALSE))