SVM_raster_predict <- function(inraster,svmModel,...)
{
  # We need to load randomForest (note this is only
  # loaded once per worker on most cluster setups):
  require(kernlab)
  
  # First, preserve the names:
  band_names <- dimnames(inraster)[3][[1]]
  print(band_names)
  
  inraster_matrix <- inraster
  dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
  
  inraster.df <- as.data.frame(inraster_matrix)
  
  # We need to re-set the names because randomForest requires it:
  names(inraster.df) <- band_names
  
  # Now we can use this in a predict statement:
  out_predictions <- predict(svmModel,inraster.df)  
  out_predictions_array <- array(as.double((as.character(out_predictions))),dim=c(dim(inraster)[1:2],1))
  return(out_predictions_array)
}

RF_raster_predict <- function(inraster,rfModel,...)
{
  # We need to load randomForest (note this is only
  # loaded once per worker on most cluster setups):
  require(randomForest)
  
  # First, preserve the names:
  band_names <- dimnames(inraster)[3][[1]]
  print(band_names)
  
  inraster_matrix <- inraster
  dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
  
  inraster.df <- as.data.frame(inraster_matrix)
  
  # We need to re-set the names because randomForest requires it:
  names(inraster.df) <- band_names
  
  # Now we can use this in a predict statement:
  out_predictions <- predict(rfModel,inraster.df)  
  out_predictions_array <- array(as.double((as.character(out_predictions))),dim=c(dim(inraster)[1:2],1))
  return(out_predictions_array)
}

RF_raster_predict_prob <- function(inraster,rfModel,...)
{
  
  # We need to load randomForest (note this is only
  # loaded once per worker on most cluster setups):
  require(randomForest)
  
  # First, preserve the names:
  band_names <- dimnames(inraster)[3][[1]]
  print(band_names)
  
  inraster_matrix <- inraster
  dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
  
  inraster.df <- as.data.frame(inraster_matrix)
  
  # We need to re-set the names because randomForest requires it:
  names(inraster.df) <- band_names
  
  # Now we can use this in a predict statement:
  out_predictions <- predict(rfModel,inraster.df, type = "p")
  out_predictions <- apply(out_predictions,1,function(x) max(x)   )
  out_predictions_array <- array(as.double((as.character(out_predictions))),dim=c(dim(inraster)[1:2],ncol(out_predictions)))
  return(out_predictions_array)
}

SVM_raster_predict_prob <- function(inraster,svmModel,...)
{
  # We need to load randomForest (note this is only
  # loaded once per worker on most cluster setups):
  require(kernlab)
  
  # First, preserve the names:
  band_names <- dimnames(inraster)[3][[1]]
  print(band_names)
  
  inraster_matrix <- inraster
  dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
  
  inraster.df <- as.data.frame(inraster_matrix[])
  
  # We need to re-set the names because randomForest requires it:
  names(inraster.df) <- band_names
  
  # Now we can use this in a predict statement:
  out_predictions <- predict(svmModel,inraster.df,type="probabilities")  
  out_predictions_array <- array(as.double((as.character(out_predictions))),dim=c(dim(inraster)[1:2],ncol(out_predictions)))
  return(out_predictions_array)
}