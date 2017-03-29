library(gbcode) ## install_github("gbonte/gbcode")
library(randomForest)
library(e1071)
library(forecast)
library(lazy)
library(pls)


#' Function to select the best K-NN hyperparameter given a certain embedding employing the recursive multistep forecasting strategy
#'
#' @param ts - Original time series
#' @param horizon - Forecasting horizon
#' @param splitting_point - Splitting point between testing and training set
#' @param embedding - Embedding of the time series
#' @param min_C - Minimum value of the hyperparameter C to test
#' @param max_C - Maximum value of the hyperparameter C to test
#' @param error_measure - Error measure to use to compare the different forecasts
#'
#' @return List [Best Prediction, Best value error measure]
#' @export
#'
#' @examples
modelSelectionKNN <- function(ts,horizon,splitting_point,embedding=3,Kmin=5,min_C=3,max_C=10,type=c("recursive","direct","mimo"),error_measure){
  
  # Check whether the parameter type is included within the allowed values
  type <- match.arg(type)

  forecasts.matrix <- NULL
  
  ts.future <- ts[splitting_point:(splitting_point+horizon-1)]
  # Looping to select the best number of neighbors of the NN 
  for (C in min_C:max_C) {
    switch(type,
           recursive={ 
             forecasts.matrix <- rbind(forecasts.matrix,multiplestepAhead(ts,embedding,horizon,method = "iter"))
           },

           direct={ 
             forecasts.matrix <- rbind(forecasts.matrix,multiplestepAhead(ts,embedding,horizon,method = "direct"))
           },
           mimo={ 
             forecasts.matrix <- rbind(forecasts.matrix,multiplestepAhead(ts,embedding,horizon,D,"mimo",Kmin,C,FF))
           }
    )
  }
  
  #browser()
  # Compare the forecast to determine the optimal model
  error_measures.vector <- apply(forecasts.matrix,1,function(forecast){error_measure(ts.future,forecast)})
  best_forecasts <- forecasts.matrix[which.min(error_measures.vector),]
  
  return(list(forecasts=best_forecasts,min_error=min(error_measures.vector)))
}
