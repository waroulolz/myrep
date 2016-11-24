#' check_equal_length_pair
#'
#' @param y - numeric vector
#' @param y_hat - numeric vector
#'
#' @return An error in case of length mismatch, nothing otherwise
#'
#' @examples
check_equal_length_pair <- function(y,y_hat){
  if(length(y) != length(y_hat)){
    stop("Mismatch on the length of forecast and real values vectors")
  }
}

#' check_equal_length_trio
#'
#' @param y - numeric vector
#' @param y_hat - numeric vector
#' @param y_hat_bench - numeric vector
#'
#' @return An error in case of any length mismatch among the three vectors, nothing otherwise
#'
#' @examples
check_equal_length_trio <- function(y,y_hat,y_hat_bench){
  if( (length(y) != length(y_hat)) | (length(y) != length(y_hat_bench)) | (length(y_hat) != length(y_hat_bench)) ){
    stop("Mismatch on the length vectors")
  }
}

################### Scale independant ########################

#' MAPE - Mean Absolute Percentage Error
#' \textbf{MAPE} : $\frac{1}{n} \sum_{t=0}^n  \mid 100 \cdot \frac{y_t - \hat{y}_t}{y_t}\mid$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MAPE value
#' @examples
MAPE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return((1/length(y))*sum(abs(100*(y-y_hat)/y)))
}

#' MdAPE - Median Absolute Percentage Error
#' \textbf{MdAPE} : $Md_{t \in \{1 \cdots n\}}(\mid 100 \cdot \frac{y_t - \hat{y}_t}{y_t}\mid)$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MdAPE value
#' @examples
MdAPE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return(median(100*(y-y_hat)/y))
}

#' RMSPE - Root Mean Squared Percentage Error
#' \textbf{RMSPE} : $\sqrt{\frac{1}{n} \sum_{t=0}^n (100 \cdot \frac{y_t - \hat{y}_t}{y_t})^2}$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return RMSPE value
#' @examples
RMSPE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return(sqrt((1/length(y))*sum(100*(((y-y_hat)/y)^2))))
}

#' RMdSPE - Root Median Squared Percentage Error
#' \textbf{RMdSPE} : $\sqrt{ Md_{t \in \{1 \cdots n\}}((100 \cdot \frac{y_t - \hat{y}_t}{y_t})^2)}$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return RMdSPE value
#' @examples
RMdSPE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return(sqrt(median(100*(((y-y_hat)/y)^2))))
}

#' sMAPE - Scaled Mean Absolute Percentage Error
#' \textbf{sMAPE} : $\frac{100}{n} \sum_{t=0}^n \cdot \frac{\mid y_t - \hat{y}_t\mid}{\frac{y_t+\hat{y}_t}{2}}$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return sMAPE value
#' @examples
sMAPE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return((200/length(y))*sum((abs(y-y_hat)/(y+y_hat))))
}


#' sMdAPE - Scaled Median Absolute Percentage Error
#' \textbf{sMdAPE} : $Md_{t \in \{1 \cdots n\}}(200 \cdot \frac{\mid y_t - \hat{y}_t\mid}{y_t+\hat{y}_t})$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return sMdAPE value
#' @examples
sMdAPE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return(median(200*((abs(y-y_hat)/(y+y_hat)))))
}

################### Scale dependant ########################

#' MSE - Mean Squared Error
#' \textbf{MSE} : $ \frac{1}{n} \sum_{t=0}^n (y_t - \hat{y}_t)^2$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MSE value
#' @examples
MSE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return((1/length(y))*sum((y-y_hat)^2))
}

#' RMSE - Root Mean Squared Error
#' \textbf{RMSE} : $\sqrt{ \frac{1}{n} \sum_{t=0}^n (y_t - \hat{y}_t)^2}$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MSE value
#' @examples
RMSE <- function(y,y_hat){
  return(sqrt(MSE(y,y_hat)))
}

#' MAE - Mean Absolute Error
#' \textbf{MAE} : $\frac{1}{n} \sum_{t=0}^n |y_t - \hat{y}_t|$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MSE value
#' @examples
MAE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return((1/length(y))*sum(abs(y-y_hat)))
}

#' MdAE - Median Absolute Error
#' \textbf{MdAE} : $Md_{t \in \{1 \cdots n\}}(|y_t - \hat{y}_t|)$
#' Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MSE value
#' @examples
MdAE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return(median(abs(y-y_hat)))
}


################### Relative Errors ########################
#' RE - Relative Error
#' \textbf{RE} : $\frac{\mathbf{y}-\hat{\mathbf{y}}}{\mathbf{y}-\hat{\mathbf{y}_b}}$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_bench - Forecasted values of the time series using the benchmark model
#'
#' @return RE value
#' @examples
RE <- function(y,y_hat,y_hat_bench){
  check_equal_length_trio(y,y_hat,y_hat_bench)
  return((y-y_hat) / (y-y_hat_bench))
}

#' MRAE - Mean Relative Absolute Error
#' \textbf{MRAE} : $\frac{1}{n} \sum_{t=0}^n  \mid r_t \mid$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_bench - Forecasted values of the time series using the benchmark model
#'
#' @return MRAE value
#' @examples
MRAE <- function(y,y_hat,y_hat_bench){
  return((1/length(y))*sum(abs(RE(y,y_hat,y_hat_bench))))
}


#' MdRAE - Median Relative Absolute Error
#' \textbf{MdRAE} : $Md_{t \in \{1 \cdots n\}}(\mid r_t \mid)$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_bench - Forecasted values of the time series using the benchmark model
#'
#' @return MdRAE value
#' @examples
MdRAE <- function(y,y_hat,y_hat_bench){
  return(median(abs(RE(y,y_hat,y_hat_bench))))
}


#' GMRAE - Geometric Mean Relative Absolute Error
#' \textbf{GMRAE} : $\sqrt[n]{\frac{1}{n} \prod{t=0}^n  \mid r_t \mid}$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_bench - Forecasted values of the time series using the benchmark model
#'
#' @return RE value
#' @examples
GMRAE <- function(y,y_hat,y_hat_bench){
  return(((1/length(y))*prod(abs(RE(y,y_hat,y_hat_bench))))^(1/length(y)))
}


#' MASE - Mean Absolute Scaled Error
#' \textbf{MASE} : $\frac{1}{T}\sum_{t=1}^T \left( \frac{\left| e_t \right|}{\frac{1}{T-1}\sum_{i=2}^T \left| Y_i-Y_{i-1} \right|}\right)$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MASE value
#' @examples
MASE <- function(y,y_hat){
  check_equal_length_pair(y,y_hat)
  return(((length(y)-1)/length(y))*(1/sum(abs(diff(y))))*sum(abs(y-y_hat)))
}


#' NMSE - Normalized Mean Squared Error
#' \textbf{NMSE:} $\frac{1}{n}\frac{\sum_{t=0}^{n} (y_t-\hat{y}_t)^2}{var(y_t)} $
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return NMSE value
#' @examples
NMSE <- function(y,y_hat){
  if( length(y) == 1 || length(y_hat) == 1 ){
    stop("NMSE undefined for horizon 1")
  }
  return(MSE(y,y_hat)/var(y))
}

#' NNMSE - Normalized Naive Mean Squared Error
#' \textbf{NMSE:} $\frac{1}{n}\frac{\sum_{t=0}^{n} (y_t-\hat{y}_t)^2}{\sum_{t=0}^{n} (y_t-\hat{y}_{Naive,t})^2} $
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_naive - Forecasted values of the time series with the naive method
#'
#' @return NNMSE value
#' @examples
NNMSE <- function(y,y_hat,y_hat_naive){
  return(MSE(y,y_hat)/MSE(y,y_hat_naive))
}

################### Relative Measures ########################

# \textbf{RelX} : $\frac{X}{X_{\text{bench}}}$
# \textbf{Percent Better} : $PB(X) = 100 \cdot \frac{1}{n} \sum_{\text{forecasts}} I(X<X_{b})$

# Currently not implemented  