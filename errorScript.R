library(plotly)
library(e1071)
library(caret)
library("quantmod")
source("C:/Users/GG/Desktop/Memoire/myrep/ErrorMeasures.R")
source("C:/Users/GG/Desktop/Memoire/myrep/KNN.R")
source("C:/Users/GG/Desktop/Memoire/myrep/SVM.R")


#Data extraction
extractDf <- function(data){
  data <- data.frame(Date = index(data), data = coredata(data))
  data[ncol(data)] <- NULL
  names(data) <- c("date", "openingPrice", "highPrice", "lowPrice", "lastPrice", "volume")
  data
}


creturn <- function(data){
  cr <- c(0, diff(log(data)))
  cr[(is.infinite(cr)) | is.na(cr)] <- 0
  cr
}


volatilitySigma4 <- function(df){
  u <- log(df$highPrice/df$openingPrice)
  d <- log(df$lowPrice /df$openingPrice)
  c <- log(df$lastPrice/df$openingPrice)
  df$target <- 0.511 * (u - d)**2 - 0.019*(c * (u + d) - 2 * u * d) - 0.383 * c**2
  df
}

volatilitySdAverage <- function(df){
  vol <- apply(embed(df$lastPrice, 21), MARGIN = 1, FUN = sd)
  df <- tail(head(df, -10), -10)
  df$target <- vol
  df
}



naiveForecast <- function(df, horizon){
  # Moving average forecast - Average of the last data according to a certain embedding e
  e <- 30
  rep(mean(tail(head(df, -horizon)$target, e)), horizon)
}

# Computes succesive errors with a rolling origin training set
computeErrors <- function(df, horizon, window, gamma, cost){
  MSEs <- c()
  MASEs <- c()
  minTrainSize <- round(2*nrow(df)/3)
  for (i in seq(minTrainSize, nrow(df), max(horizon, (nrow(df) - minTrainSize)/10))){
    horizon <- min(horizon, nrow(df) - i)
    if (horizon > 0){
      
      # Normalise data, calculate mean and sd only on training set
      meanTarget <- mean(head(head(df, i), -horizon)$target) 
      sdTarget <- sd(head(head(df, i), -horizon)$target)
      df$target <- (df$target - meanTarget) / sdTarget
      
      predictedY <- svmForecast(head(df, i), horizon, window, gamma, cost)
      
      #Unnormalise data
      predictedY  <- (predictedY * sdTarget) + meanTarget
      df$target   <- (df$target  * sdTarget) + meanTarget 
      
      MSEs <- c(MSEs, MSE(df[(i-horizon+1):i, ]$target, predictedY))
      MASEs <- c(MASEs, MASE(df[(i-horizon+1):i, ]$target, predictedY))
    }
  }
  browser()
  c(mean(MSEs), mean(MASEs))
}


# Computes succesive errors with a rolling origin training set
computeErrorsNaive <- function(df, horizon, window){
  MSEs <- c()
  MASEs <- c()
  minTrainSize <- round(2*nrow(df)/3)
  for (i in seq(minTrainSize, nrow(df), max(horizon, (nrow(df) - minTrainSize)/10))){
    horizon <- min(horizon, nrow(df) - i)
    if (horizon > 0){
      
      # Normalise data, calculate mean and sd only on training set
      meanTarget <- mean(head(head(df, i), -horizon)$target) 
      sdTarget <- sd(head(head(df, i), -horizon)$target)
      df$target <- (df$target - meanTarget) / sdTarget

      predictedY <- naiveForecast(head(df, i), horizon)

      #Unnormalise data
      predictedY  <- (predictedY * sdTarget) + meanTarget
      df$target   <- (df$target  * sdTarget) + meanTarget 
      
      MSEs <- c(MSEs, MSE(df[(i-horizon+1):i, ]$target, predictedY))
      MASEs <- c(MASEs, MASE(df[(i-horizon+1):i, ]$target, predictedY))
    }
  }
  browser()
  c(mean(MSEs), mean(MASEs))
}


# Computes succesive errors with a rolling origin training set
computeErrorsKNN <- function(df, horizon, window, C){
  MSEs <- c()
  MASEs <- c()
  minTrainSize <- round(2*nrow(df)/3)
  for (i in seq(minTrainSize, nrow(df), max(horizon, (nrow(df) - minTrainSize)/10))){
    horizon <- min(horizon, nrow(df) - i)
    if (horizon > 0){
      
      # Normalise data, calculate mean and sd only on training set
      meanTarget <- mean(head(head(df, i), -horizon)$target) 
      sdTarget <- sd(head(head(df, i), -horizon)$target)
      df$target <- (df$target - meanTarget) / sdTarget
                                                                                                                  # Range in Rule of thumb
      res <- modelSelectionKNN(head(df, i)$target, horizon, i - horizon, window, type = "recursive", min_C=C,max_C=C, error_measure = MASE)
      predictedY <- res$forecasts
      #print(res$best_k)
      #Unnormalise data
      predictedY  <- (predictedY * sdTarget) + meanTarget
      df$target   <- (df$target  * sdTarget) + meanTarget 
      
      MSEs <- c(MSEs, MSE(df[(i-horizon+1):i, ]$target, predictedY))
      MASEs <- c(MASEs, MASE(df[(i-horizon+1):i, ]$target, predictedY))
    }
  }
  c(mean(MSEs), mean(MASEs))
}

benchmarkKnn <- function(Cs) {
  MSEBench  <- matrix(0, ncol = length(Cs), nrow = 1, dimnames = list("err", Cs))
  MASEBench <- matrix(0, ncol = length(Cs), nrow = 1, dimnames = list("err", Cs))
  for (C in Cs){
    errors <- computeErrorsKNN(dd, horizon, window, C)
    MSEBench[as.character("err"), as.character(C)] <- errors[[1]]
    MASEBench[as.character("err"), as.character(C)] <- errors[[2]]
    
  }
  list(MSEBench, MASEBench)
}

results2 <- benchmarkKnn(c(1:100))
results2


benchmark <- function(gammas, costs) {
  MSEBench <- matrix(0, ncol = length(gammas), nrow = length(costs), dimnames = list(costs, gammas))
  MASEBench <- matrix(0, ncol = length(gammas), nrow = length(costs), dimnames = list(costs, gammas))
  for (gamma in gammas){
    for (cost in costs){
      errors <- computeErrors(dd, horizon, window, gamma, cost)
      MSEBench[as.character(cost), as.character(gamma)] <- errors[[1]]
      MASEBench[as.character(cost), as.character(gamma)] <- errors[[2]]
    }
  }
  list(MSEBench, MASEBench)
}




cac <- getSymbols("^FCHI", src="yahoo", to = Sys.Date(), auto.assign = FALSE) 
sp <- getSymbols("^GSPC", src="yahoo", to = Sys.Date(), auto.assign = FALSE) 

cac <- extractDf(cac)
sp <- extractDf(sp)



# Below the modifiable parameters to perform a grid search benchmark.
window <- 21
horizon <- 7


df <- cac
df <- volatilitySigma4(df)

dd <- df[c("date", "target")]
dd <- tail(dd, 6*30)



results3 <- computeErrorsNaive(dd, horizon, window)
results3


costs  <- 2^(4:4)
gammas <- 2^(-5:-5)
results <- benchmark(gammas, costs)
paramBench <- results[[1]] # 1 is MSE, 2 is MASE
bestC <-  costs[which(paramBench == min(paramBench), arr.ind = TRUE)[1]]
bestG <- gammas[which(paramBench == min(paramBench), arr.ind = TRUE)[2]]
paramBench
bestC
bestG
#, colors = "RdGy"
plot_ly(z = paramBench,  type = "heatmap", y = rownames(paramBench), x = colnames(paramBench))%>%
  layout(xaxis = list(title = "Gamma", categoryorder = "array", categoryarray = colnames(paramBench)),
         yaxis = list(title = "Cost",  categoryorder = "array", categoryarray = rownames(paramBench)))
plot_ly(dd, x = ~date, y = ~target, type = 'scatter', mode = 'lines')

#Export to latex table
rownames(paramBench) <- paste(rep(2,11), (-3:8), sep = "^")
colnames(paramBench) <- paste(rep(2,11), (-8:3), sep = "^")
print(xtable(paramBench)) #,digits=-3
