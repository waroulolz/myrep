library(plotly)
library(e1071)
library(caret)
source("C:/Users/GG/Desktop/Memoire/myrep/ErrorMeasures.R")
source("C:/Users/GG/Desktop/Memoire/myrep/SVM.R")


#Data extraction
volume <- read.csv(paste(dataDirectory, 'cac40_dataset_volume.CSV', sep=""), sep = "")
sigma_4 <- read.csv(paste(dataDirectory, 'cac40_6y_dataset_sigma_4.csv', sep=""), sep = "")
rv_50 <- read.csv(paste(dataDirectory, 'cac40_dataset_rv_50.csv', sep=""), sep = "")
ccreturn <- read.csv(paste(dataDirectory, 'cac40_dataset_ccreturn.csv', sep=""), sep = "")
volume$date <- as.Date(volume$Index)
sigma_4$date <- as.Date(sigma_4$Index)
rv_50$date <- as.Date(rv_50$Index)
ccreturn$date <- as.Date(ccreturn$Index)
names(volume) <- gsub("X", "", names(volume))
names(sigma_4) <- gsub("X", "", names(sigma_4))
names(rv_50) <- gsub("X", "", names(rv_50))
names(ccreturn) <- gsub("X", "", names(ccreturn))



# Computes succesive errors with a rolling origin training set
computeErrors <- function(df, horizon, gamma, cost, window){
  MAPEs <- c()
  minimumTrainingSize <- round(2*nrow(df)/3)
  for (i in seq(minimumTrainingSize, nrow(df), horizon)){
    horizon <- min(horizon, nrow(df) - i)
    if (horizon > 0){
      predictedY <- svmForecast(head(df, i), gamma, cost, horizon, window)
      MAPEs <- c(MAPEs, MAPE(df[(i-horizon+1):i, ]$target, predictedY))
    }
  }
  mean(MAPEs)
}



df <- tail(volume, 365)
df <- df[ , colSums(is.na(df)) == 0]

# For a configuration of those parameters, computes a mean error with a rolling origin

window <- 21
horizon <- 5
gamma <- 0.5
cost <- 0.5

meanErrors <- c()
for (column in names(df)[2:(ncol(df)-1)]){
  dd <- df[c("date", column)]
  names(dd) <- c("date", "target")
  meanErrors <- c(meanErrors, computeErrors(dd, horizon, gamma, cost, window))
}

mean(meanErrors)