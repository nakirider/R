rm(list = ls()) #clear workspace

setwd("C://Users//nikla//Google Drive//Data Science & Marketing Analytics//Assignment 2") #set working directory
SessionLevel_data <- read.csv("SessionleveldataNEW.csv",header=TRUE, na.strings="\\N") #read csv file


SessionLevel_data$livedate[is.na(SessionLevel_data$livedate)] <- min(as.character(SessionLevel_data$livedate[!is.na(SessionLevel_data$livedate)])) #replace NAs in livedate with the earliest date in livedate
SessionLevel_data$livedate <- as.Date(SessionLevel_data$livedate)

SessionLevel_data$date <- as.Date(SessionLevel_data$date)
SessionLevel_data$lastorderdate <- as.Date(SessionLevel_data$lastorderdate)
SessionLevel_data$daysdiff <- SessionLevel_data$date - SessionLevel_data$lastorderdate
SessionLevel_data$daysdiff <- as.integer(SessionLevel_data$daysdiff)

SessionLevel_data$SaleString[SessionLevel_data$sale > 0]="Sale"
SessionLevel_data$SaleString[SessionLevel_data$sale == 0]="NoSale"
SessionLevel_data$SaleString <- as.factor(SessionLevel_data$SaleString)

SessionLevel_data1 <- subset(SessionLevel_data,select = -c(lastorderdate)) # removed this because MICE does not like imputing factors with more than 50 levels, and last order date is incorporated by the variable datediff

library(mice)

#inspect pattern of missings
md.pattern(SessionLevel_data1)

#Below, the predictormatrix is specified.
#It is a square matrix of size ncol(data) containing 0/1 data specifying the set of predictors to be used for each target column. 
#Rows correspond to target variables (i.e. variables to be imputed), in the sequence as they appear in data. 
#A value of '1' means that the column variable is used as a predictor for the target variable (in the rows). 
#The diagonal of predictorMatrix must be zero.
predictorMatrix <- matrix(0,nrow = ncol(SessionLevel_data1), ncol = ncol(SessionLevel_data1)) # Make a matrix of zeros
predictorMatrix[1:8,9:12] <- 1 #variables 9:12 can be explained by variables 1:8
predictorMatrix[1:8,30] <- 1 #variable 30 can be explained by variables 1:8
predictorMatrix[14:31,9:12] <- 1 #variables 9:12 can be explained by variables 14:31
predictorMatrix[14:31,30] <- 1 #variable 30 can be explained by variables 14:31
diag(predictorMatrix) <- 0 #diagonal must be zero

#impute data
SessionLevel_data_imputed <- mice(SessionLevel_data1, predictorMatrix = predictorMatrix, m=2, maxit = 50, seed = 500)
summary(SessionLevel_data_imputed)
 
#get one of the complete data sets ( 2nd out of 5)
SessionLevel_data_complete_data <- complete(SessionLevel_data_imputed,2)

#the complete data sets can be used to estimate your model of choice
#and the results of all 5 models can be combined as in the earlier example

write.csv(SessionLevel_data_imputed, imputeddata.csv)
