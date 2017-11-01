setwd("C://Users/nikla/Google Drive/Data Science & Marketing Analytics/Assignment 2")  #set working directory

Wehkampdata <- read.csv("Wehkampdata.csv",header=TRUE) #read wehkamp file

Wehkampdata$age <- 2017 - Wehkampdata$birthyear
Wehkampdata$accountlength <- 2017 - Wehkampdata$startyear

?`=`
x <- Wehkampdata[sample(1:nrow(Wehkampdata), nrow(Wehkampdata), replace = F),]
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]


#makeLiftPlot
makeLiftPlot <- function(Prediction, Evaluate, ModelName)
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
  CustomersSorted <- Evaluate$SaleString[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumSaleReal<- sum(Evaluate$SaleString == "Sale") #total number of real sales in the evaluation set
  CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
  SaleCumulative=apply(matrix(CustomersSorted=="Sale"),2,cumsum)/SumSaleReal #cumulative fraction of sales
  ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="Sale")/floor(nrow(Evaluate)*.1) #probability of sale in 1st decile
  ProbOverall = SumSaleReal / nrow(Evaluate) #overall sale probability
  TDL = ProbTD / ProbOverall
  GINI = sum((SaleCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
  plot(CustomerCumulative,SaleCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of customers (sorted by predicted sale probability)",ylab="Cumulative fraction of sales")
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
  
  
  #BaseFormula
  BaseFormula <- as.formula(SaleString ~ views + carts + abandon + remove + gender + segment + numbersearches + avgprice + temp + suns + timerain + totalrain + cloud + age + accountlength)
  BaseFormula1 <- as.formula(Sale ~ views + carts + abandon + remove + gender + segment + numbersearches + avgprice + temp + suns + timerain + totalrain + cloud + age + accountlength)
  
  #LOGIT
  Sale_logit <- glm(BaseFormula , data = x.train, family = "binomial")
  summary(Sale_logit)
  x.evaluate$predictionlogit <- predict(Sale_logit, newdata=x.evaluate, type = "response")
  x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>.5] <- "Sale"
  x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=.5] <- "NoSale"
  x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$SaleString
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))
  LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")
  LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100

  #SVM 
  install.packages("e1071")
  library(e1071)
  Sale_SVM <- svm(BaseFormula , data = x.train, probability=T)
  summary(Sale_SVM)
  x.evaluate$predictionSVM <- predict(Sale_SVM, newdata=x.evaluate, probability = T)
  x.evaluate$correctSVM <- x.evaluate$predictionSVM == x.evaluate$SaleString
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))
  # Extract the class probabilities.
  x.evaluate$probabilitiesSVM <- attr(x.evaluate$predictionSVM,"probabilities")[,2]
  SVMOutput <- makeLiftPlot(x.evaluate$probabilitiesSVM,x.evaluate,"SVM")
  
  #BAGGING aka. bootstrap aggregation
  install.packages("adabag")
  install.packages("party")
  library(adabag)
  library(party)
  x.modelBagging  <- bagging(BaseFormula, data=x.train, control = cforest_unbiased(mtry = 3))
  
  # Use the model to predict the evaluation.
  x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluate)$class
  # Calculate the overall accuracy.
  x.evaluate$correctBagging <- x.evaluate$predictionBagging == x.evaluate$SaleString
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))
  # Extract the class probabilities.
  x.evaluate$probabilitiesBagging <- predict(x.modelBagging,newdata=x.evaluate)$prob[,1]
  BaggingOutput <- makeLiftPlot(x.evaluate$probabilitiesBagging,x.evaluate,"Bagging")
  
  #RANDOM FOREST -> Creates a model using random forest and bagging ensemble algorithms
  install.packages("randomForest")
  library(randomForest)
  x.modelRF <- randomForest(BaseFormula, data=x.train, control = cforest_unbiased(mtry = 3))
  
  # Use the model to predict the evaluation.
  x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluate, type = "response")
  # Calculate the overall accuracy.
  x.evaluate$correctRF <- x.evaluate$predictionRF == x.evaluate$SaleString
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))
  # Extract the class probabilities.
  x.evaluate$probabilitiesRF <- predict(x.modelRF,newdata=x.evaluate,type="prob")[,1]
  RFOutput <- makeLiftPlot(x.evaluate$probabilitiesRF,x.evaluate,"Random Forest")
  
  Imputeddata$SaleString=relevel(Imputeddata$SaleString,ref="Sale")
  
  
  GCtorture(TRUE)  #testaa
  
  #Neural Network
  install.packages("nnet")
  install.packages("caret")
  library(nnet)
  library(caret)
  x.modelNNet <- train(BaseFormula, data=x.train, method='nnet', trControl=trainControl(method='cv'))
  x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluate, type="raw")
  x.evaluate$correctNNet <- x.evaluate$predictionNNet == x.evaluate$SaleString
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))
  #library(devtools)
  install.packages("devtools")
  library(devtools)
  #source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
  #plot.nnet(x.modelNNet)
  x.evaluate$probabilitiesNNet <- predict(x.modelNNet, newdata = x.evaluate, type='prob')[,1]
  NNetOutput <- makeLiftPlot(x.evaluate$probabilitiesNNet,x.evaluate,"Neural Network")
  makeLiftPlot(x.evaluate$probabilitiesNNet,x.evaluate,"Neural Network")
  
  install.packages("rpart")
  install.packages("mlbench")
  install.packages("adabag")
  install.packages("party")
  library(rpart)
  library(mlbench)
  library(adabag)
  library(party)
  #Boosting -> Create a model using boosting ensemble algorithms
  x.modelBoosting  <- boosting(BaseFormula, data=x.train, boos = TRUE, mfinal = 100, control = cforest_unbiased(mtry = 3))
  # Use the model to predict the evaluation.
  x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluate)$class
  # Calculate the overall accuracy.
  x.evaluate$correctBoosting <- x.evaluate$predictionBoosting == x.evaluate$SaleString
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))
  # Extract the class probabilities.
  x.evaluate$probabilitiesBoosting <- predict(x.modelBoosting,newdata=x.evaluate)$prob[,1]
  # Make a lift curve
  BoostingOutput <- makeLiftPlot(x.evaluate$probabilitiesBoosting,x.evaluate,"Boosting")
  makeLiftPlot(x.evaluate$probabilitiesBoosting,x.evaluate,"Boosting")
  
  install.packages("ggplot2")
  library(ggplot2)
  
  install.packages("party")
  install.packages("ROCR")
  install.packages("adabag")
  library(party)
library(ROCR)  
  library(adabag)
  require(rpart)
  require(ggplot2)

  
  makeLiftPlot(x.evaluate$probabilitiesBoosting,x.evaluate,"Boosting")


  