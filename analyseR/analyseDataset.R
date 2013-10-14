#Predict closed questions on Stack Overflow

#Libraries
library(ggplot2)

#helper functions
computePriors <- function(df) {
  n = nrow(df)
  priorNotarealquestion <- nrow(df[df$OpenStatus=="not a real question",])/n
  priorNotconstructive <- nrow(df[df$OpenStatus=="not constructive",])/n
  priorOfftopic <- nrow(df[df$OpenStatus=="off topic",])/n
  priorOpen <- nrow(df[df$OpenStatus=="open",])/n
  priorToolocalized <- nrow(df[df$OpenStatus=="too localized",])/n
  return(c(priorNotarealquestion, priorNotconstructive, priorOfftopic, priorOpen, priorToolocalized))
}

multiclassLogLoss <- function(df) {
  e <- 0.000000000001
  n <- nrow(df)
  sum <- 0
  sum <- sum + sum(log(df[df$OpenStatus=="not a real question",]$predictedNotarealquestion+e))
  sum <- sum + sum(log(df[df$OpenStatus=="not constructive",]$predictedNotconstructive+e))
  sum <- sum + sum(log(df[df$OpenStatus=="off topic",]$predictedOfftopic+e))
  sum <- sum + sum(log(df[df$OpenStatus=="open",]$predictedOpen+e))
  sum <- sum + sum(log(df[df$OpenStatus=="too localized",]$predictedToolocalized+e))
  return(-(1/n)*sum)
}

savePlot <- function(filename) {
  dev.copy(png,filename)
  dev.off()
}
adjustFontSize <- theme(axis.title.x = element_text(size=22),
                        axis.text.x  = element_text(size=20)) +
                  theme(axis.title.y = element_text(size=22),
                        axis.text.y  = element_text(size=20)) +
                  theme(legend.title = element_text(size=22),
                        legend.text = element_text(size=20))
threshold <- function(x) {
  if(is.na(x)) {return(NA)}
  if(x>0.5) {return(1)}
  else      {return(0)}
}

predictionsToDataframe <- function(df) {
  df$predictedNotarealquestionBin <- sapply(df$predictedNotarealquestion, threshold)
  df$predictedNotconstructiveBin<- sapply(df$predictedNotconstructive, threshold)
  df$predictedOfftopicBin <- sapply(df$predictedOfftopic, threshold)
  df$predictedOpenBin <- sapply(df$predictedOpen, threshold)
  df$predictedToolocalizedBin <- sapply(df$predictedToolocalized, threshold)
  return(df)
}

#compute accuracy
computeAccuracy <- function(df) {
  notarealquestionAccuracy <- nrow(df[df$OpenStatus=="not a real question" & df$predictedNotarealquestionBin==1,])/
    nrow(df[df$OpenStatus=="not a real question",])
  notconstructiveAccuracy <- nrow(df[df$OpenStatus=="not constructive" & df$predictedNotconstructiveBin==1,])/
    nrow(df[df$OpenStatus=="not constructive",])
  offtopicAccuracy <- nrow(df[df$OpenStatus=="off topic" & df$predictedOfftopicBin==1,])/
    nrow(df[df$OpenStatus=="off topic",])
  openAccuracy <- nrow(df[df$OpenStatus=="open" & df$predictedOpenBin==1,])/
    nrow(df[df$OpenStatus=="open",])
  toolocalizedAccuracy <- nrow(df[df$OpenStatus=="too localized" & df$predictedToolocalizedBin==1,])/
    nrow(df[df$OpenStatus=="too localized",])
  return(c(notarealquestionAccuracy, notconstructiveAccuracy,
           offtopicAccuracy, openAccuracy, toolocalizedAccuracy))
}

computeAverageDistance <- function(df) {
  df1 <- df[df$OpenStatus=="not a real question",]
  sum1 <- sum((df1$predictedNotarealquestion - df1$Notarealquestion)^2)/nrow(df1)
  df2 <-df[df$OpenStatus=="not constructive",]
  sum2 <- sum((df2$predictedNotconstructive - df2$Notconstructive)^2)/nrow(df2)
  df3 <-df[df$OpenStatus=="off topic",]
  sum3 <- sum((df3$predictedOfftopic - df3$Offtopic)^2)/nrow(df3)
  df4 <-df[df$OpenStatus=="open",]
  sum4 <- sum((df4$predictedOpen - df4$Open)^2)/nrow(df4)
  df5 <-df[df$OpenStatus=="too localized",]
  sum5 <- sum((df5$predictedToolocalized - df5$Toolocalized)^2)/nrow(df5)
  return(c(sum1,sum2,sum3,sum4,sum5))
}

addBinaryResponse <- function(df) {
  df$Notarealquestion <- sapply(df$OpenStatus, function(x) {if(x=="not a real question"){return(1)}else{return(0)}})
  df$Notconstructive <- sapply(df$OpenStatus, function(x) {if(x=="not constructive"){return(1)}else{return(0)}})
  df$Offtopic <- sapply(df$OpenStatus, function(x) {if(x=="off topic"){return(1)}else{return(0)}})
  df$Open <- sapply(df$OpenStatus, function(x) {if(x=="open"){return(1)}else{return(0)}})
  df$Toolocalized <- sapply(df$OpenStatus, function(x) {if(x=="too localized"){return(1)}else{return(0)}})
  return(df)
}

##########################
#   Start
##########################
#first half is training data second half is test data
bigSample <- read.csv("../data/train_sample.csv")
set.seed(3009)

#reduce data
sampleSize <- nrow(bigSample)/4
trainSample <- bigSample[1:floor(sampleSize/2),]

#######################
#   Preprocess Data
######################
print("Preprocess data")
preprocessLabeledData <- function(df) {
  df$OpenStatus <- as.factor(df$OpenStatus)
  df$Title <- as.character(df$Title)
  
  addClosed <- function(df) {
    df$closed <- sapply(df$OpenStatus, function(x){return(x!="open")})
    return(df)
  }
  df <- addClosed(df)
  df$closed <- as.factor(df$closed)
  return(df)
}
trainSample <- preprocessLabeledData(trainSample)

#######################
#   Add features
#######################
print("Add features")
addFeatures <- function(df) {
  df$BodyMarkdown <- as.character(df$BodyMarkdown)
  
  #remove code in questions
  removeBoilerplate <- function(s) {
    s <- gsub("\\{.*\\}", "", s)
    s <- gsub("<.*>", "", s)
    s <- gsub("\\[.*\\]", "", s)
    s <- gsub("\\([^\\)]*?\\)", "", s)
    return(s)
  }
  print("  Remove boilerplate")
  df$BodyMarkClean <- sapply(df$BodyMarkdown, removeBoilerplate)
  
  #maxim of relevance
  countNumberTags <- function(df) {
    isEmpty <- function(x) {if(x=="") {return(0)} else {return(1)}}
    df$nTags <- sapply(df$Tag1, isEmpty) +
                sapply(df$Tag2, isEmpty) +
                sapply(df$Tag3, isEmpty) +
                sapply(df$Tag4, isEmpty) +
                sapply(df$Tag5, isEmpty)
    return(df)
  }
  print("  Count number of tags (disabled)")
  #df <- countNumberTags(df)
  
  #maxim of quantity: question length
  print("  Compute question length")
  df$nCharBody <- sapply(df$BodyMarkdown, nchar)
  df$logNCharBody <- log(df$nCharBody)
  df$nCharClean <- sapply(df$BodyMarkClean, nchar)
  df$logNCharClean <- log(df$nCharClean)
  df$nCharClean <- sapply(df$BodyMarkClean, nchar)
  invalid <- df[df$logNCharBody==-Inf,c("logNCharBody")]
  if(length(invalid)!=0){df[df$logNCharBody==-Inf,c("logNCharBody")] <- 0}
  invalid <- df[df$logNCharClean==-Inf,c("logNCharClean")]
  if(length(invalid)!=0) {df[df$logNCharClean==-Inf,c("logNCharClean")] <- 0}
  #title length
  print("  Compute title length")
  df$nCharTitle <- sapply(df$Title,nchar)
  
  
  #maxim of manner: number of commas
  print("  Compute number of commas")
  numberChars <- sapply(df$BodyMarkClean, nchar)
  numberChars <- sapply(numberChars, function(x) {if(x>0){return(x)} else{return(1)}})
  numberCommas <- sapply(df$BodyMarkClean, function(x) {return(nchar(gsub("[^,]","",x)))})
  df$commasPerChar <- numberCommas/numberChars
  df$logCommasPerChar <- log(numberCommas/numberChars)
  
  #maxim of relevance: number of stopwords
  library(tm)
  print("  Compute number of stopwords")
  corpus <- Corpus(VectorSource(df$BodyMarkdown))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #corpus <- tm_map(corpus, stripWhitespace)
  df$nStopwords <- df$nCharBody - nchar(corpus)
  
  #maxim of quality user age
  df$UserAge <- as.numeric(as.Date(df$PostCreationDate,format="%m/%d/%Y") - as.Date(df$OwnerCreationDate,format="%m/%d/%Y"))
  
  #reputation
  #df$LogReputation <- log(df$ReputationAtPostCreation)
  return(df)
}
trainSample <- addFeatures(trainSample)

##########################
#   Plot Data
##########################
#plot maxim of quantity
plotNCharBody <- function() {
  library(plyr)
  cdf <- ddply(trainSample,.(closed), summarise, logNCharBody.mean=mean(logNCharBody))
  ggplot(trainSample, aes(x=logNCharBody, fill=closed)) + geom_density(alpha=.3) +
    xlab("log(characters in question)") +
    xlim(c(4,8)) +
    geom_vline(data=cdf, aes(xintercept=logNCharBody.mean, colour=closed), linetype="dashed", size=1) +
    adjustFontSize
  savePlot('analyseDataset-nCharBody.png')
  #effect of boilerplate removal
  ggplot(trainSample, aes(x=logNCharClean, fill=closed)) + geom_density(alpha=.3) + xlim(c(3.5,9))
  ggplot(trainSample, aes(x=logNCharClean, fill=OpenStatus)) + geom_density(alpha=.3) + xlim(c(3.5,9))

  ggplot(trainSample[trainSample$OpenStatus=="open" | trainSample$OpenStatus=="not a real question",], 
         aes(x=logNCharClean, fill=OpenStatus)) + geom_density(alpha=.3) + xlim(c(3.5,9)) + 
           adjustFontSize  
  t.test(trainSample[trainSample$OpenStatus=="open",]$logNCharClean, trainSample[trainSample$OpenStatus=="not a real question",]$logNCharClean)

  jsavePlot('analyseDataset-nCharCleanOpenStatus.png')
}

plotNCharTitle <- function() {
  plot(density(trainSample$nCharTitle))
  ggplot(trainSample, aes(x=nCharTitle, fill=closed)) + geom_density(alpha=.3)
  ggplot(trainSample, aes(x=nCharTitle, fill=OpenStatus)) + geom_density(alpha=.3)
}

#plot relevance
plotNStopwords <- function() {
  trainSample$fractionStopwords <- trainSample$nStopwords/trainSample$nCharBody
  ggplot(trainSample[trainSample$OpenStatus=="open" | trainSample$OpenStatus=="too localized",], 
         aes(x=fractionStopwords, fill=OpenStatus)) + geom_density(alpha=.3) + xlim(c(0,0.4))+ 
           adjustFontSize
  t.test(trainSample[trainSample$OpenStatus=="open",]$fractionStopwords, trainSample[trainSample$OpenStatus=="too localized",]$fractionStopwords)
  savePlot('analyseDataset-fractionStopwords.png')
}
plotNTags <- function() {
  par(cex.axis=1.2, cex.lab=1.2)
  plot(as.factor(trainSample$nTags))
  plot(trainSample$closed ~ as.factor(trainSample$nTags))
  savePlot('analyseDataset-nTags.png')
}

#plot manner
#plot commas
plotCommas <- function() {
  ggplot(trainSample, aes(x=logCommasPerChar, fill=OpenStatus)) + geom_density(alpha=.3)
}

#plot reputation
plotReputation <- function() {
  #remove outliers
  trainSample <- trainSample[trainSample$ReputationAtPostCreation < 5000, ]
  ggplot(trainSample, aes(x=ReputationAtPostCreation)) + geom_density(alpha=.3, fill="salmon")  +
    adjustFontSize +
    geom_vline(aes(xintercept=mean(trainSample$ReputationAtPostCreation, na.rm=T)),   # Ignore NA values for mean
               color="salmon", linetype="dashed", size=1) +
    coord_cartesian(xlim = c(0,1000))  
  savePlot("analyseDataset-reputationDensityPlain.png")
  #load data where closed:open ratio is not 1:1 but like normal
  #veryBigSample <- read.csv("../data/bigTrain/sample.csv")
  #veryBigSample$ReputationAtPostCreation <- veryBigSample$X1
  #remove outliers
  #veryBigSampleDist <- veryBigSample[veryBigSample$ReputationAtPostCreation > 1, ]
  veryBigSampleDist <- trainSample
  ggplot(veryBigSampleDist, aes(x=ReputationAtPostCreation)) + geom_density(alpha=.3, fill="salmon")  +
              adjustFontSize +
              scale_x_log10()
  savePlot("analyseDataset-reputationDensityLog.png")
  library(plyr)
  cdf <- ddply(trainSample,.(closed), summarise, ReputationAtPostCreation.mean=mean(ReputationAtPostCreation))
  ggplot(trainSample, aes(x=ReputationAtPostCreation,fill=closed)) + geom_density(alpha=.3) + 
    adjustFontSize +
    coord_cartesian(xlim = c(0,500)) +
    geom_vline(data=cdf, aes(xintercept=ReputationAtPostCreation.mean,  colour=closed),
               linetype="dashed", size=1) 
  savePlot("analyseDataset-reputationDensity.png")
  #ggplot(trainSample, aes(x=ReputationAtPostCreation,fill=closed)) + geom_density(alpha=.3) + 
  #  adjustFontSize +
  #  xlim(c(0,500))
  ggplot(data=trainSample, aes(y=ReputationAtPostCreation,x=closed)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0,1000)) +
    adjustFontSize
  savePlot("analyseDataset-reputationBoxplot.png")
  
  ggplot(data=trainSample, aes(y=ReputationAtPostCreation,x=OpenStatus)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0,1000)) +
    adjustFontSize
    
  #trainSample$LogReputation <- log(trainSample$ReputationAtPostCreation)
  #ggplot(trainSample, aes(x=LogReputation)) + geom_density(alpha=.3) + adjustFontSize
  #ggplot(trainSample, aes(x=LogReputation, fill=closed)) + geom_density(alpha=.3) + adjustFontSize 
  
  #plot glm
  closedNumeric <- as.numeric(trainSample$closed) -rep(1,nrow(trainSample))
  par(cex=1.3)
  
  reputation.glm=glm(closed~I(log(ReputationAtPostCreation)),family=binomial,trainSample) 
  summary(reputation.glm)
    probClosed = predict(reputation.glm,type="response")
  #DecisionBoundary
  coef(reputation.glm)
  decisionBoundary = - coef(reputation.glm)[1]/coef(reputation.glm)[2]
  qplot(ReputationAtPostCreation, probClosed, data=cbind(trainSample, probClosed), size=I(3)) + geom_smooth(color="blue",size=I(0.5)) +
    geom_vline(xintercept=decisionBoundary, colour="salmon", size=1, linetype="dashed") + 
    ylab("Probability closed") +
    adjustFontSize 
  savePlot("analyseDataset-reputationRegression.png")
  newData <- bigSample[(floor(sampleSize/2)+1):sampleSize,]
  newData <- preprocessLabeledData(newData)
  newData <- addFeatures(newData)
  testModelClosed <- function(model) {
    label = newData$closed
    prediction = sapply((predict(model, type="response", newdata=newData)),threshold)
    difference = as.numeric(label) -rep(1,nrow(newData)) - prediction
    correctFraction = (1-sum(abs(difference))/length(difference))
    return(paste(c("Testdaten: ",as.character(correctFraction*100),"% korrekte Vorhersagen"),collapse = ""))
  } 
  testModelClosed(reputation.glm)
  reputationLanguage.glm=glm(closed~ReputationAtPostCreation+logNCharBody+nCharTitle, family=binomial,trainSample)
  testModelClosed(reputationLanguage.glm)
  drop1(reputationLanguage.glm)
  
  reputationUserAge.glm=glm(closed~I(log(ReputationAtPostCreation))+UserAge, family=binomial,trainSample)
  summary(reputationUserAge.glm)
  testModelClosed(reputationUserAge.glm)
  drop1(reputationLanguage.glm)
      
  
  reputationLanguageAge.glm=glm(closed~I(log(ReputationAtPostCreation))+logNCharBody+nCharTitle+nStopwords+commasPerChar, family=binomial,trainSample)
  summary(reputationLanguageAge.glm)
  testModelClosed(reputationLanguageAge.glm)
  drop1(reputationLanguageAge.glm)
  

  plot(trainSample$ReputationAtPostCreation, trainSample$UserAge)
  x <- cor(trainSample$ReputationAtPostCreation, trainSample$UserAge)
  cor(trainSample$ReputationAtPostCreation, trainSample$logNCharBody)
  abline(a=0, b=x)
  
  cdf <- ddply(trainSample[trainSample$OpenStatus!="too localized" & trainSample$OpenStatus!="off topic" ,],.(OpenStatus), summarise, ReputationAtPostCreation.mean=mean(ReputationAtPostCreation))
  ggplot(trainSample[trainSample$OpenStatus!="too localized" & trainSample$OpenStatus!="off topic" ,], 
            aes(x=ReputationAtPostCreation, fill=OpenStatus)) + 
    geom_density(alpha=.3) + adjustFontSize +
#    coord_cartesian(xlim = c(0,600)) +
              scale_x_log10()+
    geom_vline(data=cdf, aes(xintercept=ReputationAtPostCreation.mean,  colour=OpenStatus),
               linetype="dashed", size=1)
  savePlot("analyseDataset-ReputationMaximsDensity.png")
  
  reputationAll.glm <- glm(closed~OwnerUndeletedAnswerCountAtPostTime+ReputationAtPostCreation+logNCharClean
                         +commasPerChar+logNCharBody+nStopwords+nCharTitle
                         ,data=trainSample, family="binomial")
  summary(reputationAll.glm)
  drop1(reputationAll.glm,test="Chisq")
  testModel(reputationAll.glm, testSample, testSample$closed)
}

plotUndeletedAnswers <- function() {
  trainSample$logUndeletedAnswers <- log(trainSample$OwnerUndeletedAnswerCountAtPostTime)
  plot(density(trainSample$logUndeletedAnswers))
  ggplot(trainSample, 
         aes(x=logUndeletedAnswers, colour=OpenStatus)) + 
    geom_density(alpha=.3) + adjustFontSize
}
#######################
#   Feature Scaling
######################
#feature scaling
featureScaling <- function(df) {
  df$ReputationAtPostCreation <- scale(df$ReputationAtPostCreation)
  df$logNCharClean <- scale(df$logNCharClean)
  df$commasPerChar <- scale(df$commasPerChar)
  df$OwnerUndeletedAnswerCountAtPostTime <- scale(df$OwnerUndeletedAnswerCountAtPostTime)
  df$logNCharBody <- scale(df$logNCharBody)
  df$nStopwords <- scale(df$nStopwords)
  df$nCharTitle <- scale(df$nCharTitle)
  return(df)
}
trainSample <- featureScaling(trainSample)
#####################
#   Train Models
####################
print("Train Models")
priors <- computePriors(trainSample)
#glm
print("1")
library(glmnet)
trainSample.glm <- glmnet(as.matrix(trainSample[,c("ReputationAtPostCreation", "logNCharClean", 
                                                   "logNCharBody", "commasPerChar", "nStopwords",
                                                   "OwnerUndeletedAnswerCountAtPostTime",
                                                   "nCharTitle"
                                                   )]), trainSample$OpenStatus, family="multinomial")
#rpart
library(rpart)
print("1")
trainSample.rpart <- rpart(OpenStatus~OwnerUndeletedAnswerCountAtPostTime+ReputationAtPostCreation+logNCharClean
                           +commasPerChar+logNCharBody+nStopwords+nCharTitle
                           ,data=trainSample,
                           parms=list(prior=computePriors(trainSample)))
#plot(trainSample.rpart)
#par(cex=1.2)
#text(trainSample.rpart)
#savePlot("analyseDataset-plotRpart.png")
#summary(trainSample.rpart)

#random forest
library(randomForest)
print("1")
trainSample.randomForest <- randomForest(OpenStatus~OwnerUndeletedAnswerCountAtPostTime+ReputationAtPostCreation+
                                        logNCharClean+logNCharBody+commasPerChar+nStopwords+nCharTitle
                                         ,data=trainSample,
                                         classwt=computePriors(trainSample))
#trainSample.randomForest$importance
#importance(trainSample.randomForest)
#naive bayes
library(e1071)
print("1")
trainSample.naiveBayes <- naiveBayes(OpenStatus~OwnerUndeletedAnswerCountAtPostTime+ReputationAtPostCreation+
                                       logNCharClean+logNCharBody+commasPerChar+nStopwords+nCharTitle
                                     ,data=trainSample)

#summary(trainSample.naiveBayes)
print("1")
library(tree)
#r tree
trainSample.tree <- tree(OpenStatus~OwnerUndeletedAnswerCountAtPostTime+ReputationAtPostCreation +
                                      + logNCharClean+logNCharBody+commasPerChar+nStopwords+nCharTitle
                                     ,data=trainSample)
#text(trainSample.tree)
#plot(trainSample.tree)
#summary(trainSample.tree)

######################
#   Evaluate Models
######################
print("Evaluate Models")
#predict values from df and append them
appendToDataframe <- function(df, df.predicted) {
  #make nice column names
  colnames <- colnames(df.predicted)
  colnames <- gsub(" ","", colnames , fixed=TRUE)
  colnames <- gsub("\\.\\d$","", colnames, fixed=FALSE)
  df.predicted <- as.data.frame(df.predicted)
  colnames(df.predicted) <- colnames
  df$predictedNotarealquestion <- df.predicted[,c("notarealquestion")]
  df$predictedNotconstructive<- df.predicted[,c("notconstructive")]
  df$predictedOfftopic <- df.predicted[,c("offtopic")]
  df$predictedOpen <- df.predicted[,c("open")]
  df$predictedToolocalized <- df.predicted[,c("toolocalized")]
  return(df)
}
#test on non-trained data
testSample <- bigSample[(floor(sampleSize/2)+1):sampleSize,]
testSample <- preprocessLabeledData(testSample)
testSample <- addFeatures(testSample)
testSample <- featureScaling(testSample)
testSample.predictedGLM <- predict(trainSample.glm, newx=as.matrix(testSample[c("ReputationAtPostCreation", "logNCharClean", 
                                                    "commasPerChar", "logNCharBody", "nStopwords",
                                                    "OwnerUndeletedAnswerCountAtPostTime", "nCharTitle"
                                                    )]), type="response", s=0.01)
testSample <- appendToDataframe(testSample, testSample.predictedGLM)
logLoss.glm <- multiclassLogLoss(testSample)
print(paste("GLM on test data set: ", logLoss.glm))

testSample.predictForest <- predict(trainSample.randomForest, newdata=testSample, type='prob')
testSample <- appendToDataframe(testSample, testSample.predictForest)
logLoss.randomForest <- multiclassLogLoss(testSample)
print(paste("Random forest on test data set: ", logLoss.randomForest))

testSample.predictRpart <- predict(trainSample.rpart, testSample)
testSample <- appendToDataframe(testSample, testSample.predictRpart)
logLoss.rpart <- multiclassLogLoss(testSample)
print(paste("Rpart on test data set: ", logLoss.rpart))

testSample.predictNaiveBayes <- predict(trainSample.naiveBayes, newdata=testSample, type="raw")
testSample <- appendToDataframe(testSample, testSample.predictNaiveBayes)
logLoss.naiveBayes <- multiclassLogLoss(testSample)
print(paste("Naive Bayes on test data set: ", logLoss.naiveBayes))

testSample.predictTree <- predict(trainSample.tree, newdata=testSample)
testSample <- appendToDataframe(testSample, testSample.predictTree)
logLoss.tree <- multiclassLogLoss(testSample)
print(paste("Tree on test data set: ", logLoss.tree))

############################
#apply to leaderboard data
###########################
applyToLeaderboard <- function() {
  print("Apply model to leaderboard data")
  leaderboardSample <- read.csv("../data/private_leaderboard.csv")
  leaderboardSample <- addFeatures(leaderboardSample)
  leaderboardSample <- featureScaling(leaderboardSample)
  pred.randomForest <- predict(trainSample.randomForest, newdata=leaderboardSample, type='prob')
  pred.rpart <- predict(trainSample.rpart, leaderboardSample)
  pred.glm <- predict(trainSample.glm, newx=as.matrix(leaderboardSample[c("ReputationAtPostCreation", "logNCharClean", 
                                                                          "commasPerChar", "logNCharBody", "nStopwords",
                                                                          "OwnerUndeletedAnswerCountAtPostTime", "nCharTitle"
  )]), type="response", s=0.01)
  #set na to 0
  pred[is.na(pred)] <- 0
  write.table(pred.glm, file="predictionGLM.csv", sep=",", row.names=FALSE,col.names=FALSE)
  write.table(pred.rpart, file="predictionRpart.csv", sep=",", row.names=FALSE,col.names=FALSE)
  write.table(pred.randomForest, file="predictionRandomForest.csv", sep=",", row.names=FALSE,col.names=FALSE)
}
#uniform benchmark: 1.6
#my best score: 0.76, rank 43
#prior benchmark: 0.47
#basic benchmark: 0.46





