#helper functions
savePlot <- function(filename) {
  #dev.copy(png,filename)
  #dev.off()
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

multiclassLogLoss <- function(df) {
  e <- 0.0000001
  n <- nrow(df)
  sum <- 0
  sum <- sum + sum(log(df[df$OpenStatus=="not a real question",]$predictedNotarealquestion+e))
  sum <- sum + sum(log(df[df$OpenStatus=="not constructive",]$predictedNotconstructive+e))
  sum <- sum + sum(log(df[df$OpenStatus=="off topic",]$predictedOfftopic+e))
  sum <- sum + sum(log(df[df$OpenStatus=="open",]$predictedOpen+e))
  sum <- sum + sum(log(df[df$OpenStatus=="too localized",]$predictedToolocalized+e))
  return(-(1/n)*sum)
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
library(ggplot2)
bigTrainSample <- read.csv("smallTrainSample.csv")
set.seed(3006)

#reduce data
sampleSize <- 8000
trainSample <- bigTrainSample[sample(nrow(bigTrainSample), sampleSize),]

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
  df <- countNumberTags(df)
  
  #maxim of quantity: question length
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
  df$nCharTitle <- sapply(df$Title,nchar)
  
  #maxim of manner: number of commas
  numberChars <- sapply(df$BodyMarkClean, nchar)
  numberChars <- sapply(numberChars, function(x) {if(x>0){return(x)} else{return(1)}})
  numberCommas <- sapply(df$BodyMarkClean, function(x) {return(nchar(gsub("[^,]","",x)))})
  df$commasPerChar <- numberCommas/numberChars
  df$logCommasPerChar <- log(numberCommas/numberChars)
  
  #maxim of relevance: number of stopwords
  library(tm)
  corpus <- Corpus(VectorSource(df$BodyMarkdown))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #corpus <- tm_map(corpus, stripWhitespace)
  df$nStopwords <- df$nCharBody - nchar(corpus)
  
  #reputation
  df$LogReputation <- log(df$ReputationAtPostCreation)
  return(df)
}
trainSample <- addFeatures(trainSample)

##########################
#   Plot Data
##########################
#plot maxim of quantity
#library(plyr)
#cdf <- ddply(trainSample,.(closed), summarise, logNCharBody.mean=mean(logNCharBody))
#ggplot(trainSample, aes(x=logNCharBody, fill=closed)) + geom_density(alpha=.3) +
#  xlab("log(characters in question)") +
#  xlim(c(4,8)) +
#  geom_vline(data=cdf, aes(xintercept=logNCharBody.mean, colour=closed), linetype="dashed", size=1) +
#  adjustFontSize
#savePlot('analyseDataset-nCharBody.png')
#effect of boilerplate removal
#ggplot(trainSample, aes(x=logNCharClean, fill=closed)) + geom_density(alpha=.3) + xlim(c(3.5,9))
#ggplot(trainSample, aes(x=nCharBodyClean, nCharBody, colour=closed)) + geom_point()

#plot commas
#ggplot(trainSample, aes(x=logCommasPerChar, fill=closed)) + geom_density(alpha=.3)

#plot relevance
#ggplot(trainSample, aes(x=nStopwords, fill=closed)) + geom_density(alpha=.3)

#plot reputation
#remove outliers
#trainSamplePlot <- trainSample[trainSample$ReputationAtPostCreation < 5000, ]
#ggplot(trainSamplePlot, aes(x=LogReputation, fill=closed)) + geom_density(alpha=.3) + adjustFontSize
#savePlot("analyseDataset-ReputationDensity.png")

#ggplot(trainSamplePlot[trainSamplePlot$OpenStatus!="open" & trainSamplePlot$OpenStatus!="off topic",], aes(x=LogReputation, fill=OpenStatus)) + 
#  geom_density(alpha=.3) + adjustFontSize
#savePlot("analyseDataset-ReputationMaximsDensity.png")

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
  df$nTags <- scale(df$nTags)
  df$nCharTitle <- scale(df$nCharTitle)
  return(df)
}
trainSample <- featureScaling(trainSample)
#####################
#   Train Models
####################
print("Train Models")
#glm
library(glmnet)
trainSample.glm <- glmnet(as.matrix(trainSample[,c("ReputationAtPostCreation", "logNCharClean", 
                                                   "logNCharBody", "commasPerChar", "nStopwords",
                                                   "OwnerUndeletedAnswerCountAtPostTime",
                                                   "nCharTitle", "nTags")]), trainSample$OpenStatus, family="multinomial")
#rpart
library(rpart)
trainSample.rpart <- rpart(OpenStatus~OwnerUndeletedAnswerCountAtPostTime+ReputationAtPostCreation+logNCharClean
                           +commasPerChar+logNCharBody+nStopwords+nCharTitle+nTags,
                           data=trainSample)
plot(trainSample.rpart)
text(trainSample.rpart)

#random forest
library(randomForest)
trainSample.randomForest <- randomForest(OpenStatus~OwnerUndeletedAnswerCountAtPostTime+ReputationAtPostCreation+
                                        logNCharClean+logNCharBody+commasPerChar+nStopwords+nCharTitle+nTags, data=trainSample)

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
testSample <- bigTrainSample[sample(nrow(bigTrainSample), sampleSize),]
testSample <- preprocessLabeledData(testSample)
testSample <- addFeatures(testSample)
testSample <- featureScaling(testSample)
testSample.predictedGLM <- predict(trainSample.glm, newx=as.matrix(testSample[c("ReputationAtPostCreation", "logNCharClean", 
                                                    "commasPerChar", "logNCharBody", "nStopwords",
                                                    "OwnerUndeletedAnswerCountAtPostTime", "nCharTitle", "nTags")]), type="response", s=0.01)
testSample <- appendToDataframe(testSample, testSample.predictedGLM)
logLoss.glm <- multiclassLogLoss(testSample)
print(paste("GLM on test data set: ", logLoss.glm))

testSample.predictForest <- predict(trainSample.randomForest, newdata=testSample, type='prob')
testSample <- appendToDataframe(testSample, testSample.predictForest)
print("RandomForest on test data set")
logLoss.randomForest <- multiclassLogLoss(testSample)
print(paste("Random forest on test data set: ", logLoss.randomForest))

testSample.predictRpart <- predict(trainSample.rpart, testSample)
testSample <- appendToDataframe(testSample, testSample.predictRpart)
print("Rpart on test data set")
logLoss.rpart <- multiclassLogLoss(testSample)
print(paste("Rpart on test data set: ", logLoss.rpart))

############################
#apply to leaderboard data
###########################
print("Apply model to leaderboard data")
leaderboardSample <- read.csv("private_leaderboard.csv")
leaderboardSample <- addFeatures(leaderboardSample)
leaderboardSample <- featureScaling(leaderboardSample)
pred.randomForest <- predict(trainSample.randomForest, newdata=leaderboardSample, type='prob')
pred.rpart <- predict(trainSample.rpart, leaderboardSample)
pred.glm <- predict(trainSample.glm, newx=as.matrix(leaderboardSample[c("ReputationAtPostCreation", "logNCharClean", 
                                                                 "commasPerChar", "logNCharBody", "nStopwords",
                                                                 "OwnerUndeletedAnswerCountAtPostTime", "nCharTitle"
                                                                        , "nTags")]), type="response", s=0.01)
#set na to 0
pred[is.na(pred)] <- 0
write.table(pred.glm, file="predictionGLM.csv", sep=",", row.names=FALSE,col.names=FALSE)
write.table(pred.rpart, file="predictionRpart.csv", sep=",", row.names=FALSE,col.names=FALSE)
write.table(pred.randomForest, file="predictionRandomForest.csv", sep=",", row.names=FALSE,col.names=FALSE)

#score: 8.67054, rank 45




