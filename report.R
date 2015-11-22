library(caret)
library(kernlab)
library(randomForest)
library(corrplot)
library(rpart)

trainRaw <- read.csv("pml-training.csv", na.strings=c("NA", "", " "))
testRaw <- read.csv("pml-testing.csv", na.strings=c("NA", "", " "))

dim(trainRaw)
names(trainRaw)

na_stat <- sapply(trainRaw, function(x){sum(is.na(x))})
trainData <- trainRaw[, which(na_stat == 0)]
trainData <- trainData[8:length(trainData)]

na_stat_test <- sapply(testRaw, function(x){sum(is.na(x))})
testData <- testRaw[, which(na_stat_test == 0)]
testData <- testData[8:length(testData)]

inTrain <- createDataPartition(y=trainData$classe, p=0.7, list=FALSE)
training <- trainData[inTrain, ]
testing <- trainData[-inTrain, ]

corMatrix <- cor(training[, -length(names(training))])
corrplot(corMatrix, method="square", type="lower", tl.cex=.6)

modLDA <- train(classe ~ ., data=training, method="lda")
modRf <- train(classe ~ ., data=training, method="rf")

lda <- predict(modLDA, testing)
confusionMatrix(lda, testing$classe)

rf <- predict(modRf, testing)
confusionMatrix(rf, testing$classe)

pml_create_files <- function(pref, x){
  n = length(x)
  for (i in 1:n){
    fn <- paste0(pref, "/problem_id_",i,".txt")
    write.table(x[i], file=fn, quote=FALSE, row.names=FALSE, col.names=FALSE)
  }
}

mainfunction <- function(){
  pred1 <- predict(modLDA, testData)
  answers1 <- as.vector(pred1)
  pml_create_files("lda", answers1)
  
  pred2 <- predict(modRf, testData)
  answers2 <- as.vector(pred2)
  pml_create_files("rf", answers2)
  
}

mainfunction()
