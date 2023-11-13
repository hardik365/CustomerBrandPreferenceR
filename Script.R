install.packages("caret",dependencies=c("Depends","Suggests"))
install.packages("ggplot2",dependencies=c("Depends","Suggests"))
install.packages("gower",dependencies=c("Depends","Suggests") )
install.packages("hardhat", dependencies=c("Depends","Suggests"))
install.packages("timechange",dependencies=c("Depends","Suggests"))
install.packages("ModelMetrics",dependencies=c("Depends","Suggests"))
install.packages("klaR",dependencies=c("Depends","Suggests"))
install.packages("ellipsis",dependencies=c("Depends","Suggests"))

install.packages("pkgconfig",dependencies=c("Depends","Suggests"))
install.packages("inum",dependencies=c("Depends","Suggests"))

.libPaths()

#load libraries and set a seed
library(caret)
library(readr)
library(C50)
library(forcats)
library(gbm)
set.seed(123)

#Read our completed data survey
CompleteData<- read.csv("CompleteResponses.csv")
InCompleteData <- read.csv("SurveyIncomplete.csv")

attributes(CompleteData)
str(CompleteData) 

#convert the brand column to factor type
CompleteData$brand<-as.factor(CompleteData$brand)
InCompleteData$brand<-as.factor(InCompleteData$brand)

str(CompleteData)

#randomly set our training and testing data in the complete dataframe
inTraining <- createDataPartition(CompleteData$brand, p = .75, list = FALSE)
training <- CompleteData[inTraining,]
testing <- CompleteData[-inTraining,]


#now we try with GBM to see how well this model works
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
gbmFit <- train(brand~.,
                data = training, 
                method = "gbm",
                trControl = fitControl,
                tuneLength = 5)

gbmFit$finalModel$tuneValue
gbmFit$finalModel$tree
summary(gbmFit)
plot(gbmFit)
gbmFit
varImp(gbmFit) 

#now we try with Random Forest (manual grid) to see how it works

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

#note the system time wrapper.
system.time(rfFitm1 <- train(brand~.,
                             data = training,
                             method = "rf",
                             trControl=fitControl,
                             tuneGrid=rfGrid))
rfFitm1
varImp(rfFitm1)
plot(rfFitm1)
summary(rfFitm1)
#Both seem to run well, I will proceed and use GBM to make the model
#but either one can hypothetically work well

#I will predict with GBM 
testPred <- predict(gbmFit, testing)
#This will compare the actual results from testing and predictions
postResample(testPred, testing$brand)
confusionMatrix(testPred, testing$brand)

#this is the final prediction on the incomplete survey
finalPred <- predict(gbmFit, InCompleteData)
postResample(finalPred,testing$brand)

summary(finalPred)

summary(CompleteData)


