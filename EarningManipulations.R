setwd("C:/Users/Keerthi Reddy/Desktop/DataMining")
sample_manipulator<-read.csv("sample.csv",header=T)
ind = sample(2,nrow(sample_manipulator), replace= TRUE, prob = c(0.7,0.3))
trainData = sample_manipulator[ind==1,]
testData = sample_manipulator[ind==2,]
mylogit <- glm(C.MANIPULATOR ~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data = trainData, family = "binomial")
summary(mylogit)
summary(mylogit)
View(sample_manipulator)
str(sample_manipulator)
sample_manipulator$C.MANIPULATOR <-as.factor(sample_manipulator$C.MANIPULATOR)

mylogit1 <- glm(C.MANIPULATOR ~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data = testData, family = "binomial")
summary(mylogit1)

pred<-predict(mylogit, newdata = testData, type = "response")
pred<-predict(mylogit2, newdata = testData, type = "response")

pred
display(select(pred,"label","prediction"))
display(pred)

fitted.results <- ifelse(pred > 0.8,1,0)
misClasificError <- mean(fitted.results != testData$manipulator)
print(paste('Accuracy',1-misClasificError))

confint(mylogit)

install.packages("ROCR")
library(ROCR)
ROCRpred <- prediction(pred, testData$Manipulator)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(0,1)
auc <- performance(ROCRpred,"auc")
auc <- unlist(slot(auc, "y.values"))
minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")

legend(0.3,0.6,c(minauct,maxauct,"\n"),border="white",cex=0.8,box.col = "white")


install.packages("pROC")
library(pROC)
pred_train<-predict(mylogit, type = "response")

roccurve<-roc()

ind = sample(2,nrow(sample_manipulator), replace= TRUE, prob = c(0.7,0.3))
trainData = sample_manipulator[ind==1,]
testData = sample_manipulator[ind==2,]

table(trainData$Manipulator)

#building a logistic regression model with the training data
mylogit <- glm(C.MANIPULATOR ~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data = trainData, family = "binomial")
summary(mylogit)

#oversampling the training to balance the manipulators and non-manipulators
install.packages("ROSE")
library(ROSE)
train_Over<-ovun.sample(C.MANIPULATOR~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data = trainData, method = "over", N=252)$data
table(train_Over$C.MANIPULATOR)

#building a logistic regression model with the oversampled data
mylogit2 <- glm(C.MANIPULATOR ~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data = train_Over, family = "binomial")
summary(mylogit2)

pred_training = predict(mylogit, testData, type = "response")
TraindataPredictionTable = table(predict(mylogit, type="response", newData = testData), newData = testData$C.MANIPULATOR)
confusionMatrix(TraindataPredictionTable)


pred = predict(mylogit2, testData, type = "response") 
#The accuracy can be found using:
  accuracy = mean(testData$C.MANIPULATOR == pred)
accuracy

model_pred_prob_train<-predict(mylogit,testData,type="response")
model_pred_prob_Over<-predict(mylogit2,testData,type="response")

model_pred_prob_train
head(model_pred_prob_train)

#Code to find accuracy
model_pred_manipulator<-rep("0",64)
model_pred_manipulator[pred>0.9]<-"1"
tab<-table(model_pred_manipulator,testData$C.MANIPULATOR)
print(tab)
1-sum(diag(tab))/sum(tab)

View(train_Over)
train_Over_df<-as.data.frame(as.matrix(train_Over))
install.packages(randomForest)
library(randomForest)
randomForest(C.MANIPULATOR ~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data = train_Over)
