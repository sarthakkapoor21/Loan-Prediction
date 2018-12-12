
df <- read.csv('loan.csv')

require(caTools)
library(e1071)
library(rpart)
library(class)
library(rpart.plot)
library(fastAdaboost)
library(ggplot2)

head(df)
str(df)


df <- df[-((ncol(df)-17):ncol(df))]
df <- df[-c(1,2,16,19,20,22,23,24,27,29,30,37,38,43,46,48,49,51,53,54,55)]

na.omit(df) -> df
nrow(df)

str(df)

Factored <- df[c(4,7,8,9,10,11,13:16,25,35)]
df <- df[-c(4,7,8,9,10,11,13:16,25,35)]
scale(df) -> Scaled
cbind(Scaled,Factored) -> Final

Final[-c(23)] -> Final
na.omit(Final) -> Final
Final[-c(26)] -> Final
Final[-c(25)] -> Final

set.seed(101)
sample <- sample.split(Final$loan_amnt,SplitRatio = 0.65)
train <-subset(Final,sample==TRUE)
test <- subset(Final,sample==FALSE)
na.omit(train) -> train

redit_boost100 <- naiveBayes(train[-30], train$loan_status)
predicts1 <- predict(redit_boost100,test)
predicts1 <- as.vector(predicts1)

sum(predicts1==test$loan_status)/length(predicts1)

library(randomForest)
library(rpart.plot)
library(class)

redit_boost101 <- rpart(loan_status~.,data=train)
predict2 <- predict(redit_boost101,test,type="class")
predict2 <- as.vector(predict2)

sum(predict2==test$loan_status)/length(predict2) #Accuracy

rpart.plot(redit_boost101)

train[c(1:22)] -> Numerical

mtscaled <- as.matrix((Numerical))

cor(Numerical)

library(corrplot)
corrplot(cor(Numerical),method="color")

str(train)

ggplot(aes(x=loan_status),data=Final)+geom_bar()
ggsave("First.png",width=20,height=10)

ggplot(aes(x=loan_status),data=train)+geom_bar()+facet_wrap(~verification_status)
ggsave("Sedcond.png",width=20,height=10)

ggplot(aes(x=loan_status),data=train)+geom_bar()+facet_wrap(~grade)
ggsave("Third.png",width=20,height=10)

ggplot(aes(x=total_acc,y=open_acc),data=subset(Final,loan_status=="Default"))+geom_point(aes(col=loan_status))
