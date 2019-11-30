sp <- read.csv2("material/spambase.csv")
sp$Spam <- as.factor(sp$Spam)
n <- dim(sp)[1]
set.seed(1234567890)
id <- sample(1:n, floor(n*0.6))
train <- sp[id,]
test <- sp[-id,]

library(mboost)
err_rate1<-c()
for(i in seq(10,100,10)){
  bb<-blackboost(Spam~.,data=train,control = boost_control(mstop = i),family = Binomial())
  pred<-predict.mboost(bb,train,type="class")
  cft<-table(Pred=pred,Actual=train$Spam)
  tp <- cft[2, 2]
  tn <- cft[1, 1]
  fp <- cft[2, 1]
  fn <- cft[1, 2]
  accuracy <- 1-(tp + tn)/(tp + tn + fp + fn)
  err_rate1<-c(err_rate1,accuracy)
}


err_rate2<-c()
for(i in seq(10,100,10)){
  bb<-blackboost(Spam~.,data=train,control = boost_control(mstop = i),family = AdaExp())
  pred<-predict.mboost(bb,test,type="class")
  cft<-table(Pred=pred,Actual=test$Spam)
  tp <- cft[2, 2]
  tn <- cft[1, 1]
  fp <- cft[2, 1]
  fn <- cft[1, 2]
  accuracy <- 1-(tp + tn)/(tp + tn + fp + fn)
  err_rate2<-c(err_rate2,accuracy)
}
library(ggplot2)
df<-data.frame(step=seq(10,100,10),err1=err_rate1,err2=err_rate2)
ggplot()+geom_line(data = df, aes(x = step, y = err1), color = "blue") +
  geom_line(data = df, aes(x = step, y = err2), color = "red") 

library(randomForest)

for(i in seq(10,100,10)){
  rf<-randomForest(Spam~.,data=train,ntree=i)
  pred<-predict(rf,test)
  cft<-table(Pred=pred,Actual=test$Spam)
  tp <- cft[2, 2]
  tn <- cft[1, 1]
  fp <- cft[2, 1]
  fn <- cft[1, 2]
  accuracy <- 1-(tp + tn)/(tp + tn + fp + fn)
  err_rate1<-c(err_rate1,accuracy)
}




