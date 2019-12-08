setwd("C:/Users/PRINCE/Desktop/Machine Learning Models")
getwd
###Importing dataset
library(readr)
Engagement <- read_csv("C:/Users/PRINCE/Desktop/Engagement.csv")

##Coverting some interger column to factor columns
names<-c(1:5,9,11)
Engagement[,names]<-lapply(Engagement[,names],factor)
str(Engagement)

##To subset from data
Engagement.t=Engagement[,c(2,7)]

###Splitting dataset into training set and test set
library(caTools)
set.seed(123)
split=sample.split(Engagement.t$Engagement,SplitRatio = 0.8)
training_set=subset(Engagement.t,split==TRUE)
test_set=subset(Engagement.t,split==FALSE)

###Decision Tree Regression making independent variable factors
install.packages("rpart")
library(rpart)
dtg_reg=rpart(formula = Engagement ~ ., 
            data = training_set,control = rpart.control(minsplit = 1))

summary(dtg_reg)

###Prediction using Decision Tree Regression model
y_pred=predict(dtg_reg,newdata = test_set)
y_pred=predict(dtg_reg,data.frame(TypeNo=1))
y_pred


##Visualizing Decision Tree Regression model
library(ggplot2)
x_grid=seq(min(Engagement.t$TypeNo),max(Engagement.t$TypeNo),0.01)
ggplot()+
  geom_point(aes(x=Engagement.t$TypeNo,y=Engagement.t$Engagement),
             colour="red")+
  geom_line(aes(x=Engagement.t$TypeNo,y=predict(dtg_reg,newdata = Engagement.t)),
            colour="blue")+
  ggtitle("Post Type vs Engagement Count(Decision Tree Regression)")+
  xlab("Post Type")+
  ylab("Engagement Count")






