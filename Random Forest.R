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
Engagement.t=Engagement[,c(8,7)]

###Splitting dataset into training set and test set
library(caTools)
set.seed(123)
split=sample.split(Engagement.t$Engagement,SplitRatio = 0.8)
training_set=subset(Engagement.t,split==TRUE)
test_set=subset(Engagement.t,split==FALSE)

###Random Forest Regression making independent variable factors
install.packages("randomForest")
library(randomForest)
set.seed(1234)
dtg_reg=randomForest(x=Engagement.t[1],
                     y=Engagement.t$Engagement,
                     ntree=100)
              

summary(dtg_reg)

###Prediction using Random Forest Regression model
y_pred=predict(dtg_reg,newdata = test_set)
y_pred=predict(dtg_reg,data.frame(Likes_Count=50000))
y_pred


##Visualizing Random Forest Regression model
library(ggplot2)
x_grid=seq(min(Engagement.t$Likes_Count),max(Engagement.t$Likes_Count),1)
ggplot()+
  geom_point(aes(x=Engagement.t$Likes_Count,y=Engagement.t$Engagement),
             colour="red")+
  geom_line(aes(x=Engagement.t$Likes_Count,y=predict(dtg_reg,newdata = Engagement.t)),
            colour="blue")+
  ggtitle("Like Count vs Engagement Count(Random Forest Regression)")+
  xlab("Like Count")+
  ylab("Engagement Count")






