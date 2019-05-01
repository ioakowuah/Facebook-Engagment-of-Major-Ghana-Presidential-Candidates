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


###Support Vector Machines Regression making independent variable factors
install.packages("e1071")
library(e1071)
svm_reg=svm(formula = Engagement ~ ., 
           data = training_set, type = "eps-regression" )

summary(svm_reg)

###Support Vector Machines Regression making independent variable as integers
svm_reg=svm(formula = Engagement ~ ., 
            data = Engagement.t, type = "eps-regression" )

summary(svm_reg)

###Prediction using svm model
y_pred=predict(svm_reg,newdata = test_set)
y_pred=predict(svm_reg,data.frame(TypeNo=1))
y_pred


##Visualizing svm model
library(ggplot2)
ggplot()+
  geom_point(aes(x=Engagement.t$TypeNo,y=Engagement.t$Engagement),
             colour="red")+
  geom_line(aes(x=Engagement.t$TypeNo,y=predict(svm_reg,newdata = Engagement.t)),
            colour="blue")+
  ggtitle("Post Type vs Engagement Count")+
  xlab("Post Type")+
  ylab("Engagement Count")






