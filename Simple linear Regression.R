###Importing dataset
Engagement <- read_csv("C:/Users/PRINCE/Desktop/Engagement.csv")
View(Engagement)


##Coverting some interger column to factor columns
names<-c(1:5,9,11)
Engagement[,names]<-lapply(Engagement[,names],factor)
str(Engagement)

##To subset from data
Engagement.t=Engagement[,c(2,7)]

###Splitting dataset into training set and test set
library("caTools", lib.loc="~/R/win-library/3.3")
set.seed(123)
split=sample.split(Engagement.t$Engagement,SplitRatio = 2/3)
split
training_set=subset(Engagement.t,split==TRUE)
test_set=subset(Engagement.t,split==FALSE)


###Simple linear regression
lin_reg=lm(formula = Engagement ~TypeNo,
             data = training_set )

summary(lin_reg)

###Predicting test data
y_pred=predict(lin_reg,newdata=test_set)
y_pred


###Visualizing the training set results
library(ggplot2)
ggplot()+
  geom_point(aes(x=training_set$TypeNo,y=training_set$Engagement),
             colour="red")+
  geom_line(aes(x=training_set$TypeNo,y=predict(lin_reg,newdata = training_set)),
            colour="blue")+
  ggtitle("Presidential Candidate vs Engagement Level(Training set)")+
  xlab("Post Type")+
  ylab("Engagement Count")

###Visualizing the test set results
ggplot()+
  geom_point(aes(x=test_set$TypeNo,y=test_set$Engagement),
             colour="red")+
  geom_line(aes(x=training_set$TypeNo,y=predict(lin_reg,newdata = training_set)),
            colour="blue")+
  ggtitle("Presidential Candidate vs Engagement Level(Test set)")+
  xlab("Post Type")+
  ylab("Engagement Count")






