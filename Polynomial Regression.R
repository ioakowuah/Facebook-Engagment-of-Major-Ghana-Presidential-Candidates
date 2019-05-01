###Importing dataset
Engagement <- read_csv("C:/Users/PRINCE/Desktop/Engagement.csv")

##Coverting some interger column to factor columns
names<-c(1:5,9,11)
Engagement[,names]<-lapply(Engagement[,names],factor)
str(Engagement)

##To subset from data
#Engagement.t=Engagement[,c(1,4)]

###Splitting dataset into training set and test set
set.seed(123)
split=sample.split(Engagement$EngagementNo,SplitRatio = 0.8)
split
training_set=subset(Engagement,split==TRUE)
test_set=subset(Engagement,split==FALSE)


###Polynomial Regression
Engagement$TypeNo2=Engagement$TypeNo^2
Engagement$TypeNo3=Engagement$TypeNo^3
Pol_reg=lm(formula = EngagementNo ~ TypeNo+TypeNo2+TypeNo3,
             data = training_set )

summary(Pol_reg)

ggplot()+
  geom_point(aes(x=test_set$TypeNo,y=test_set$EngagementNo),
             colour="red")+
  geom_line(aes(x=training_set$TypeNo,y=predict(Pol_reg,newdata = training_set)),
            colour="blue")+
  ggtitle("Presidential Candidate vs Engagement Level(Test set)")+
  xlab("Post Type")+
  ylab("Engagement Level")

###Prediction using polynomial model
y_pred=predict(Pol_reg,newdata = test_set)
y_pred






