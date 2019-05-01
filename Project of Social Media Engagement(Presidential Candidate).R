######
## 1. Number of Post made by each candidate on facebook and Twitter
#####
library("readr")
Mahama <- read_csv("C:/Users/PRINCE/Desktop/Facebook data/Mahama.csv")
JDMahamaMahama_tweets <- read_csv("C:/Users/PRINCE/Desktop/Twitter data/JDMahamaMahama_tweets.csv")
NaNaAddo <- read_csv("C:/Users/PRINCE/Desktop/Facebook data/NaNaAddo.csv")
NAkufoAddoNana_tweets <- read_csv("C:/Users/PRINCE/Desktop/Twitter data/NAkufoAddoNana_tweets.csv")

######
##Subsetting Facebook post from  1st Jan 2016 to 30th December 2016
######
Mahama$dates <- as.Date(format(Mahama$created_time, "%Y-%m-%d"))
NaNaAddo$dates <- as.Date(format(NaNaAddo$created_time, "%Y-%m-%d"))
DATE1 <- as.Date("2016-01-01")
DATE2 <- as.Date("2016-12-31")
Mahamadates_fb <- Mahama[Mahama$dates>=DATE1 & Mahama$dates<=DATE2,]
NaNaAddodates_fb <- NaNaAddo[NaNaAddo$dates>=DATE1 & NaNaAddo$dates<=DATE2,]
dim(Mahamadates_fb)
dim(NaNaAddodates_fb)

######
##Subsetting Twitter post from  1st Jan 2016 to 30th December 2016
######
JDMahamaMahama_tweets$dates<-substr(JDMahamaMahama_tweets$created,1,10)
JDMahamaMahama_tweets$dates <- as.Date(JDMahamaMahama_tweets$dates,"%m/%d/%Y")
NAkufoAddoNana_tweets$dates<-as.Date(format(NAkufoAddoNana_tweets$created, "%Y-%m-%d"))
DATE1 <- as.Date("2016-01-01")
DATE2 <- as.Date("2016-12-31")
Mahamadates_tw<- JDMahamaMahama_tweets[JDMahamaMahama_tweets$dates>=DATE1 & JDMahamaMahama_tweets$dates<=DATE2,]
NaNaAddodates_tw<- NAkufoAddoNana_tweets[NAkufoAddoNana_tweets$dates>=DATE1 & NAkufoAddoNana_tweets$dates<=DATE2,]
dim(Mahamadates_tw)
dim(NaNaAddodates_tw)

#####
##Plot of barchart
####
data<-data.frame(Presidental_Candidate=c("John_Mahama","Nana_Addo_Dankwah","John_Mahama","Nana_Addo_Dankwah"),Social_Media=c("Facebook","Twitter","Twitter","Facebook"),Post_Count=c(301,1827,178,851))
ggplot(data, aes(y=Post_Count, x=Social_Media, color=Presidental_Candidate, fill=Presidental_Candidate)) + 
geom_bar( position="dodge", stat="identity")+geom_text(aes(label=Post_Count), hjust=1.0, color="black", size=5.5)

#####
##Number of Post per month on both Facebook and Twitter
#####
##Facebook
NanaMahama_fb<-rbind(Mahamadates_fb,NaNaAddodates_fb)
NanaMahama_fb$Month<-as.Date(NanaMahama_fb$created_time)
NanaMahama_fb$mon<-as.numeric(format(NanaMahama_fb$Month,format="%m"))
NanaMahama_fb$mon[NanaMahama_fb$mon==1]<-"Jan"
NanaMahama_fb$mon[NanaMahama_fb$mon==2]<-"Feb"
NanaMahama_fb$mon[NanaMahama_fb$mon==3]<-"Mar"
NanaMahama_fb$mon[NanaMahama_fb$mon==4]<-"Apr"
NanaMahama_fb$mon[NanaMahama_fb$mon==5]<-"May"
NanaMahama_fb$mon[NanaMahama_fb$mon==6]<-"Jun"
NanaMahama_fb$mon[NanaMahama_fb$mon==7]<-"Jul"
NanaMahama_fb$mon[NanaMahama_fb$mon==8]<-"Aug"
NanaMahama_fb$mon[NanaMahama_fb$mon==9]<-"Sep"
NanaMahama_fb$mon[NanaMahama_fb$mon==10]<-"Oct"
NanaMahama_fb$mon[NanaMahama_fb$mon==11]<-"Nov"
NanaMahama_fb$mon[NanaMahama_fb$mon==12]<-"Dec"
names(NanaMahama_fb)[3]<-"Presidential_Candidate_Page"
NanaMahama_fb$mon<-factor(NanaMahama_fb$mon,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
ggplot(NanaMahama_fb,aes(x=mon,fill=Presidential_Candidate_Page))+geom_bar(position = "dodge")+scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+labs(title="MONTHLY POST COMPARISON ON FACEBOOK", y="Number of Posts", x="Month", caption="Data Source: Facebook")
table(NanaMahama_fb$mon,NanaMahama_fb$Presidential_Candidate_Page)
summary(NanaMahama_fb$Engagement)

###Twitter
NanaMahama_tw<-rbind(Mahamadates_tw,NaNaAddodates_tw)
NanaMahama_tw$Month<-as.Date(NanaMahama_tw$dates)
NanaMahama_tw$mon<-as.numeric(format(NanaMahama_tw$Month,format="%m"))
NanaMahama_tw$mon[NanaMahama_tw$mon==1]<-"Jan"
NanaMahama_tw$mon[NanaMahama_tw$mon==2]<-"Feb"
NanaMahama_tw$mon[NanaMahama_tw$mon==3]<-"Mar"
NanaMahama_tw$mon[NanaMahama_tw$mon==4]<-"Apr"
NanaMahama_tw$mon[NanaMahama_tw$mon==5]<-"May"
NanaMahama_tw$mon[NanaMahama_tw$mon==6]<-"Jun"
NanaMahama_tw$mon[NanaMahama_tw$mon==7]<-"Jul"
NanaMahama_tw$mon[NanaMahama_tw$mon==8]<-"Aug"
NanaMahama_tw$mon[NanaMahama_tw$mon==9]<-"Sep"
NanaMahama_tw$mon[NanaMahama_tw$mon==10]<-"Oct"
NanaMahama_tw$mon[NanaMahama_tw$mon==11]<-"Nov"
NanaMahama_tw$mon[NanaMahama_tw$mon==12]<-"Dec"
names(NanaMahama_tw)[12]<-"Presidential_Candidate_Page"
NanaMahama_tw$mon<-factor(NanaMahama_tw$mon,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
ggplot(NanaMahama_tw,aes(x=mon,fill=Presidential_Candidate_Page))+geom_bar(position = "dodge")+scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+labs(title="MONTHLY POST COMPARISON ON TWITTER", y="Number of Posts", x="Month", caption="Data Source: Twitter")
table(NanaMahama_tw$mon)
table(NanaMahama_tw$mon,NanaMahama_tw$Presidential_Candidate_Page)

########
##Post Type made on each Presidential Facebook Page
########
names(Mahamadates_fb)[6]<-"Post_Type"
names(NaNaAddodates_fb)[6]<-"Post_Type"
Mahamadates_fbplot<-ggplot(Mahamadates_fb,aes(x=Post_Type,fill=Post_Type))+geom_bar(position = "dodge")
Mahamadates_fbplot+ labs(title="JOHN DRAMANI MAHAMA POST TYPE", y="Number of Posts", x="Post Type", caption="Data Source: Facebook_John_Dramani_Mahama")
NaNaAddodates_fbplot<-ggplot(NaNaAddodates_fb,aes(x=Post_Type,fill=Post_Type))+geom_bar(position = "dodge")
NaNaAddodates_fbplot+ labs(title="NANA ADDO DANKWA AKUFO-ADDO POST TYPE", y="Number of Posts", x="Post Type", caption="Data Source: Facebook_Nana_Addo_Dankwa_Akufo-Addo")

names(NanaMahama_fb)[6]<-"Post_Type"
NanaMahama_fbplot<-ggplot(NanaMahama_fb,aes(x=Post_Type,fill=Post_Type))+geom_bar(position = "dodge")+facet_grid(. ~ Presidential_Candidate_Page)
NanaMahama_fbplot+ labs(title="POST TYPE BY BOTH PRESIDENTIAL CANDIDATES", y="Number of Posts", x="Post Type", caption="Data Source: Facebook (Jan 2016 - Dec 2016)")

###Limitation
##Twitter data is silent on the post type. It makes known the text on the page.

####
##Engagement over time
####
NanaMahama_fb$Engagement<-(NanaMahama_fb$likes_count+NanaMahama_fb$comments_count+NanaMahama_fb$shares_count)
ggplot(NanaMahama_fb,aes(x=as.Date(NanaMahama_fb$created_time),y=NanaMahama_fb$Engagement))+geom_jitter(aes(colour=NanaMahama_fb$Post_Type))+facet_grid(Presidential_Candidate_Page~ .)+ labs(title="Comparison Engagement",subtitle = "Both Nana Addo and John Mahama",caption="Source: Based on Facebook data [Jan 2016 - Jan 2017]",x="Post Date",y="Engement")


########
##Anova Test to show if Average Engagement varies with Post Type
#######

##Anova Hypothesis
#Null:Mean engagement across post type are same
#Alternative: At least one mean engagement across post type is different
###Table of means and sd of Post Type
library(dplyr)
group_by(NanaMahama_fb, Post_Type) %>%
  summarise(
    count = n(),
    mean = mean(Engagement, na.rm = TRUE),
    sd = sd(Engagement, na.rm = TRUE)
  )

###Boxplot of means of Post Type
library("ggpubr")
ggboxplot(NanaMahama_fb, x = "Post_Type", y = "Engagement", 
          color = "Post_Type", palette = c("#00AFBB", "#E7B800", "#FC4E07","#7FFFD4","#00640D"),
          order = c("event", "link", "photo","status","video"),
          ylab = "Engagement", xlab = "Post_Type")

####Table of Post Type and its Total Engagement
EventEngmaha_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='John Dramani Mahama' & NanaMahama_fb$Post_Type=='event')
LinkEngmaha_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='John Dramani Mahama' & NanaMahama_fb$Post_Type=='link')
PhotoEngmaha_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='John Dramani Mahama' & NanaMahama_fb$Post_Type=='photo')
StatusEngmaha_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='John Dramani Mahama' & NanaMahama_fb$Post_Type=='status')
VideoEngmaha_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='John Dramani Mahama' & NanaMahama_fb$Post_Type=='video')

a=sum(EventEngmaha_fb$Engagement)
b=sum(LinkEngmaha_fb$Engagement)
c=sum(PhotoEngmaha_fb$Engagement)
d=sum(StatusEngmaha_fb$Engagement)
e=sum(VideoEngmaha_fb$Engagement)

Post<-c(nrow(EventEngmaha_fb),nrow(LinkEngmaha_fb),nrow(PhotoEngmaha_fb),nrow(StatusEngmaha_fb),nrow(VideoEngmaha_fb))
Post_Eng<-c(a,b,c,d,e)
Post_Type<-c('Event','Link','Photo','Status','Video')
PP<-data.frame(Post_Type,Post,Post_Eng)
PP

EventEngnana_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='Nana Addo Dankwa Akufo-Addo' & NanaMahama_fb$Post_Type=='event')
LinkEngnana_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='Nana Addo Dankwa Akufo-Addo' & NanaMahama_fb$Post_Type=='link')
PhotoEngnana_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='Nana Addo Dankwa Akufo-Addo' & NanaMahama_fb$Post_Type=='photo')
StatusEngnana_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='Nana Addo Dankwa Akufo-Addo' & NanaMahama_fb$Post_Type=='status')
VideoEngnana_fb<-subset(NanaMahama_fb,NanaMahama_fb$Presidential_Candidate_Page=='Nana Addo Dankwa Akufo-Addo' & NanaMahama_fb$Post_Type=='video')

aa=sum(EventEngnana_fb$Engagement)
bb=sum(LinkEngnana_fb$Engagement)
cc=sum(PhotoEngnana_fb$Engagement)
dd=sum(StatusEngnana_fb$Engagement)
ee=sum(VideoEngnana_fb$Engagement)

Postt<-c(nrow(EventEngnana_fb),nrow(LinkEngnana_fb),nrow(PhotoEngnana_fb),nrow(StatusEngnana_fb),nrow(VideoEngnana_fb))
Post_Engg<-c(aa,bb,cc,dd,ee)
Post_Type<-c('Event','Link','Photo','Status','Video')
TT<-data.frame(Post_Type,Postt,Post_Engg)
TT
####Twitter Engagement
NanaMahama_tw$Engagement<-NanaMahama_tw$favoriteCount+NanaMahama_tw$retweetCount
PostNana<-nrow(subset(NanaMahama_tw,NanaMahama_tw$Presidential_Candidate_Page=='NAkufoAddo'))
EngCountNana<-subset(NanaMahama_tw,NanaMahama_tw$Presidential_Candidate_Page=='NAkufoAddo')
EngCountNana1<-sum(EngCountNana$Engagement)

PostMaha<-nrow(subset(NanaMahama_tw,NanaMahama_tw$Presidential_Candidate_Page=='JDMahama'))
EngCountMaha<-subset(NanaMahama_tw,NanaMahama_tw$Presidential_Candidate_Page=='JDMahama')
EngCountMaha1<-sum(EngCountMaha$Engagement)
presidential_Candidate<-c('Nana Addo Dankwa Akufo-Addo','John Dramani Mahama')
Post_Count<-c(PostNana,PostMaha)
Post_Engagement<-c(EngCountNana1,EngCountMaha1)
Twitter_Eng<-data.frame(presidential_Candidate,Post_Count,Post_Engagement)
Twitter_Eng

####Anova test
res.aov <- aov(Engagement ~ Post_Type, data = NanaMahama_fb)
summary(res.aov)
TukeyHSD(res.aov)
###Test of Assumptions
##The population at each factor level are approximately normally.
plot(res.aov, 1)
##The variance at each factor level are approximately equal to one another.
plot(res.aov, 2)


####Which Presidential Candidate had his post most engaged
group_by(NanaMahama_fb, Presidential_Candidate_Page) %>%
  summarise(
    count = n(),
    mean = mean(Engagement, na.rm = TRUE),
    sd = sd(Engagement, na.rm = TRUE)
  )

###Boxplot of means of Post Type
library("ggpubr")
ggboxplot(NanaMahama_fb, x = "Presidential_Candidate_Page", y = "Engagement", 
          color = "Presidential_Candidate_Page", palette = c("#7FFFD4","#00640D"),
          order = c("John Dramani Mahama", "Nana Addo Dankwa Akufo-Addo"),
          ylab = "Engagement", xlab = "Presidential_Candidate_Page")


###Preparing data for modelling
##Recoding Engagement to Engagement Level
summary(NanaMahama_fb$Engagement)
boxplot(NanaMahama_fb$Engagement)
NanaMahama_fb$Engagement_Level<-as.numeric(NanaMahama_fb$Engagement_Level)
NanaMahama_fb$Engagement_Level<-(NanaMahama_fb$likes_count+NanaMahama_fb$comments_count+NanaMahama_fb$shares_count)
NanaMahama_fb$Engagement_Level[NanaMahama_fb$Engagement_Level<=5096]<-"Low_Engagement"
NanaMahama_fb$Engagement_Level[NanaMahama_fb$Engagement_Level>2754 & NanaMahama_fb$Engagement_Level<9524]<-"Medium_Engagement"
NanaMahama_fb$Engagement_Level[NanaMahama_fb$Engagement_Level>5096]<-"High_Engagement"
NanaMahama_fb$Engagement_Level[NanaMahama_fb$Engagement_Level=='NA']<-"Low_Engagement"
NanaMahama_fb$Engagement_Level[NanaMahama_fb$Engagement_Level==975]<-"Low_Engagement"
NanaMahama_fb$Engagement_Level[NanaMahama_fb$Engagement_Level==973]<-"Low_Engagement"
NanaMahama_fb$Engagement_Level[NanaMahama_fb$Engagement_Level<=970]<-"Low_Engagement"
table(NanaMahama_fb$Engagement_Level)
str(NanaMahama_fb$Engagement_Level)
NanaMahama_fb$Engagement_Level[is.na(NanaMahama_fb$Engagement_Level)] <- "Low_Engagement"


###Recoding Created time to Weekday
NanaMahama_fb$Weekday<-weekdays(NanaMahama_fb$created_time, abbreviate = FALSE)
table(NanaMahama_fb$Weekday)
NanaMahama_fb$Hours<-substr(NanaMahama_fb$created_time,12,16)
NanaMahama_fb$Hours_Momemt<-substr(NanaMahama_fb$created_time,12,13)

NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt==12]<-"Noon"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt>12 & NanaMahama_fb$Hours_Momemt<=17]<-"Afternoon"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt>17 & NanaMahama_fb$Hours_Momemt<=20]<-"Evening"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt>=20 & NanaMahama_fb$Hours_Momemt<=23]<-"Night"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="00"]<-"Night"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="01"]<-"Night"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="02"]<-"Night"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="03"]<-"Night"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="04"]<-"Night"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="05"]<-"Night"


NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="06"]<-"Morning"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="07"]<-"Morning"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="08"]<-"Morning"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="09"]<-"Morning"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="10"]<-"Morning"
NanaMahama_fb$Hours_Momemt[NanaMahama_fb$Hours_Momemt=="11"]<-"Morning"
table(NanaMahama_fb$Hours)
table(NanaMahama_fb$Hours_Momemt)
str(NanaMahama_fb$Hours_Momemt)


NanaMahama_fb$Post_Typee=NanaMahama_fb$Post_Type
NanaMahama_fb$Post_Typee[NanaMahama_fb$Post_Typee=="event"]<-"link"
NanaMahama_fb$Post_Typee[NanaMahama_fb$Post_Typee=="link"]<-"link+event"
chisq<-chisq.test(NanaMahama_fb$Presidential_Candidate_Page,NanaMahama_fb$Post_Typee)
table(NanaMahama_fb$Presidential_Candidate_Page,NanaMahama_fb$Post_Typee)
round(chisq$residuals, 3)
chisq.test(NanaMahama_fb$Presidential_Candidate_Page,NanaMahama_fb$Engagement_Level)
table(NanaMahama_fb$Presidential_Candidate_Page,NanaMahama_fb$Engagement_Level)

summary(NaNaAddo_Mahama$Engagement)
