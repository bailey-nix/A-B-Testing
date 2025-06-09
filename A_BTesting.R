#A/B Testing

#loading data set and libraries
setwd("~/Desktop")
library(tidyverse)
library(ggplot2)
dat<-read_csv("AB_Test_Results.csv")

#check dataset & group numbers
head(dat)
table(dat$VARIANT_NAME)

#separate variant & control 
variant<-dat%>%filter(VARIANT_NAME=="variant")
control<-dat%>%filter(VARIANT_NAME=="control")

#check normality 
ggplot(variant,aes(REVENUE))+geom_histogram()
ggplot(control,aes(REVENUE))+geom_histogram()

ggplot(variant,aes(sample=REVENUE))+
  geom_qq()+geom_qq_line()

ggplot(control,aes(sample=REVENUE))+
  geom_qq()+geom_qq_line()
#not normal

#check for equal variances
var(variant$REVENUE)
var(control$REVENUE)
#variances not equal---proceed with t-test with Welch's correction

#conduct welchs t-test- 2 ways
t.test(REVENUE~VARIANT_NAME,data=dat)

t.test(variant$REVENUE,control$REVENUE)
####not significant

#visualize 
ggplot(dat,aes(REVENUE,VARIANT_NAME))+geom_boxplot()+ggtitle("Revenue by Group")+theme(plot.title = element_text(hjust = 0.5))+xlab("Revenue")+ylab("Group")


ggplot(dat, aes(x=factor(VARIANT_NAME),y=REVENUE))+geom_bar(stat="summary", fun="mean",fill=c("blue","red"))+
  ggtitle("Average Revenue by Group")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Group")+ylab("Average Revenue")
  