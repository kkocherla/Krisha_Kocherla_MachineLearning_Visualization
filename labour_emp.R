# libraries
rm(list=ls())
install.packages("tidyverse")
install.packages("caret")
install.packages("twitteR")
library(ggplot2)
library(data.table)
library(tidyr)
library(tidyverse)
library(readr)
library(dplyr)
library(class)
library(foreach)
library(caret)
library(twitteR)
install.packages("thePackage")
#importing data set

labour=read_csv("C:/Users/koche/OneDrive/Desktop/labour.csv")
lb=labour
employement=read_csv("C:/Users/koche/OneDrive/Desktop/employement.csv")
em=employement

# descriptive statistics
head(labour)
head(employement)

# visualization
boxplot(labour$`Oct-19`,labour$`Nov-19`,labour$`Dec-19`,labour$`Jan-20`,labour$`Feb-20`,names = c("oct-19","nov-19","dec-19","jan-20","feb-20"))
# a similar plot for the employement  was visualized as shown below
boxplot(employement$`Oct-19`,employement$`Nov-19`,employement$`Dec-19`,employement$`Jan-20`,employement$`Feb-20`,names = c ("oct-19","nov-19","dec-19","jan-20","feb-20"))



#means  in various Sex of individuals
#1. labour 
labour_sex<-labour %>% group_by(Sex)
labour_sex %>% summarise(`Oct-19`=mean(`Oct-19`),`Nov-19`=mean(`Nov-19`),`Dec-19`=mean(`Dec-19`),`Jan-20`=mean(`Jan-20`),`Feb-20`=mean(`Feb-20`))
#2.employement means
employement_sex<-employement%>%group_by(Sex)
employement_sex%>%summarise(`Oct-19`=mean(`Oct-19`),`Nov-19`=mean(`Nov-19`),`Dec-19`=mean(`Dec-19`),`Jan-20`=mean(`Jan-20`),`Feb-20`=mean(`Feb-20`))

#1. labour 
labour_sex<-labour%>%group_by(`Age group`)
labour_sex%>%summarise(`Oct-19`=mean(`Oct-19`),`Nov-19`=mean(`Nov-19`),`Dec-19`=mean(`Dec-19`),`Jan-20`=mean(`Jan-20`),`Feb-20`=mean(`Feb-20`))
#2.employement means
employement_sex<-employement%>%group_by(`Age group`)
employement_sex%>%summarise(`Oct-19`=mean(`Oct-19`),`Nov-19`=mean(`Nov-19`),`Dec-19`=mean(`Dec-19`),`Jan-20`=mean(`Jan-20`),`Feb-20`=mean(`Feb-20`))

par(mfrow=c(5,1))
ggplot(data=labour,mapping = aes(x=reorder(Sex,`Oct-19`,median,na.rm=TRUE),y=`Oct-19`))+geom_boxplot()+labs(x="SEx",y="labour(`Oct-19`)")
ggplot(data=labour,mapping = aes(x=reorder(Sex,`Nov-19`,median,na.rm=TRUE),y=`Nov-19`))+geom_boxplot()+labs(x="`SEx`",y="labour(`Nov-19`)")


ggplot(data=labour,mapping = aes(x=reorder(Sex,`Dec-19`,median,na.rm=TRUE),y=`Dec-19`))+geom_boxplot()+labs(x="SEx",y="labour(`Dec-19`)")
ggplot(data=labour,mapping = aes(x=reorder(Sex,`Jan-20`,median,na.rm=TRUE),y=`Jan-20`))+geom_boxplot()+labs(x="`SEx`",y="labour(`Jan-20`)")
ggplot(data=labour,mapping = aes(x=reorder(Sex,`Feb-20`,median,na.rm=TRUE),y=`Feb-20`))+geom_boxplot()+labs(x="`SEx`",y="labour(`Feb-20`)")

par(mfrow=c(5,1))
ggplot(data=employement,mapping = aes(x=reorder(Sex,`Oct-19`,median,na.rm=TRUE),y=`Oct-19`))+geom_boxplot()+labs(x="SEx",y="employement(`Oct-19`)")
ggplot(data=employement,mapping = aes(x=reorder(Sex,`Nov-19`,median,na.rm=TRUE),y=`Nov-19`))+geom_boxplot()+labs(x="`SEx`",y="employement(`Nov-19`)")


ggplot(data=employement,mapping = aes(x=reorder(Sex,`Dec-19`,median,na.rm=TRUE),y=`Dec-19`))+geom_boxplot()+labs(x="SEx",y="employement(`Dec-19`)")
ggplot(data=employement,mapping = aes(x=reorder(Sex,`Jan-20`,median,na.rm=TRUE),y=`Jan-20`))+geom_boxplot()+labs(x="`SEx`",y="employement(`Jan-20`)")
ggplot(data=employement,mapping = aes(x=reorder(Sex,`Feb-20`,median,na.rm=TRUE),y=`Feb-20`))+geom_boxplot()+labs(x="`SEx`",y="employement(`Feb-20`)")

# normalizing the numeric variables
norm<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
lb<-as.data.frame(lapply(labour[3:7], norm))
lb<-data.frame(lb,labour$Sex)

r1<-sample(2,nrow(lb),replace = TRUE,prob = c(.75,.25))
train=lb[r1==1,1:5]
test=lb[r1==2,1:5]
#view(lb)
train_labels<-lb[r1==1,6]
test_labels<-lb[r1==2,6]
model1<-knn(train = train,test=test,cl=train_labels,k=3)
# evaluate the model perfomance

data.frame(model1,test_labels)

norm<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
lb<-as.data.frame(lapply(employement[3:7], norm))
lb<-data.frame(lb,employement$Sex)

r1<-sample(2,nrow(lb),replace = TRUE,prob = c(.9,.1))
train=lb[r1==1,1:5]
test=lb[r1==2,1:5]

train_labels<-lb[r1==1,6]
test_labels<-lb[r1==2,6]
model2<-knn(train = train,test=test,cl=train_labels,k=3)
# evaluate the model perfomance
data.frame(model2,test_labels)