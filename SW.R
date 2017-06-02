rm(list = ls())
gc()

#load packages
library(dplyr)
library(ggplot2)

#Import data
DQ_Data<-read.table(file = 'DQ_Data_b.tsv', sep = '\t', header = TRUE)

head(DQ_Data)

#Names refined
names(DQ_Data)[3]<-"Percent Unique"
names(DQ_Data)[4]<-"Number of Visits"

#subset 
learning_set<-subset(DQ_Data,source==0)
set1<-subset(DQ_Data,source==1)
set2<-subset(DQ_Data,source==2)
set3<-subset(DQ_Data,source==3)

#make sure the sets don't have any data irrelevant to the learning set
set1<-subset(set1, set1$site %in% learning_set$site)
set2<-subset(set2, set2$site %in% learning_set$site)
set3<-subset(set3, set3$site %in% learning_set$site)




#subsetting learning set by each data set
learning1<-subset(learning_set, learning_set$site %in% set1$site)
learning1<-subset(learning_set, learning_set$site %in% set1$site)
learning2<-subset(learning_set, learning_set$site %in% set2$site)
learning3<-subset(learning_set, learning_set$site %in% set3$site)


#Initial linear regression for set 1 visits
visits1<-data.frame(learning = learning1$`Number of Visits`, set = set1$`Number of Visits`)
visitFit1<-lm(set~learning, data = visits1)
summary(visitFit1)$coefficients

#Initial linear regression for set 1 uniques
unique1<-data.frame(learning = learning1$`Percent Unique`, set = set1$`Percent Unique`)
uniqueFit1<-lm(set~learning, data = visits1)
summary(uniqueFit1)$coefficients


#Initial linear regression for set 2 visits
visits2<-data.frame(learning = learning2$`Number of Visits`, set = set2$`Number of Visits`)
visitFit2<-lm(set~learning, data = visits2)
summary(visitFit2)$coefficients

#Initial linear regression for set 2 uniques
unique2<-data.frame(learning = learning2$`Percent Unique`, set = set2$`Percent Unique`)
uniqueFit2<-lm(set~learning, data = visits2)
summary(uniqueFit2)$coefficients

#Initial linear regression for set 3 visits
visits3<-data.frame(learning = learning3$`Number of Visits`, set = set3$`Number of Visits`)
visitFit3<-lm(set~learning, data = visits3)
summary(visitFit3)$coefficients

#Initial linear regression for set 3 uniques
unique2<-data.frame(learning = learning3$`Percent Unique`, set = set3$`Percent Unique`)
uniqueFit3<-lm(set~learning, data = visits3)
summary(uniqueFit3)$coefficients

#cleaning outliers

#Getting rid of outliers
#for (i in 1:length(visi))

