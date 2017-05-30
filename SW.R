rm(list = ls())
gc()

#load packages
library(dplyr)

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



