rm(list = ls())
gc()

source('functions.R')
dev.off()
#load packages
library(dplyr)
library(ggplot2)

#Import data
DQ_Data<-read.table(file = 'DQ_Data_b.tsv', sep = '\t', header = TRUE)
DQ_Data<-arrange(DQ_Data, site)
head(DQ_Data)

#Names refined
names(DQ_Data)[3]<-"PU"
names(DQ_Data)[4]<-"NoV"

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
learning2<-subset(learning_set, learning_set$site %in% set2$site)
learning3<-subset(learning_set, learning_set$site %in% set3$site)


#attaching the learning set data to relevant test sets
set1$LPU<-learning1$PU
set2$LPU<-learning2$PU
set3$LPU<-learning3$PU

set1$LNoV<-learning1$NoV
set2$LNoV<-learning2$NoV
set3$LNoV<-learning3$NoV





#Learning Set Analysis


g<-ggplot(learning_set, aes(x = NoV, y = PU))
g<-g+geom_point(col = "blue", alpha = 0.2)+labs(x = "No of Visits", y = "Percent Unique", title = "Raw Learning Set")
g 
dev.copy(png,file = "Raw Learning Set.png", height = 480, width = 480, units = "px")
dev.off()

#Data seems limited to under 1e8

#Histogram of Visit count

q2<-qplot(NoV, data = learning_set,xlab = "Number of Visits", bins = 10)
q2
dev.copy(png,file = "Raw Learning No of Visits Histogram.png", height = 480, width = 480, units = "px")
dev.off()


#clustering of learning set by k-means
learning_set_clustered<-learning_set[clustering(learning_set$NoV, learning_set$PU),]

#Histogram of post-clustering learning set

q2<-qplot(NoV, data = learning_set_clustered,xlab = "Number of Visits", bins = 10)
q2
dev.copy(png,file = "Learning set after clustering Histogram.png", height = 480, width = 480, units = "px")
dev.off()



#Regression plot. Red lines signify confidence interval

plotLinearRegression(learning_set_clustered$NoV, learning_set_clustered$PU)+labs(x="No of Visits", y="Percent Unique")
dev.copy(png,file = "Learning No of Visits with Regression.png", height = 480, width = 480, units = "px")
dev.off()
fitLearning<-lm(PU~NoV, data = learning_set_clustered)
