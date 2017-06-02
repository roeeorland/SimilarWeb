rm(list = ls())
gc()

#load packages
library(dplyr)
library(ggplot2)

#Import data
DQ_Data<-read.table(file = 'DQ_Data_b.tsv', sep = '\t', header = TRUE)

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
learning1<-subset(learning_set, learning_set$site %in% set1$site)
learning2<-subset(learning_set, learning_set$site %in% set2$site)
learning3<-subset(learning_set, learning_set$site %in% set3$site)

set1<-arrange(set1, site)
learning1<-arrange(learning1, site)
set1$LPU<-learning1$PU
set1$LNoV<-learning1$NoV

pred1<-predict(lm(NoV ~ LNoV, data = set1), newdata = data.frame(LNoV=set1$LNoV),level = 0.95, interval = "prediction")
plot(set1$LNoV, set1$NoV, col = "blue",xlab = "Learning set Number of Visits", ylab = "Test set Number of Visits")
ord<-order(set1$LNoV)
matlines(set1$LNoV[ord], pred1[ord,], col = c(1,2,2))

#Cleaning out outliers
outlier<-c()
for (j in 1:length(set1$LNoV) )
{
        if (set1$LNoV[j]<pred1[j,2] | set1$LNoV[j]>pred1[j,3])
        {
                outlier<-c(outlier, j)
                
        }
}


set1_reduced_NoV_outliers<-set1[-outlier,]
pred1<-predict(lm(NoV ~ LNoV, data = set1_reduced_NoV_outliers), newdata = data.frame(LNoV=set1_reduced_NoV_outliers$LNoV),level = 0.95, interval = "prediction")
plot(set1_reduced_NoV_outliers$LNoV, set1_reduced_NoV_outliers$NoV, col = "blue",xlab = "Learning set Number of Visits", ylab = "Test set Number of Visits")
ord<-order(set1_reduced_NoV_outliers$LNoV)
matlines(set1_reduced_NoV_outliers$LNoV[ord], pred1[ord,], col = c(1,2,2))
fit <- lm(NoV ~ LNoV, data = set1_reduced_NoV_outliers)
summary(fit)$coefficients

