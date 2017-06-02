
mergeSets<-function(testSet,learningSet)
{
        
        testSet<-subset(testSet, testSet$site %in% learningSet$site)
        learningSet<-subset(learningSet, learningSet$site %in% testSet$site)
        testSet<-arrange(testSet, site)
        learningSet<-arrange(learningSet, site)
        testSet$LPU<-learningSet$PU
        testSet$LNoV<-learningSet$NoV
        testSet
        
}


removeOutliers<-function(testSet)
{
        
        
        pred1<-predict(lm(NoV ~ LNoV, data = testSet), newdata = data.frame(LNoV=testSet$LNoV),level = 0.95, interval = "prediction")
        plot(testSet$LNoV, testSet$NoV, col = "blue",xlab = "Learning set Number of Visits", ylab = "Test set Number of Visits")
        ord<-order(testSet$LNoV)
        matlines(testSet$LNoV[ord], pred1[ord,], col = c(1,2,2))
        
        #Cleaning out outliers
        outlier<-c()
        for (j in 1:length(testSet$LNoV) )
        {
                if (testSet$LNoV[j]<pred1[j,2] | testSet$LNoV[j]>pred1[j,3])
                {
                        outlier<-c(outlier, j)
                        
                }
        }
        
        
        testSet_reduced_NoV_outliers<-testSet[-outlier,]
        pred1<-predict(lm(NoV ~ LNoV, data = testSet_reduced_NoV_outliers), newdata = data.frame(LNoV=testSet_reduced_NoV_outliers$LNoV),level = 0.95, interval = "prediction")
        plot(testSet_reduced_NoV_outliers$LNoV, testSet_reduced_NoV_outliers$NoV, col = "blue",xlab = "Learning set Number of Visits", ylab = "Test set Number of Visits")
        ord<-order(testSet_reduced_NoV_outliers$LNoV)
        matlines(testSet_reduced_NoV_outliers$LNoV[ord], pred1[ord,], col = c(1,2,2))
        fit <- lm(NoV ~ LNoV, data = testSet_reduced_NoV_outliers)
        #summary(fit)$coefficients
        testSet_reduced_NoV_outliers
        
        
        
        
}
        