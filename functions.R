
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


plotLinearRegression<-function(x,y)
{
        fit<-lm(y~x)
         pred1<-predict(fit, level = 0.95, interval = "prediction")
         fit_lower<-lm(pred1[,2]~x)
        fit_upper<-lm(pred1[,3]~x)

        g<-ggplot(fit, aes(x=x, y=y))
        g+geom_point(col = "blue", alpha = 0.2)+geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2], color = "blue" )+
                geom_abline(intercept = fit_lower$coefficients[1], slope = fit_lower$coefficients[2], color = "red" )+
                geom_abline(intercept = fit_upper$coefficients[1], slope = fit_upper$coefficients[2], color = "red" )




}



Outliers<-function(x,y)
{
        
        
        pred1<-predict(lm(y ~ x), newdata = data.frame(x=x),level = 0.95, interval = "prediction")
        
        
        #Getting outlier indices
        outlier<-c()
        for (j in 1:length(x) )
        {
                if (y[j]<pred1[j,2] | y[j]>pred1[j,3])
                {
                        outlier<-c(outlier, j)
                        
                }
        }
        
        
        outlier
        
        
        
        
        
}



clustering<-function(x,y)
{
        kframe<-cbind(x,y,1:length(x))
        km<-kmeans(kframe, centers = 2)
        k_index<-1:length(x)
        if (length(km$cluster[km$cluster==1])/length(km$cluster)>0.95)
        {
                kframe<-kframe[km$cluster==1,] 
        }
        
        if (length(km$cluster[km$cluster==2])/length(km$cluster)>0.95)
        {
                kframe<-kframe[km$cluster==2,] 
        }  
        
        kframe[,3]
        
}

        