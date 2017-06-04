#initial scatterplot

g<-ggplot(set1, aes(y = NoV, x = LNoV))
g+geom_point(col = "red", alpha = 0.2)+labs(y = "Set 1 No of Visits", x = "Learning Set No of Visits")
dev.copy(png,file = "Learning v Set 1 No of Visits Scatterplot.png", height = 480, width = 480, units = "px")
dev.off()


qplot(LNoV, data = set1)
dev.copy(png,file = "Learning v Set 1 No of Visits Histogram.png", height = 480, width = 480, units = "px")
dev.off()

#clustering

set1_clustered<-set1[clustering(set1$LNoV, set1$NoV),]



#histogram of clustered data

qplot(LNoV, data = set1_clustered)

NoV_set1_v_learningSet<-set1_clustered[-Outliers(set1$LNoV,set1$NoV),]
PU_set1_v_learningSet<-set1_clustered[-Outliers(set1$NoV,set1$PU),]

#linear regression of learning set vs. test set

plotLinearRegression(NoV_set1_v_learningSet$LNoV, NoV_set1_v_learningSet$NoV)+labs(y="Set 1 No of Visits", x="Learning Set No of Visits")
dev.copy(png,file = "Learning v Set 1 No of Visits Scatterplot clustered.png", height = 480, width = 480, units = "px")
dev.off()


#linear regression of test set variables

plotLinearRegression(PU_set1_v_learningSet$NoV, PU_set1_v_learningSet$PU)+labs(y="Set 1 Percent Unique", x="Set 1 No of Visits")
dev.copy(png,file = "Set 1 No of Visits vs PU Scatterplot clustered.png", height = 480, width = 480, units = "px")
dev.off()


#linear regression coefficients

fitSet1<-lm(PU~NoV, data = set1_clustered)

fitSet1vsLearning<-lm(NoV~LNoV, data = set1_clustered)




