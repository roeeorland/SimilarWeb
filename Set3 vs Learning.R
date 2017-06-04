#initial scatterplot

g<-ggplot(set3, aes(y = NoV, x = LNoV))
g+geom_point(col = "red", alpha = 0.2)+labs(y = "Set 3 No of Visits", x = "Learning Set No of Visits")
dev.copy(png,file = "Learning v Set 3 No of Visits Scatterplot.png", height = 480, width = 480, units = "px")
dev.off()


qplot(LNoV, data = set3)
dev.copy(png,file = "Learning v Set 3 No of Visits Histogram.png", height = 480, width = 480, units = "px")
dev.off()

set3_clustered<-set3[clustering(set3$LNoV, set3$NoV),]


#histogram of clustered data

qplot(LNoV, data = set3_clustered)

NoV_set3_v_learningSet<-set3_clustered[-Outliers(set3_clustered$NoV,set3_clustered$LNoV),]
PU_set3_v_learningSet<-set3_clustered[-Outliers(set3$NoV,set3$PU),]


#linear regression of learning set vs. test set

plotLinearRegression(NoV_set3_v_learningSet$LNoV, NoV_set3_v_learningSet$NoV)+labs(y="Set 3 No of Visits", x="Learning Set No of Visits")
dev.copy(png,file = "Learning v Set 3 No of Visits Scatterplot clustered.png", height = 480, width = 480, units = "px")
dev.off()


#linear regression of test set variables

plotLinearRegression(PU_set3_v_learningSet$NoV, PU_set3_v_learningSet$PU)+labs(y="Set 1 Percent Unique", x="Set 1 No of Visits")
dev.copy(png,file = "Set 3 No of Visits vs PU Scatterplot clustered.png", height = 480, width = 480, units = "px")
dev.off()


#linear regression coefficients

fitset3<-lm(PU~NoV, data = set3_clustered)

fitset3vsLearning<-lm(NoV~LNoV, data = set3_clustered)




