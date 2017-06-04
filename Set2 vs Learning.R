#initial scatterplot

g<-ggplot(set2, aes(y = NoV, x = LNoV))
g+geom_point(col = "red", alpha = 0.2)+labs(y = "Set 2 No of Visits", x = "Learning Set No of Visits")
dev.copy(png,file = "Learning v Set 2 No of Visits Scatterplot.png", height = 480, width = 480, units = "px")
dev.off()


qplot(LNoV, data = set2)
dev.copy(png,file = "Learning v Set 2 No of Visits Histogram.png", height = 480, width = 480, units = "px")
dev.off()

set2_clustered<-set2[clustering(set2$LNoV, set2$NoV),]



#histogram of clustered data


qplot(LNoV, data = set2_clustered)

NoV_set2_v_learningSet<-set2_clustered[-Outliers(set2_clustered$NoV,set2_clustered$LNoV),]
PU_set2_v_learningSet<-set2_clustered[-Outliers(set2$NoV,set2$PU),]


#linear regression of learning set vs. test set

plotLinearRegression(NoV_set2_v_learningSet$LNoV, NoV_set2_v_learningSet$NoV)+labs(y="Set 2 No of Visits", x="Learning Set No of Visits")
dev.copy(png,file = "Learning v Set 2 No of Visits Scatterplot clustered.png", height = 480, width = 480, units = "px")
dev.off()


#linear regression of test set variables

plotLinearRegression(PU_set2_v_learningSet$NoV, PU_set2_v_learningSet$PU)+labs(y="Set 2 Percent Unique", x="Set 2 No of Visits")
dev.copy(png,file = "Set 2 No of Visits vs PU Scatterplot clustered.png", height = 480, width = 480, units = "px")
dev.off()


#linear regression coefficients

fitset2<-lm(PU~NoV, data = set2_clustered)

fitset2vsLearning<-lm(NoV~LNoV, data = set2_clustered)






