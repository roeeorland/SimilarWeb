
#Learning Set Analysis

g<-ggplot(set2, aes(x = NoV, y = PU))
g<-g+geom_point(col = "blue", alpha = 0.2)+labs(x = "No of Visits", y = "Percent Unique", title = "Raw  Set")
g #outliers on NoV
dev.copy(png,file = "Raw set2.png", height = 480, width = 480, units = "px")
dev.off()

#Data seems limited to under 1e8

#Histogram of Visit count

q2<-qplot(NoV, data = set2,xlab = "Number of Visits", bins = 10)
q2
dev.copy(png,file = "Set2 No of Visits Histogram.png", height = 480, width = 480, units = "px")
dev.off()
#results: very compact

length(set2[set2$NoV>2e7,]$NoV)/length(set2$NoV) #only 1% of NoV 1e7 or larger

q3<-qplot(NoV, data = set2[set2$NoV<2e7, ],xlab = "Number of Visits", bins = 10)
q3
dev.copy(png,file = "set2 Short Histogram.png", height = 480, width = 480, units = "px")
dev.off()
set2_1e7<-subset(set2, NoV<2e7)

plotLinearRegression(set2_1e7$NoV, set2_1e7$PU)+labs(x="No of Visits", y="Percent Unique")
dev.copy(png,file = "Set2 Short NoV v PU.png", height = 480, width = 480, units = "px")
dev.off()

set2_1e7Clean<-set2_1e7[-Outliers(set2_1e7$NoV,set2_1e7$PU),]

plotLinearRegression(set2_1e7Clean$NoV, set2_1e7Clean$PU)+labs(x="No of Visits", y="Percent Unique")
dev.copy(png,file = "set2 NoV v PU Dropped outliers.png", height = 480, width = 480, units = "px")
dev.off()
set2Fit<-lm(PU~NoV, data = set2_1e7Clean)
#q4<-qplot(NoV, PU, data = set2_1e7, ylab = "Percent Unique", xlab = "Number of Visits")




