
#Learning Set Analysis

g<-ggplot(set3, aes(x = NoV, y = PU))
g<-g+geom_point(col = "blue", alpha = 0.2)+labs(x = "No of Visits", y = "Percent Unique", title = "Raw  Set")
g #outliers on NoV
dev.copy(png,file = "Raw set3.png", height = 480, width = 480, units = "px")
dev.off()

#Data seems limited to under 1e8

#Histogram of Visit count

q2<-qplot(NoV, data = set3,xlab = "Number of Visits", bins = 10)
q2
dev.copy(png,file = "set3 No of Visits Histogram.png", height = 480, width = 480, units = "px")
dev.off()
#results: very compact

length(set3[set3$NoV>2e7,]$NoV)/length(set3$NoV) #only 1% of NoV 1e7 or larger

q3<-qplot(NoV, data = set3[set3$NoV<2e7, ],xlab = "Number of Visits", bins = 10)
q3
dev.copy(png,file = "set3 Short Histogram.png", height = 480, width = 480, units = "px")
dev.off()
set3_1e7<-subset(set3, NoV<2e7)

plotLinearRegression(set3_1e7$NoV, set3_1e7$PU)+labs(x="No of Visits", y="Percent Unique")
dev.copy(png,file = "set3 Short NoV v PU.png", height = 480, width = 480, units = "px")
dev.off()

set3_1e7Clean<-set3_1e7[-Outliers(set3_1e7$NoV,set3_1e7$PU),]

plotLinearRegression(set3_1e7Clean$NoV, set3_1e7Clean$PU)+labs(x="No of Visits", y="Percent Unique")
dev.copy(png,file = "set3 NoV v PU Dropped outliers.png", height = 480, width = 480, units = "px")
dev.off()
set3Fit<-lm(PU~NoV, data = set3_1e7Clean)
#q4<-qplot(NoV, PU, data = set3_1e7, ylab = "Percent Unique", xlab = "Number of Visits")




