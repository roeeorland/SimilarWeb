
#initial plotting
library(ggplot2)
g<-ggplot2(lm(set1$`Number of Visits` ~ learning1$`Number of Visits`))
g<-ggplot(lm(set1$`Number of Visits` ~ learning1$`Number of Visits`))
g
g<-g+geom_point(size = 5, color = "black", alpha = 0.5)
g
g<-ggplot(lm(set1$`Number of Visits` ~ learning1$`Number of Visits`), aes(x=learning1$`Number of Visits`, y=set1$`Number of Visits`))
g<-g+geom_point(size = 5, color = "black", alpha = 0.5)
g
g<-g+xlab("Learning Set Visits")
g<-g+ylab("Set 1 Visits")
g
g<-g+geom_smooth(method = "lm", color = "blue")
g
coef(lm(set1$`Number of Visits` ~ learning1$`Number of Visits`))
