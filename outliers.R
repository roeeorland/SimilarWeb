too_far<-c()

outliers<-c()
for(i in 1:length(visits1$learning))
{
        
        
        prediction<-predict(lm(set ~ learning, data = visits1),  newdata = data.frame(learning = i), level = 0.99, interval = "prediction")
        if (visits1$set[i]<prediction[2]|| visits1$set[i]>prediction[3])
        {
                outliers<-c(outliers, i)
                
        }
}
