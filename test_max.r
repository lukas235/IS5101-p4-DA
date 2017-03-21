id<-c(1,1,2,3,3)
date<-c("23-01-08","01-11-07","30-11-07","17-12-07","12-12-08")
df<-data.frame(id,date)
df$date2<-as.Date(as.character(df$date), format = "%d-%m-%y")
# aggregate can be used for this type of thing
d = aggregate(df$date2,by=list(df$id),max)
# And merge the result of aggregate 
# with the original data frame
df2 = merge(df,d,by.x=1,by.y=1)
df2
d