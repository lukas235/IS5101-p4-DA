#!/usr/bin/env Rscript
require(ggplot2)
library(ggplot2)
# require(plyr)
# Read in data
data <- read.csv("datafile.csv")

n <- max(data$pid)

f <- length(subset(data, fid == 1 & map == 'MapA' & gender == 'f')$gender)
m <- length(subset(data, fid == 1 & map == 'MapA' & gender == 'm')$gender)
o <- length(subset(data, fid == 1 & map == 'MapA' & gender == 'o')$gender)

mapa <- subset(data, map == 'MapA')
mapb <- subset(data, map == 'MapB')

# Pie Chart with Percentages of genders
slices <- c(f,m,o)
lbls <- c('female','male','n/a')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, ' (', slices, ' person(s))', sep="")
lbls <- paste(lbls, "\n", pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Gender distribution")

# Calculate age mean, sd, range, bandwidth and plot
age <- mean(subset(data, fid == 1 & map == 'MapA')$age)
age_sd <- sd(subset(data, fid == 1 & map == 'MapA')$age)
age_range <-range(subset(data, fid == 1 & map == 'MapA')$age)
age_diff = age_range[2] - age_range[1]
hist(subset(data, fid == 1 & map == 'MapA')$age, xlab="Age (years)", ylab="Count")


# Nationalities
mytable <- table(subset(data, fid == 1 & map =='MapA')$nationality)

lbls <- paste(names(mytable), " (", mytable, " person(s))", "\n", as.vector(mytable)/sum(as.vector(mytable))*100, "%", sep="")
pie(mytable, labels = lbls, 
    main="Nationalities\n (7 different countries)")


# Mean and SD of the questions answered with each map system
faults_mapa <- aggregate(mapa$fid,by=list(mapa$pid), max, na.rm=TRUE)
faults_mapb <- aggregate(mapb$fid,by=list(mapb$pid), max, na.rm=TRUE)

mean_f_mapa <- mean(faults_mapa$x, na.rm=TRUE)
mean_f_mapb <- mean(faults_mapb$x, na.rm=TRUE)

sd_f_mapa <- sd(faults_mapa$x, na.rm=TRUE)
sd_f_mapb <- sd(faults_mapb$x, na.rm=TRUE)

# Hyp 1 calculate mean & sd of times locating faults and conduct t- as well as F-test:

# We can assume normal dist
mapa_loc_time <-subset(data, map == 'MapA', na.rm=TRUE)$f.time
hist(mapa_loc_time, main="Distribution of finding times for MapA", xlab="Time in sec to locate a fault", breaks = 20)
curve(dnorm(x, mean=mean(mapa_loc_time), sd=sd(mapa_loc_time)), add=TRUE)

plot(density(mapa_loc_time),main="Density estimate of data", lty="dotted")
curve(dnorm(x, mean=mean(mapa_loc_time), sd=sd(mapa_loc_time)), add=TRUE)

# chisq.test(density(mapa_loc_time), dnorm(x, mean=mean(mapa_loc_time), sd=sd(mapa_loc_time)))

mapb_loc_time <-subset(data, map == 'MapB', na.rm=TRUE)$f.time
hist(mapb_loc_time, main="Distribution of finding times for MapB", xlab="Time in sec to locate a fault", breaks = 20)
curve(dnorm(x, mean=mean(mapb_loc_time), sd=sd(mapb_loc_time)), add=TRUE)

# Means & StdDevs
mean_mapa_loc_time = mean(mapa_loc_time, na.rm=TRUE)
sd_mapa_loc_time = sd(mapa_loc_time, na.rm=TRUE)

mean_mapb_loc_time = mean(mapb_loc_time, na.rm=TRUE)
sd_mapb_loc_time = sd(mapb_loc_time, na.rm=TRUE)

# Tests
t.test(mapa_loc_time, mapb_loc_time)
var.test(mapa_loc_time, mapb_loc_time)



# Hyp 2 calculate mean & sd of times typing faults and conduct t- as well as F-test:
mapa_typ_time <-subset(data, map == 'MapA', na.rm=TRUE)$t.time
hist(mapa_typ_time, main="Distribution of typing times for MapA", xlab="Time in sec to type in a fault", breaks = 20)
# curve(dnorm(x, mean=mean(mapa_typ_time), sd=sd(mapb_typ_time)), add=TRUE)
# lines(density(mapa_typ_time))             # add a density estimate with defaults
# lines(density(mapa_typ_time), lty="dotted")
curve(max(mapa_typ_time)*dnorm(x, mean=mean(mapa_typ_time), sd=sd(mapa_typ_time)), add=TRUE)

mapb_typ_time <-subset(data, map == 'MapB', na.rm=TRUE)$t.time
hist(mapb_typ_time, main="Distribution of typing times for MapB", xlab="Time in sec to type a fault", breaks = 20)
# curve(dnorm(x, mean=mean(mapb_typ_time), sd=sd(mapb_typ_time)), add=TRUE)
plot(density(mapb_typ_time),main="Density estimate of data", lty="dotted")
curve(dnorm(x, mean=mean(mapb_typ_time), sd=sd(mapb_typ_time)), add=TRUE)

plot(ecdf(mapb_typ_time),main="Empirical cumulative distribution function")

# range(mapb_typ_time)
# breaks = seq(1.0, 3, by=0.1)
# mapb_typ_time.cut = cut(mapb_typ_time, breaks)
# mapb_typ_time.cut = mapb_typ_time.cut
# mapb_typ_time.freq = table(mapb_typ_time.cut)
# 
# plot(mapb_typ_time.cut)

mean_mapa_typ_time = mean(mapa_typ_time, na.rm=TRUE)
sd_mapa_typ_time = sd(mapa_typ_time, na.rm=TRUE)

mean_mapb_typ_time = mean(mapb_typ_time, na.rm=TRUE)
sd_mapb_typ_time = sd(mapb_typ_time, na.rm=TRUE)

t2 <- t.test(mapa_typ_time, mapb_typ_time)
v2 <- var.test(mapa_typ_time, mapb_typ_time)

t.test(mapa_typ_time, mapb_typ_time)
var.test(mapa_typ_time, mapb_typ_time)

# xd <- subset(data, map == 'MapA', na.rm=TRUE)$t.time
# h<-hist(xd, breaks=20, col="grey", xlab="Time in seconds", main="Distribution of typing times for MapA") 
# xfit<-seq(min(xd),max(xd),length=3200) 
# yfit<-dnorm(xfit,mean=mean(xd),sd=sd(xd)) 
# yfit <- yfit*diff(h$mids[1:2])*length(xd)

xd <- subset(data, map == 'MapA', na.rm=TRUE)$t.time
dens<-density(xd)
dens$y <-dens$y/10
h<-hist(xd, breaks=20, col="grey", xlab="Time in seconds", main="Distribution of typing times for MapA")
# h<-hist(x, plot=F)
h$counts <- h$counts / sum(h$counts)
plot(h, freq=TRUE, ylab="Relative Frequency", xlab="Time in seconds", main="Distribution of typing times for MapA", col="grey")
xfit<-seq(min(xd),max(xd),length=3200) 
yfit<-dnorm(xfit,mean=mean(xd),sd=sd(xd)) 
yfit <- yfit*diff(h$mids[1:2])
lines(dens, lty="dotted",lwd=2, col="red")
lines(xfit, yfit, col="blue", lwd=2)

# plot( mapa$lat, mapa$long, main="Latitude against Longitude" )
# plot( mapb$lat, mapb$long, main="Latitude against Longitude" )

# # loading the required packages
# library(ggplot2)
# library(ggmap)
# 
# # creating a sample data.frame with your lat/lon points
# lon <- data$long
# lat <- data$lat
# correct.lon <- data$correct.long
# correct.lat <- data$correct.lat
# df <- as.data.frame(cbind(lon,lat))
# df2 <-as.data.frame(cbind(correct.lon,correct.lat))
# 
# # getting the map
# mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 10,
#                       maptype = "satellite", scale = 2)
# 
# # plotting the map with some points on it
# ggmap(mapgilbert) +
#   geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 1), size = 5, shape = 15, fill = "red") +
#   geom_point(data = df2, aes(x = correct.lon, y = correct.lat, fill = "red", alpha = 0.8), size = 5, shape = 21, alpha = 0.8) +
#   guides(fill=FALSE, alpha=FALSE, size=FALSE)

ma = sqrt((mapa$lat-mapa$correct.lat)^2 + (mapa$long-mapa$correct.long)^2)
mapa_geom_err <- as.data.frame(cbind(mapa, ma))

hist(mapa_geom_err$ma, breaks = 20)

# boxplot anova

ggplot(data, aes(x=f.time, y=t.time)) + geom_point(size=1, shape=1, color="steelblue", stroke=1)
ggplot(data, aes(x=f.time)) + geom_histogram(size=2, fill=3, color="red", binwidth = 1) + geom_density(kernel="gaussian")
ggplot(data, aes(f.time)) + geom_density(size=1, fill=3, color="red") + geom_density(kernel="gaussian") + labs(title="Density plot")  # Density plot
ggplot(mapa, aes(x=age)) + geom_bar()

ggplot(mapa, aes(x=f.time)) +
  geom_histogram(aes(y=..density..), alpha=0.5,position='identity',binwidth=0.5) +
  geom_density(aes(y=..density..,position="stack")) +
  stat_function(fun = dnorm, args = list(mean = mean_mapa_loc_time, sd = sd_mapa_loc_time))
