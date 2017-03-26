#!/usr/bin/env Rscript
require(ggplot2)
require(stringdist)
require(stringi)
require(ggmap)
require(gridExtra)

# if(!require(rcompanion)){install.packages("rcompanion")}

library(ggplot2)
library(stringdist)
library(stringi)
library(ggmap)
library(gridExtra)

# 1-10: MapA MapB
# 11-20: MapB MapA
learned <- function(pid, map){
  return ((pid <= 10 & map == "MapB")|(pid >10 & map == "MapA"))
}

# Read in data
data <- read.csv("datafile.csv")

summary(data)

# Add location error, string distances and boolean for learning-effect to the data structure
data["loc.err"] <- sqrt((data$lat-data$correct.lat)^2 + (data$long-data$correct.long)^2)
data["dist"] <- stringdist(data$text, data$correct.text, method="lv") / stri_length(data$correct.text)
data["learned"] <- learned(data$pid, data$map)

# Number of participants
max(data$pid)

f <- length(subset(data, fid == 1 & map == 'MapA' & gender == 'f')$gender)
m <- length(subset(data, fid == 1 & map == 'MapA' & gender == 'm')$gender)
o <- length(subset(data, fid == 1 & map == 'MapA' & gender == 'o')$gender)

# Split the table into subsets of MapA and MapB
mapa <- subset(data, map == 'MapA')
mapb <- subset(data, map == 'MapB')

# Difference in reported faults between A and B
nrow(mapa) - nrow(mapb)


# Pie Chart with Percentages of genders
slices <- c(f,m,o)
lbls <- c('female','male','n/a')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, ' (', slices, ' person(s))', sep="")
lbls <- paste(lbls, "\n", pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls) # main="Gender distribution"

# Calculate age mean, sd, range, bandwidth and plot of age
age <- mean(subset(data, fid == 1 & map == 'MapA')$age)
age_md <- median(subset(data, fid == 1 & map == 'MapA')$age)
age_sd <- sd(subset(data, fid == 1 & map == 'MapA')$age)
age_range <-range(subset(data, fid == 1 & map == 'MapA')$age)
age_diff = age_range[2] - age_range[1]
hist(subset(data, fid == 1 & map == 'MapA')$age, xlab="Age (years)", ylab="Count", breaks = 30, col = "grey", main="Age distribution")


# Nationalities
mytable <- table(subset(data, fid == 1 & map =='MapA')$nationality)
lbls <- paste(names(mytable), " (", mytable, " person(s))", "\n", as.vector(mytable)/sum(as.vector(mytable))*100, "%", sep="")
pie(mytable, labels = lbls)


# Mean and SD of the questions answered with each map system
faults_mapa <- aggregate(mapa$fid,by=list(mapa$pid), max, na.rm=TRUE)
faults_mapb <- aggregate(mapb$fid,by=list(mapb$pid), max, na.rm=TRUE)

# fff <- rbind(faults_mapa, faults_mapb)

mean(faults_mapa$x, na.rm=TRUE)
mean(faults_mapb$x, na.rm=TRUE)

faults <- faults_mapa + faults_mapb
mean(faults$x, na.rm=TRUE)

sd_f_mapa <- sd(faults_mapa$x, na.rm=TRUE)
sd_f_mapb <- sd(faults_mapb$x, na.rm=TRUE)
sd_f <- mean(faults$x, na.rm=TRUE)


# Hyp 1 calculate mean & sd of times locating faults and conduct t- as well as F-test:

# Means & StdDevs
mean(mapa$f.time, na.rm=TRUE)
sd(mapa$f.time, na.rm=TRUE)

mean(mapb$f.time, na.rm=TRUE)
sd(mapb$f.time, na.rm=TRUE)

# We can assume normal dist -> plots
mapa.ttime.nd <- ggplot(mapa, aes(x=f.time)) +
  geom_histogram(aes(y=..density..), alpha=0.5,position='identity',binwidth=0.5, colour="black") +
  geom_density(aes(y=..density..), alpha=0.2, fill="red", colour="red", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(mapa$f.time, na.rm=TRUE), sd = sd(mapa$f.time, na.rm=TRUE)), colour="blue", size=1) + 
  labs(title="Distribution of f.time (MapA)") + theme(plot.title=element_text(face="bold", color="black"))

mapb.ttime.nd <- ggplot(mapb, aes(x=f.time)) +
  geom_histogram(aes(y=..density..), alpha=0.5,position='identity',binwidth=0.5, colour="black") +
  geom_density(aes(y=..density..), alpha=0.2, fill="red", colour="red", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(mapb$f.time, na.rm=TRUE), sd = sd(mapb$f.time, na.rm=TRUE)), colour="blue", size=1) +
  labs(title="Distribution of f.time (MapB)") + theme(plot.title=element_text(face="bold", color="black"))

grid.arrange(mapa.ttime.nd, mapb.ttime.nd, ncol=2)  # arrange

# Tests
t.test(mapa$f.time , mapb$f.time)
var.test(mapa$f.time , mapb$f.time)



# Hyp 2 calculate mean & sd of times typing faults and conduct t- as well as F-test:

# Means & StdDevs
mean(mapa$t.time, na.rm=TRUE)
sd(mapa$t.time, na.rm=TRUE)

mean(mapb$t.time, na.rm=TRUE)
sd(mapb$t.time, na.rm=TRUE)

# We can assume normal dist -> plots
mapa.ftime.nd <- ggplot(mapa, aes(x=t.time)) +
  geom_histogram(aes(y=..density..), alpha=0.5,position='identity',binwidth=0.1, colour="black") +
  geom_density(aes(y=..density..), alpha=0.2, fill="red", colour="red", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(mapa$t.time, na.rm=TRUE), sd = sd(mapa$t.time, na.rm=TRUE)), colour="blue", size=1) + 
  labs(title="Distribution of t.time (MapA)") + theme(plot.title=element_text(face="bold", color="black"))

mapb.ftime.nd <- ggplot(mapb, aes(x=t.time)) +
  geom_histogram(aes(y=..density..), alpha=0.5,position='identity',binwidth=0.1, colour="black") +
  geom_density(aes(y=..density..), alpha=0.2, fill="red", colour="red", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(mapb$t.time, na.rm=TRUE), sd = sd(mapb$t.time, na.rm=TRUE)), colour="blue", size=1) +
  labs(title="Distribution of t.time (MapB)") + theme(plot.title=element_text(face="bold", color="black"))

grid.arrange(mapa.ftime.nd, mapb.ftime.nd, ncol=2)  # arrange

# Tests
t.test(mapa$t.time , mapb$t.time)
var.test(mapa$t.time , mapb$t.time)



# xd <- subset(data, map == 'MapA', na.rm=TRUE)$t.time
# dens<-density(xd)
# dens$y <-dens$y/10
# h<-hist(xd, breaks=20, col="grey", xlab="Time in seconds", main="Distribution of typing times for MapA")
# # h<-hist(x, plot=F)
# h$counts <- h$counts / sum(h$counts)
# plot(h, freq=TRUE, ylab="Relative Frequency", xlab="Time in seconds", main="Distribution of typing times for MapA", col="grey")
# xfit<-seq(min(xd),max(xd),length=3200) 
# yfit<-dnorm(xfit,mean=mean(xd),sd=sd(xd)) 
# yfit <- yfit*diff(h$mids[1:2])
# lines(dens, lty="dotted",lwd=2, col="red")
# lines(xfit, yfit, col="blue", lwd=2)

# plot( mapa$lat, mapa$long, main="Latitude against Longitude" )
# plot( mapb$lat, mapb$long, main="Latitude against Longitude" )


# Show coordinates on the map
lon <- data$long
lat <- data$lat
correct.lon <- data$correct.long
correct.lat <- data$correct.lat
df <- as.data.frame(cbind(lon,lat))
df2 <-as.data.frame(cbind(correct.lon,correct.lat))

mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 10,
                      maptype = "satellite", scale = 2)

ggmap(mapgilbert) +
  geom_point(data = df2, aes(x = correct.lon, y = correct.lat, fill = 100, alpha = 0.1), size = 5, shape = 21, alpha = 0.1) +
  geom_point(data = df, aes(x = lon, y = lat, fill = 1, alpha = 1), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# Analysis of localisation error
ggplot(data, aes(x=loc.err)) +
  geom_density(aes(group=map,colour=map, fill=map), alpha=0.2)

t.test(mapa$loc.err, mapb$loc.err)
var.test(mapa$loc.err, mapb$loc.err)
wilcox.test(mapa$loc.err, mapb$loc.err)
kruskal.test(loc.err ~ map, data = data)

median(mapa$loc.err)
median(mapb$loc.err)


# Analysis of the typing error
ggplot(data, aes(x=dist)) +
  geom_histogram(aes(y=..density.., group=map,colour=map,fill=map), alpha=0.5,position='identity') +
  geom_density(aes(group=map,colour=map, fill=map), alpha=0.2)

t.test(mapa$dist, mapb$dist)
var.test(mapa$dist, mapb$dist)
wilcox.test(mapa$dist, mapb$dist)
kruskal.test(dist ~ map, data = data)

median(mapa$dist)
median(mapb$dist)



## Correlation between times
ggplot(data, aes(data$f.time, data$t.time, colour=data$map)) +
  geom_point() +
  geom_smooth(method = "lm")

# cor.test(data$f.time,data$t.time)
cor.test(mapa$f.time,mapa$t.time)
cor.test(mapb$f.time,mapb$t.time)

## Correlation between errors
ggplot(data, aes(data$dist, data$loc.err, colour=data$map)) +
  geom_point() +
  geom_smooth(method = "lm")

# cor.test(data$f.time,data$t.time)
cor.test(mapa$loc.err,mapa$dist)
cor.test(mapb$loc.err,mapb$dist)


## Correlations between time and error
ggplot(data, aes(x=f.time, y=loc.err, colour=map)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm")

ggplot(data, aes(x=t.time, y=dist, colour=map)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm")

cor.test(mapa$f.time, mapa$loc.err)
cor.test(mapb$f.time, mapb$loc.err)

cor.test(mapa$t.time, mapa$dist)
cor.test(mapb$t.time, mapb$dist)

# Relations between age and {loc.err, dist, t.time, f.time}
ggplot(data, aes(x=age, y=loc.err)) +
  geom_point(aes(group=map,colour=map,fill=map), alpha=0.5)

cor(data$age, data$loc.err)
cor(data$age, data$dist)

cor(data$age, data$t.time)
cor(data$age, data$f.time)

# Relation between country and {loc.err, dist, t.time, f.time}
ggplot(data, aes(nationality, loc.err)) +
  geom_boxplot() + coord_flip()

ggplot(data, aes(nationality, dist)) +
  geom_boxplot() + coord_flip()

ggplot(data, aes(nationality, t.time)) +
  geom_boxplot() + coord_flip()

ggplot(data, aes(nationality, f.time)) +
  geom_boxplot() + coord_flip()

# Relation between gender and speed
ggplot(data, aes(gender, loc.err, colour=map)) +
  geom_boxplot()

ggplot(data, aes(gender, dist, colour=map)) +
  geom_boxplot()

ggplot(data, aes(gender, t.time, colour=map)) +
  geom_boxplot()

ggplot(data, aes(gender, f.time, colour=map)) +
  geom_boxplot()


# aggregate(data$f.time, list(data$fid), mean)
ttt <- aggregate(data$f.time, list(data$fid, data$map), mean)

# ggplot(ttt, aes(Group.1, x)) +
#   geom_line(aes(group=ttt$Group.2, colour=ttt$Group.2)) + 
#   geom_smooth(method = "lm", group=ttt$Group.2, colour=ttt$Group.2)

ggplot(ttt, aes(Group.1, x, colour = Group.2)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")


# ggplot(data, aes(x=fid, y=mean() +
#   geom_point(aes(group=map,colour=map), alpha=0.2)



# Strength of learning-effect
ggplot(data, aes(map, loc.err)) +
  geom_boxplot(aes(colour=learned), alpha=0.5)

ggplot(data, aes(map, dist)) +
  geom_boxplot(aes(colour=learned), alpha=0.5)

ggplot(data, aes(map, t.time)) +
  geom_boxplot(aes(colour=learned), alpha=0.5)

ggplot(data, aes(map, f.time)) +
  geom_boxplot(aes(colour=learned), alpha=0.5)

