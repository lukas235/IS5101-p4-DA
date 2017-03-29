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
mean(subset(data, fid == 1 & map == 'MapA')$age)
median(subset(data, fid == 1 & map == 'MapA')$age)
sd(subset(data, fid == 1 & map == 'MapA')$age)
range(subset(data, fid == 1 & map == 'MapA')$age)
ggplot(subset(data, fid == 1 & map == 'MapA'), aes(age)) +
  geom_histogram(alpha=0.5,position='identity',binwidth=1, colour="black") +
  xlab("Participants' Age") + ylab("Count")


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
mapa.ftime.nd <- ggplot(mapa, aes(x=f.time)) +
  geom_histogram(aes(y=..density..), alpha=0.5,position='identity',binwidth=0.5, colour="black") +
  geom_density(aes(y=..density..), alpha=0.2, fill="red", colour="red", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(mapa$f.time, na.rm=TRUE), sd = sd(mapa$f.time, na.rm=TRUE)), colour="blue", size=1, linetype="dashed") + 
  labs(title="Distribution of f.time (MapA)") + theme(plot.title=element_text(face="bold", color="black"))

mapb.ftime.nd <- ggplot(mapb, aes(x=f.time)) +
  geom_histogram(aes(y=..density..), alpha=0.5,position='identity',binwidth=0.5, colour="black") +
  geom_density(aes(y=..density..), alpha=0.2, fill="red", colour="red", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(mapb$f.time, na.rm=TRUE), sd = sd(mapb$f.time, na.rm=TRUE)), colour="blue", size=1, linetype="dashed") +
  labs(title="Distribution of f.time (MapB)") + theme(plot.title=element_text(face="bold", color="black"))

grid.arrange(mapa.ftime.nd, mapb.ftime.nd, ncol=2)  # arrange

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
mapa.ttime.nd <- ggplot(mapa, aes(x=t.time)) +
  geom_histogram(aes(y=..density..), alpha=0.5,position='identity',binwidth=0.1, colour="black") +
  geom_density(aes(y=..density..), alpha=0.2, fill="red", colour="red", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(mapa$t.time, na.rm=TRUE), sd = sd(mapa$t.time, na.rm=TRUE)), colour="blue", size=1, linetype="dashed") + 
  labs(title="Distribution of t.time (MapA)") + theme(plot.title=element_text(face="bold", color="black"))

mapb.ttime.nd <- ggplot(mapb, aes(x=t.time)) +
  geom_histogram(aes(y=..density..), alpha=0.5,position='identity',binwidth=0.1, colour="black") +
  geom_density(aes(y=..density..), alpha=0.2, fill="red", colour="red", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(mapb$t.time, na.rm=TRUE), sd = sd(mapb$t.time, na.rm=TRUE)), colour="blue", size=1, linetype="dashed") +
  labs(title="Distribution of t.time (MapB)") + theme(plot.title=element_text(face="bold", color="black"))

grid.arrange(mapa.ttime.nd, mapb.ttime.nd, ncol=2)  # arrange

# Tests
t.test(mapa$t.time , mapb$t.time)
var.test(mapa$t.time , mapb$t.time)

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
  geom_histogram(aes(y=..density.., group=map,colour=map,fill=map), alpha=0.1,position='identity') +
  geom_density(aes(group=map,colour=map, fill=map), alpha=0.2)
  # geom_histogram(aes(y=..density..), alpha = 0.2)

t.test(mapa$loc.err, mapb$loc.err)
var.test(mapa$loc.err, mapb$loc.err)
wilcox.test(mapa$loc.err, mapb$loc.err)
kruskal.test(loc.err ~ map, data = data)

median(mapa$loc.err)
median(mapb$loc.err)


# Analysis of the typing error
ggplot(data, aes(x=dist)) +
  geom_histogram(aes(y=..density.., group=map,colour=map,fill=map), binwidth = 0.05, alpha=0.2,position='identity') +
  geom_density(aes(group=map,colour=map, fill=map), alpha=0.2)

t.test(mapa$dist, mapb$dist)
var.test(mapa$dist, mapb$dist)
wilcox.test(mapa$dist, mapb$dist)
kruskal.test(dist ~ map, data = data)

median(mapa$dist)
median(mapb$dist)



## Correlation between times
times.avg <- aggregate(data$t.time, list(data$pid), mean)
times.tmp <- aggregate(data$f.time, list(data$pid), mean)
times.avg['y'] <- times.tmp$x
times.avg <- cbind(times.avg, times.tmp$x)

ggc1 <- ggplot(times.avg, aes(times.avg$x, times.avg$y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("f.time") + xlab("t.time") + labs(title = "Correlation between\nf.time and t.time")

cor.test(times.avg$x, times.avg$y)

# ggplot(data, aes(data$f.time, data$t.time, colour=data$map)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# # cor.test(data$f.time,data$t.time)
# cor.test(mapa$f.time,mapa$t.time)
# cor.test(mapb$f.time,mapb$t.time)

## Correlation between errors
errors.avg <- aggregate(data$loc.err, list(data$pid), mean)
errors.tmp <- aggregate(data$dist, list(data$pid), mean)
errors.avg['y'] <- errors.tmp$x
errors.avg <- cbind(errors.avg, errors.tmp$x)

ggc2 <- ggplot(errors.avg, aes(errors.avg$x, errors.avg$y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Typing Error") + xlab("Localisation Error") + labs(title = "Correlation between typing\nerror and localisation error")

cor.test(errors.avg$x, errors.avg$y)

# ggplot(data, aes(data$dist, data$loc.err, colour=data$map)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# # cor.test(data$f.time,data$t.time)
# cor.test(mapa$loc.err,mapa$dist)
# cor.test(mapb$loc.err,mapb$dist)


## Correlations between avg time and error per participant
floc.avg <- aggregate(data$f.time, list(data$pid), mean)
floc.tmp <- aggregate(data$loc.err, list(data$pid), mean)
floc.avg['y'] <- floc.tmp$x
floc.avg <- cbind(floc.avg, floc.tmp$x)

ggc3 <- ggplot(floc.avg, aes(x=floc.avg$x, y=floc.avg$y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Localisation Error") + xlab("f.time") + labs(title = "Correlation between\nf.time and Localisation Error")

cor.test(floc.avg$x, floc.avg$y)


tdist.avg <- aggregate(data$t.time, list(data$pid), mean)
tdist.tmp <- aggregate(data$dist, list(data$pid), mean)
tdist.avg['y'] <- tdist.tmp$x
tdist.avg <- cbind(tdist.avg, tdist.tmp$x)

ggc4 <- ggplot(tdist.avg, aes(tdist.avg$x, tdist.avg$y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Typing Error") + xlab("t.time") + labs(title = "Correlation between\ntyping error and t.time")

cor.test(tdist.avg$x, tdist.avg$y)

grid.arrange(ggc1, ggc2 ,ggc3, ggc4, ncol=2)  # arrange


# ggplot(data, aes(x=f.time, y=loc.err, colour=map)) +
#   geom_point(alpha=0.5) +
#   geom_smooth(method = "lm")
# 
# ggplot(data, aes(x=t.time, y=dist, colour=map)) +
#   geom_point(alpha=0.5) +
#   geom_smooth(method = "lm")
# 
# cor.test(mapa$f.time, mapa$loc.err)
# cor.test(mapb$f.time, mapb$loc.err)
# 
# cor.test(mapa$t.time, mapa$dist)
# cor.test(mapb$t.time, mapb$dist)

# Relations between age and {loc.err, dist, t.time, f.time}
ftime.age <- aggregate(data$f.time, list(data$age, data$map), mean)
age1 <- ggplot(ftime.age, aes(x=Group.1, y=x, colour=Group.2)) +
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm") +
  xlab("Age") + ylab("f.time")

loc.err.age <- aggregate(data$loc.err, list(data$age, data$map), mean)
age2 <- ggplot(loc.err.age, aes(x=Group.1, y=x, colour=Group.2)) +
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm") +
  xlab("Age") + ylab("Localisation Error")

ttime.age <- aggregate(data$t.time, list(data$age, data$map), mean)
age3 <- ggplot(ttime.age, aes(x=Group.1, y=x, colour=Group.2)) +
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm") +
  xlab("Age") + ylab("t.time")

dist.age <- aggregate(data$dist, list(data$age, data$map), mean)
age4 <- ggplot(dist.age, aes(x=Group.1, y=x, colour=Group.2)) +
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm") +
  xlab("Age") + ylab("Typing Error")

grid.arrange(age1, age2, age3, age4, ncol=2)  # arrange

cor.test(ftime.age$Group.1, ftime.age$x)
cor.test(loc.err.age$Group.1, loc.err.age$x)
cor.test(ttime.age$Group.1, ttime.age$x)
cor.test(dist.age$Group.1, dist.age$x)


# Relation between country and {loc.err, dist, t.time, f.time}
gg1 <- ggplot(data, aes(nationality, loc.err)) +
  geom_boxplot() + coord_flip() +
  labs(title="Localisation error per country")

gg2 <- ggplot(data, aes(nationality, dist)) +
  geom_boxplot() + coord_flip() +
  labs(title="Typing error per country")

gg3 <- ggplot(data, aes(nationality, t.time)) +
  geom_boxplot() + coord_flip() +
  labs(title="Typing time (t.time) per country")

gg4 <- ggplot(data, aes(nationality, f.time)) +
  geom_boxplot() + coord_flip() +
  labs(title="Localisation time (f.time) per country")

grid.arrange(gg4, gg3, gg1, gg2, ncol=2)  # arrange




# Relation between gender and speed
gg5 <- ggplot(data, aes(gender, loc.err, colour=map)) +
  geom_boxplot() +
  labs(title="Localisation error per gender")

gg6 <- ggplot(data, aes(gender, dist, colour=map)) +
  geom_boxplot() +
  labs(title="Typing error per gender")

gg7 <- ggplot(data, aes(gender, t.time, colour=map)) +
  geom_boxplot() +
  labs(title="Typing time (t.time) per gender")

gg8 <- ggplot(data, aes(gender, f.time, colour=map)) +
  geom_boxplot() +
  labs(title="Localisation time (f.time) per gender")

grid.arrange(gg8, gg7, gg5, gg6, ncol=2)  # arrange


# Learning: f.time
ftime.learn <- aggregate(data$f.time, list(data$fid, data$map), mean)
ggplot(ftime.learn, aes(Group.1, x, colour = Group.2)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

cor.test(subset(ftime.learn, Group.2 == "MapA")$Group.1, subset(ftime.learn, Group.2 == "MapA")$x)
cor.test(subset(ftime.learn, Group.2 == "MapB")$Group.1, subset(ftime.learn, Group.2 == "MapB")$x)

# Learning: loc.err
loc.err.learn <- aggregate(data$loc.err, list(data$fid, data$map), mean)
ggplot(loc.err.learn, aes(Group.1, x, colour = Group.2)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

cor.test(subset(loc.err.learn, Group.2 == "MapA")$Group.1, subset(loc.err.learn, Group.2 == "MapA")$x)
cor.test(subset(loc.err.learn, Group.2 == "MapB")$Group.1, subset(loc.err.learn, Group.2 == "MapB")$x)

# Learning: t.time
ttime.learn <- aggregate(data$t.time, list(data$fid, data$map), mean)
ggplot(ttime.learn, aes(Group.1, x, colour = Group.2)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

cor.test(subset(ttime.learn, Group.2 == "MapA")$Group.1, subset(ttime.learn, Group.2 == "MapA")$x)
cor.test(subset(ttime.learn, Group.2 == "MapB")$Group.1, subset(ttime.learn, Group.2 == "MapB")$x)

# Learning: dist
dist.learn <- aggregate(data$dist, list(data$fid, data$map), mean)
ggplot(dist.learn, aes(Group.1, x, colour = Group.2)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

cor.test(subset(dist.learn, Group.2 == "MapA")$Group.1, subset(dist.learn, Group.2 == "MapA")$x)
cor.test(subset(dist.learn, Group.2 == "MapB")$Group.1, subset(dist.learn, Group.2 == "MapB")$x)



# Strength of learning-effect
le1 <- ggplot(data, aes(map, f.time)) +
  geom_boxplot(aes(colour=learned), alpha=0.5)

t.test(subset(data, learned==FALSE & map=="MapA")$f.time, subset(data, learned==TRUE & map=="MapA")$f.time)
t.test(subset(data, learned==FALSE & map=="MapB")$f.time, subset(data, learned==TRUE & map=="MapB")$f.time)

le2 <- ggplot(data, aes(map, loc.err)) +
  geom_boxplot(aes(colour=learned), alpha=0.5)

mean(subset(data, learned==FALSE & map=="MapA")$loc.err)
mean(subset(data, learned==TRUE & map=="MapA")$loc.err)
mean(subset(data, learned==FALSE & map=="MapB")$loc.err)
mean(subset(data, learned==TRUE & map=="MapB")$loc.err)
wilcox.test(subset(data, learned==FALSE & map=="MapA")$loc.err, subset(data, learned==TRUE & map=="MapA")$loc.err)
wilcox.test(subset(data, learned==FALSE & map=="MapB")$loc.err, subset(data, learned==TRUE & map=="MapB")$loc.err)

le3 <- ggplot(data, aes(map, t.time)) +
  geom_boxplot(aes(colour=learned), alpha=0.5)

t.test(subset(data, learned==FALSE & map=="MapA")$t.time, subset(data, learned==TRUE & map=="MapA")$t.time)
t.test(subset(data, learned==FALSE & map=="MapB")$t.time, subset(data, learned==TRUE & map=="MapB")$t.time)

le4 <- ggplot(data, aes(map, dist)) +
  geom_boxplot(aes(colour=learned), alpha=0.5)

mean(subset(data, learned==FALSE & map=="MapA")$dist)
mean(subset(data, learned==TRUE & map=="MapA")$dist)
mean(subset(data, learned==FALSE & map=="MapB")$dist)
mean(subset(data, learned==TRUE & map=="MapB")$dist)
wilcox.test(subset(data, learned==FALSE & map=="MapA")$dist, subset(data, learned==TRUE & map=="MapA")$dist)
wilcox.test(subset(data, learned==FALSE & map=="MapB")$dist, subset(data, learned==TRUE & map=="MapB")$dist)

grid.arrange(le1, le2, le3, le4, ncol=2)
