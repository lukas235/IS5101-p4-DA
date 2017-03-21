#!/usr/bin/env Rscript

data <- read.csv("titanic.csv")

# Let's output all the data!
data

# ... except that's rather a lot
head(data)

# ...actually, just those under 60 please
# Store the result in a new variable 'young'
young <- subset(data, Age < 60)
# and anyone over 65 too, except third class
rich.retired <- subset(data, Age > 60 & Pclass != 2)


# Combine some conditions together: over-60s in third class
subset(data, Age >= 60 & Pclass == 3)

# Let's gather some basic information
min(data$Pclass, na.rm=TRUE)
max(data$Pclass, na.rm=TRUE)
mean(data$Fare, na.rm=TRUE)
median(data$Fare, na.rm=TRUE)
var(data$Age, na.rm=TRUE)
sd(data$Age, na.rm=TRUE)
nrow(data)
length(data$PassengerId)

# And some simple graphs
hist(data$Age, main="Passenger Ages", xlab="Age (years)", ylab="Count", breaks=16)
plot(data$Pclass, data$Fare, main="Fare vs. class")

# Run some statistical tests
t.test( subset(data, Age >= 30)$Fare, subset(data, Age < 30)$Fare )
var.test( young$Fare, rich.retired$Fare )

# Calculate some correlations
cor( data$Fare, data$Pclass )
cor( data$SibSp, data$Parch )

# Calculate a histogram of fares
hist(data$Fare, main="Passenger Fares", xlab="Fare Cost", ylab="Count", breaks=8)

# What percentage was upper-class?
nrow( subset(data, Pclass == 1) ) / nrow(data)

# How many ports were visited?
length( unique( data$Embarked ) )

# Ratio of men vs. women?
nrow( subset(data, Sex=="male") )
nrow( subset(data, Sex=="female" ) )
nrow( subset(data, Sex=="male") ) / nrow(data)
nrow( subset(data, Sex=="female" ) ) / nrow(data)
# Ratio of men vs. women who survived?
nrow( subset(data, Sex=="male" & Survived == 1) )
nrow( subset(data, Sex=="female" & Survived == 1 ) )
nrow( subset(data, Sex=="male" & Survived == 1) ) / nrow( subset(data, Survived == 1) )
nrow( subset(data, Sex=="female" & Survived == 1) ) / nrow( subset(data, Survived == 1) )

# Investigate the effect of age on survival
t.test( subset(data, Survived == 1)$Age, subset(data, Survived == 0)$Age )
