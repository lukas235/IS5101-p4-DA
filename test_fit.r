#Fake data
z <- rnorm(n=2000,m=10,sd=2)
#Your code
# factorx <- factor(cut(x, breaks=seq(1.0, 3, by=0.1))) # seq(1.0, 3, by=0.1)
#Tabulate and turn into data.frame
# xout <- as.data.frame(table(factorx))
#Add cumFreq and proportions
# xout <- transform(xout, cumFreq = cumsum(Freq), relative = prop.table(Freq))
# xd <- table(factorx)

x   <- seq(5,15,length=1000)
y   <- dnorm(x,mean=10, sd=3)
plot(x,y, type="l", lwd=1)


hist(z, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(z), col="blue", lwd=2) # add a density estimate with defaults
lines(density(z, adjust=2), lty="dotted", col="darkgreen", lwd=2)
lines(dnorm(x, mean=mean(y), sd=sd(y)))


# lines(dnorm(x, mean=mean(y), sd=sd(y)))

# plot(xd, prob=TRUE, col="grey")

#-----
# factorx Freq cumFreq   relative
# 1 (9.99,11.4]   11      11 0.25000000
# 2 (11.4,12.9]    3      14 0.06818182
# 3 (12.9,14.3]   11      25 0.25000000
# 4 (14.3,15.7]    2      27 0.04545455
# 5 (15.7,17.1]    6      33 0.13636364
# 6 (17.1,18.6]    3      36 0.06818182
# 7   (18.6,20]    8      44 0.18181818
