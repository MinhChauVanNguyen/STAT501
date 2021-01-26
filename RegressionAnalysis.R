#### Before removing outliers

fish <- read.csv("fish.csv",header=TRUE,stringsAsFactors=FALSE)

colnames(fish)

## Scatter plot
scatter.smooth(x=fish$TotHg, y=fish$fishmlwk, 
               main="Total Hg ~ Fish meals per week")

scatter.smooth(x=fish$fishmlwk, y=fish$TotHg, 
               main="Total Hg ~ Fish meals per week")  

plot(fish$TotHg, fish$fishmlwk, main="Scatterplot bitch", 
     xlab="TotHg ", ylab="Fish meal per week ", pch=19)  
  

## Normality

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(fish$TotHg), main="Total Hg", 
     ylab="Frequency", sub=paste("Skewness:", 
    round(e1071::skewness(fish$TotHg), 2)))  # density plot for 'speed'
polygon(density(fish$TotHg), col="red")

plot(density(fish$fishmlwk), main="Fish meals per week", 
     ylab="Frequency", sub=paste("Skewness:", 
    round(e1071::skewness(fish$fishmlwk), 2)))  # density plot for 'dist'
polygon(density(fish$fishmlwk), col="blue")

par(mfrow=c(1,1))






### After removing outliers

fish2 <- read.csv("fish2.csv",header=TRUE,stringsAsFactors=FALSE)

colnames(fish2)

## Scatter plot
scatter.smooth(x=fish2$TotHg, y=fish2$fishmlwk, 
               main="Total Hg ~ Fish meals per week")

scatter.smooth(x=fish2$fishmlwk, y=fish2$TotHg, 
               main="Total Hg ~ Fish meals per week")  

plot(fish2$TotHg, fish2$fishmlwk, main="Scatterplot", 
     xlab="TotHg ", ylab="Fish meal per week ", pch=19)  

## Normality

library(e1071)

par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(fish2$TotHg), main="Total Hg", 
     ylab="Frequency", sub=paste("Skewness:", 
    round(e1071::skewness(fish2$TotHg), 2)))  # density plot for 'speed'
polygon(density(fish2$TotHg), col="red")

plot(density(fish2$fishmlwk), main="Fish meals per week", 
     ylab="Frequency", sub=paste("Skewness:", 
        round(e1071::skewness(fish2$fishmlwk), 2)))  # density plot for 'dist'
polygon(density(fish2$fishmlwk), col="blue")

par(mfrow=c(1,1))

## Correlation
# calculate correlation between speed and distance 

cor(fish2$fishmlwk, fish2$TotHg)  

## Build Linear Model

linearMod <- lm(fish2$TotHg ~ fish2$fishmlwk, data=fish2) 

summary(linearMod)


### After removing outliers
# Mean MeHg & Total Hg comparison

fish3 <- read.csv("Fish3.csv", header=TRUE, stringsAsFactors=FALSE)

library(ggplot2)

# Basic barplot for Mean MeHg
p <- ggplot(data=fish3, aes(x=fish3$fishmlwk, 
    y=fish3$MeHg.mean)) + geom_bar(stat="identity")
p

# Change the width of bars
ggplot(data=fish3, aes(x=fish3$fishmlwk,y=fish3$MeHg.mean)) +
  geom_bar(stat="identity", width=1)
# Change colors
ggplot(data=fish3, aes(x=fish3$fishmlwk,y=fish3$MeHg.mean)) +
  geom_bar(stat="identity", color="blue", fill="white")
# Outside bars labels
ggplot(data=fish3, aes(x=fish3$fishmlwk,y=fish3$MeHg.mean)) +
    geom_bar(stat="identity",color="blue", fill="white") +
    geom_text(aes(label=fish3$fishmlwk), vjust=-0.3, size=3.5)

# Basic barplot for Mean Total Hg
p <- ggplot(data=fish3, aes(x=fish3$fishmlwk, 
    y=fish3$TotHg.mean)) + geom_bar(stat="identity")
p

# Change the width of bars
ggplot(data=fish3, aes(x=fish3$fishmlwk,y=fish3$TotHg.mean)) +
  geom_bar(stat="identity", width=1)
# Change colors
ggplot(data=fish3, aes(x=fish3$fishmlwk,y=fish3$TotHg.mean)) +
  geom_bar(stat="identity", color="blue", fill="white")
# Outside bars labels
ggplot(data=fish3, aes(x=fish3$fishmlwk,y=fish3$TotHg.mean)) +
  geom_bar(stat="identity",color="blue", fill="white") +
  geom_text(aes(label=fish3$fishmlwk), vjust=-0.3, size=3.5)


# Compare the two means for fishermen group

meHg <- c(6.501,3.414,3.819,6.169)

totHg <- c(6.819,3.546,3.895,6.753)

a <- rbind(meHg,totHg)

a

dimnames(a) <- list(c("MeHg","TotalHg"),
                        c("group1","group2","group3","group4"))

a

barplot(a, main = "Mean of different Fish meals per week",
        xlab = "Fish meal per week", ylab="mercury levels",
        col = c("pink","blue"),
        beside=TRUE)

legend(locator(1),
        c("MeHg","TotalHg"),
        fill = c("pink","blue"), cex = 0.75)

fishy <- read.csv("fish3factors.csv",header=TRUE,stringsAsFactors=FALSE)

fishy$fishmlwk <- as.factor(fishy$fishmlwk)

library("dummies")
fishy.new <- dummy.data.frame(fishy, sep = ".")

is.factor(fishy$fishmlwk)

lm.out1 <- with(fishy, lm(TotHg ~ fishmlwk))

summary(lm.out1)

summary.aov(lm.out1) 

lm.out2 <- with(fishy, lm(log(TotHg) ~ fishmlwk))

summary(lm.out2)

summary.aov(lm.out2)

#lowess
plot(fsh$fishmlwk,fishy$TotHg, pch=16, cex=0.6)
points(lowess(fsh$fishmlwk,fishy$TotHg), pch=16, col="red", cex=0.5)

lines(lowess(fsh$fishmlwk,fishy$TotHg, f=0.667), col="red", lwd=2)
lines(lowess(fsh$fishmlwk,fishy$TotHg, f=0.33), col="blue", lwd=2)
lines(lowess(fsh$fishmlwk,fishy$TotHg, f=0.80), col="purple", lwd=2)
