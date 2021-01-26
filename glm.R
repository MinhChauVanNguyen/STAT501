library(ggplot2)
library(gplots)
library(ggpubr)
library(tidyr)
library(dplyr)

fosh <- read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE)
fish <- fish[!(names(fish) %in% c("fisherman", "TotHg"))]

fish <- fish %>% mutate(fishmlwk = case_when(fishmlwk < 7  ~ '<7 grp',
                                             fishmlwk ==7 ~ '7 grp',
                                             fishmlwk > 7 ~ '>7 grp'))
fish$fishmlwk <- factor(fish$fishmlwk)
mean(fish$MeHg[fish$fishmlwk == "<7 grp"])
mean(fish$MeHg[fish$fishmlwk == "7 grp"])
mean(fish$MeHg[fish$fishmlwk == ">7 grp"])

median(fish$MeHg[fish$fishmlwk == "<7 grp"])
median(fish$MeHg[fish$fishmlwk == "7 grp"])
median(fish$MeHg[fish$fishmlwk == ">7 grp"])


fish$fishpart <- factor(fish$fishpart)

fit <- glm(TotHg ~ fishmlwk + fishpart + weight + height + age + restime,
           data = fish, family = Gamma(link = "log"))
summary(fit)

fit2 <- glm(TotHg ~ fishmlwk + fishpart + weight,
           data = fish, family = Gamma())
summary(fit2)

plot(fit2, which=1, col=c("blue"))

library(DHARMa)

simout  <- simulateResiduals(fit2,  n=1000)
tiff("Residualplots", units="in", width=10, height=7, res=300)
par(mfrow=c(1,2))
newplotQQunif(simout) 
plotResiduals(simout)
dev.off()
par(mfrow=c(1,1))

testResiduals(simout)
testUniformity(simout)

grid.ar
