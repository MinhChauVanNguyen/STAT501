library(ggplot2)
library(gplots)
library(ggpubr)
library(tidyr)
library(dplyr)
library(grid)
library(caret)
library(gridExtra)

fishy <- read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE)

fish <- fish[!(names(fish) == "fisherman")]


## MeHg 

# histogram

tiff("plot1", units="in", width=5, height=4, res=300)

dat <- data.frame(cond = factor(rep(c("MeHg"), each=100)), 
                  MeHg = fish$MeHg)

dat2 <- data.frame(cond = factor(rep(c("TotHg"), each=100)), 
                   TotHg = fish$TotHg)

ggplot() +
  geom_histogram(data = dat, aes(x=MeHg, fill = cond, y=..density..), color = 'hotpink', bins = 15, alpha=.5, position="identity") +
  geom_density(data = dat2, aes(x=TotHg, fill = cond), alpha=.4) +
  scale_fill_manual(values = c("lavender","lightskyblue")) +
  theme_bw() + labs(fill = "Mercury conc.", x = expression(bold(paste("Mercury levels (", mu, "g/g)")))) + 
  theme(axis.title.y = element_text(face = "bold"),
        legend.position=c(.7,.75),
        legend.background = element_rect(color = "black", fill = "#E0FFFF"),
        axis.title.x = element_text(face = "bold")) 

# ggplot(fish, aes(x = MeHg)) + 
#   geom_histogram(bins = 15, fill = "lavender", color = 'hotpink') +
#   #geom_density(aes(y = ..count..), fill = 'lightskyblue', alpha = 0.4) + 
#   theme_bw() + labs(x = expression(bold(paste("MeHg (", mu, "g/g)"))),
#                     y = "No.of.obs (person)") +
#   annotate(geom = "text", x = 15, y = 20, label = paste ("Mean ==", mean(fish$MeHg)), parse = TRUE, color = "red", size = 5) +
#   annotate(geom = "text", x = 14.9, y = 18, label = paste ("Median ==", median(fish$MeHg)), parse = TRUE, color = "red", size = 5) +
#   theme(axis.title.y = element_text(face = "bold"),
#         legend.position=c(.9,.75),
#         axis.title.x = element_text(face = "bold")) 

dev.off()

# scatter plot
d2 <- fish
d2$fisherman <- ""
d2$fisherman[d2$MeHg > 15] <- c("outlier", "outlier")


p1 <- ggplot(d, aes(x = 1:nrow(d), y = TotHg, label = fisherman)) +
  geom_point(col = "blue", fill = "#ffb6da", alpha = 0.6, shape = 21, size = 4, stroke = 1) +
  labs(x = "Index", y = expression(bold(paste("TotHg (", mu, "g/g)")))) + theme_bw() +
  annotate(geom = "text", x = 50, y = 14, label = paste ("Mean ==", mean(fish$TotHg)), parse = TRUE, color = "red", size = 5) +
  annotate(geom = "text", x = 50, y = 12.7, label = paste ("Median ==", median(fish$TotHg)), parse = TRUE, color = "red", size = 5) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(size = 8)) +
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5)

p2 <- ggplot(d2, aes(x = 1:nrow(d2), y = MeHg, label = fisherman)) +
  geom_point(col = "blue", fill = "#b6daff", alpha = 0.6, shape = 21, size = 4, stroke = 1) +
  labs(x = "Index", y = expression(bold(paste("MeHg (", mu, "g/g)")))) + theme_bw() +
  annotate(geom = "text", x = 50, y = 14, label = paste ("Mean ==", mean(fish$MeHg)), parse = TRUE, color = "red", size = 5) +
  annotate(geom = "text", x = 50, y = 12.7, label = paste ("Median ==", median(fish$MeHg)), parse = TRUE, color = "red", size = 5) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(size = 8)) +
  geom_label_repel(box.padding = 0.35, point.padding = 0.5)

tiff("Scatter", units="in", width=6, height=4, res=300)

ggarrange(p2, p1)

dev.off()


# normality test
shapiro.test(log(fish$MeHg))



## fishpart
# give.median <- function(x){
#   return(c(y = median(x)*0.8, label = round(median(x), 2))) 
#   # experiment with the multiplier to find the perfect position
# }

fish$fishpart <- factor(fish$fishpart)

means <- aggregate(fish[, 'MeHg'], list('fishpart' = fish$fishpart), mean)
medians <- aggregate(fish[, 'MeHg'], list('fishpart' = fish$fishpart), median)

tiff("plot3", units="in", width=5, height=4, res=300)

ggplot(fish, aes(x=fishpart, y=MeHg, fill=fishpart)) +  # This is the plot function
  geom_boxplot(alpha = 0.5) + theme_bw() + 
  labs(y = expression(bold(paste("MeHg (", mu, "g/g)"))), fill = "fishpart") +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) +
  geom_text(data = means, aes(label = paste("Mean =",round(x, 2)), y = x + 4), size = 3, col = "red") + 
  geom_text(data = medians, aes(label = paste("Median =",round(x, 2)), y = x + 3), size = 3, col = "red") + 
  #stat_summary(fun.data = give.median, geom = "text", fun = median) +
  scale_fill_manual(values = c("#87CEFA", "#dbb6ff", "#ffb6da"), 
                    labels = c("Muscle", "Both", "Whole fish")) +
  xlim("Group 1", "Group 2", "Group 3")

dev.off()

## fishmlwk

# Pie chart

tiff("plot4", units="in", width=5, height=7, res=300)

df <- data.frame(
  group = c("<7 grp", "7 grp", ">7 grp"),
  value = c(14, 70, 16))

labs <- paste(df$value, "obs")

ggpie(df, "value", label = labs, fill = "group",
      lab.pos = "in", lab.font = list(style = "bold", size = 9)) +
  scale_fill_manual(values = alpha(c("deeppink","darkviolet", "deepskyblue"), .3)) +
  labs(fill = "fishmlwk") +
  theme(legend.position = "right",
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold"),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(-30,-30,-30,-30),
        legend.background = element_rect(fill=NA)) +
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  

dev.off()


fish <- fish %>% mutate(fishmlwk = case_when(fishmlwk < 7  ~ '<7 grp',
                                             fishmlwk ==7 ~ '7 grp',
                                             fishmlwk > 7 ~ '>7 grp'
))

fish$fishmlwk <- factor(fish$fishmlwk, levels = c('<7 grp', '7 grp', '>7 grp'))


meanss <- aggregate(fish[, 'MeHg'], list('fishmlwk' = fish$fishmlwk), mean)
medianss <- aggregate(fish[, 'MeHg'], list('fishmlwk' = fish$fishmlwk), median)

tiff("plot5", units="in", width=5, height=3, res=300)

p <- ggplot(fish, aes(x = MeHg, fill = fishmlwk)) + 
  geom_histogram(bins = 10, alpha = 0.5) +
  labs(x = expression(bold(paste("MeHg (", mu, "g/g)")))) +
  facet_wrap( ~ fishmlwk) + 
  theme_bw() +
  geom_text(data = meanss, aes(x = x, y = Inf, label = paste("Mean =",round(x, 3))),
             vjust = 4, hjust = -0.2, col = "red", size = 3) +
  geom_text(data = medianss, aes(x = x, y = Inf, label = paste("Median =",round(x, 3))),
            vjust = 6, hjust = -0.2, col = "red", size = 3) +
  scale_fill_manual(values=c("#00CED1", "#FF00FF", "#56B4E9")) +
  theme(legend.position="none",
        strip.text = element_text(face = "bold", size = 10))

g <- ggplot_gtable(ggplot_build(p))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("#00CED1", "#FF00FF", "#56B4E9")
k <- 1

for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

grid.draw(g)

dev.off()


## Height and Weight

weight <- ggplot(fish, aes(x = weight)) + 
  geom_density(aes(y = ..count..), fill = 'lightskyblue', alpha = 0.4) + 
  theme_bw() + labs(x = "Weight (kgs)",
                    y = "No.of.obs (person)") +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) +
  geom_vline(aes(xintercept = mean(weight), col = "mean"),size=1) +
  geom_vline(aes(xintercept = median(weight), col = "median"),size=1) +
  scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red")) +
  theme(legend.position=c(.8, .75),
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"))

gw <- ggplotGrob(weight)
gw <- editGrob(grid.force(gw), gPath("key-[3,4]-1-[1,2]"), 
               grep = TRUE, global = TRUE,
               x0 = unit(0, "npc"), y0 = unit(0.5, "npc"), 
               x1 = unit(1, "npc"), y1 = unit(0.5, "npc")) 


height <- ggplot(fish, aes(x = height)) + 
  geom_density(aes(y = ..count..), fill = 'orchid', alpha = 0.4) + 
  theme_bw() + labs(x = "Height (cm)",
                    y = "No.of.obs (person)") +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) +
  geom_vline(aes(xintercept = mean(height), col = "mean"),size=1) +
  geom_vline(aes(xintercept = median(height), col = "median"),size=1) +
  scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red")) +
  theme(legend.position=c(.8, .75),
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"))

gt <- ggplotGrob(height)
gt <- editGrob(grid.force(gt), gPath("key-[3,4]-1-[1,2]"), 
               grep = TRUE, global = TRUE,
               x0 = unit(0, "npc"), y0 = unit(0.5, "npc"), 
               x1 = unit(1, "npc"), y1 = unit(0.5, "npc")) 

tiff("plot6", units="in", width=6, height=4, res=300)
grid.arrange(gw, gt, ncol=2)
dev.off()


## Age

tiff("plot7", units="in", width=5, height=4, res=300)

p <- ggscatter(fish, x = "weight", y = "MeHg", color = "age", size = 5,
               add = "reg.line",
          xlab = "Weight (kg)", ylab = expression(bold(paste("MeHg (", mu, "g/g)")))) +
  theme_bw() + gradient_color(c("#00BFFF", "#8A2BE2", "violet", "lavender")) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        legend.title = element_text(size = 10, face="bold")) 

ggpar(p, legend.title = "Age (years)") 

dev.off()


## Residence time in Kuwait

tiff("plot8", units="in", width=5, height=4, res=300)

ggplot(fish, aes(x=restime, y=MeHg)) +
  geom_point(aes(size=MeHg), color = "dodgerblue", alpha = 0.7) +
  theme_bw() +
  labs(x = "Residence time in Kuwait (years)", y = expression(bold(paste("MeHg (", mu, "g/g)")))) +
  geom_smooth(aes(y=MeHg, x=restime), method = lm, se = FALSE, color = "pink") +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        legend.title = element_text(size = 10, face="bold"),
        legend.key = element_rect(linetype = "solid", color = "dodgerblue"),
        legend.background = element_rect(fill="aliceblue",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue"),
        legend.position = c(0.9, 0.8)) +
  guides(size = guide_legend(title = expression(bold(paste("MeHg (", mu, "g/g)")))))
  
dev.off()


## Age vs Residence time

p1 <- ggplot(fish, aes(x = age)) + 
  geom_histogram(bins = 15, fill = "#8A2BE2", alpha = 0.3, color = 'blue') +
  theme_bw() + labs(x = "Age (years)", y = "No.of.obs (persons)") +
  annotate(geom = "text", x = 50, y = 18, label = paste ("Mean =", round(mean(fish$age)), "years"), color = "red", size = 5) +
  annotate(geom = "text", x = 49.5, y = 16.5, label = paste ("Median =", median(fish$age), "years"), color = "red", size = 5) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) 

p2 <- ggplot(fish, aes(x = restime)) + 
  geom_histogram(bins = 15, fill = "#48D1CC", alpha = 0.4, color = 'blue') +
  theme_bw() + labs(x = "Residene time in Kuwait (years)", y = "No.of.obs (persons)") +
  annotate(geom = "text", x = 15, y = 20, label = paste("Mean =", round(mean(fish$restime)), "years"), color = "red", size = 5) +
  annotate(geom = "text", x = 14.7, y = 18, label = paste("Median =", median(fish$restime), "years"), color = "red", size = 5) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) 

tiff("plot9", units="in", width=10, height=3, res=300)
ggarrange(p1, p2, ncol = 2)
dev.off()


## Collinearity between features
dmy <- dummyVars(" ~ .", data = fish)
dummy <- data.frame(predict(dmy, newdata=fish))

dummy <- dummy[!(names(dummy) == "MeHg")]

names(dummy) <- c("<7 fishmlwk", "7 fishmlwk", ">7 fishmlwk",
                  "restime", "height", "weight", "age",
                  "fishpart1", "fishpart2", "fishpart3")

res <- cor(dummy)

tiff("plot10", units="in", width=5, height=4, res=300)

heatmap.2(res, scale = "none", col = c("aliceblue", "lightskyblue", "deepskyblue", "hotpink"), 
          trace = "none",
          #cellnote = res,
          colsep = 1:ncol(res),
          rowsep = 1:nrow(res),
          density.info = "density", dendrogram = "none",
          margins = c(6,3),
          keysize = 1, key.par=list(mar=c(3.5,0,3,0)),
          cexCol = 1,
          cexRow = 1,
          key.xlab = NA,
          key.ylab = NA,
          lmat = rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1), 
          key.title = "Color Key & Density Plot")

dev.off()
 
###########################################################

fishy <- fishy %>% mutate(fishmlwk = case_when(fishmlwk < 7  ~ '1',
                                             fishmlwk ==7 ~ '2',
                                             fishmlwk > 7 ~ '3'
))

g1 <- ggplot(fish, aes(fishmlwk, MeHg, shape = fishmlwk)) + 
  geom_point(aes(shape = factor(fishmlwk)), size = 5, color = "blue") +
  geom_point(aes(shape = factor(fishmlwk)), colour = "#ffb6da", size = 3.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(y = expression(bold(paste("MeHg (", mu, "g/g)"))), x = "Fish meals per week") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold")) +
  scale_x_discrete(limits=c("<7 grp","7 grp",">7 grp"))

g2 <- ggplot(fishy, aes(x = as.numeric(fishmlwk), y = TotHg)) + 
  geom_point(aes(shape = factor(fishmlwk)), size = 5, color = "blue") +
  geom_point(aes(shape = factor(fishmlwk)), colour = "#ff1e8c", size = 3.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(y = expression(bold(paste("TotHg (", mu, "g/g)"))), x = "Fish meals per week") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold")) +
  scale_x_discrete(limits=c("<7 grp","7 grp",">7 grp"))


tiff("fishmlwk", units="in", width=6, height=3, res=300)
ggarrange(g1, g2)
dev.off()
