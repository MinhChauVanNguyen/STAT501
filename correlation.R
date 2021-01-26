kruskal.test(MeHg ~ fishpart, data = fish)
kruskal.test(MeHg ~ fishmlwk, data = fish)

cor.test(fish$MeHg, fish$TotHg, method = 'spearman')
cor.test(x=fish$age, y=fish$MeHg, method = 'spearman')
cor.test(x=fish$restime, y=fish$MeHg, method = 'spearman')

cor.test(x=fish$weight, y=fish$MeHg, method = 'spearman')
cor.test(x=fish$height, y=fish$MeHg, method = 'spearman')






