## EDA for white wines  
# on the Features : alcohol, volatile acidity, Residual sugar, pH, Free sulfur dioxide
# total 5

#https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

setwd('C:/Users/renz/Documents/GitHub/Naver_assingment(Hyun&Hyeri)')
whitewine <- read.csv('WHITEWINE_ORDINAL.csv', sep =',')
head(whitewine)


require(gglot2)
install.packages('ggplot2')
library(ggplot2)
install.packages('reshape2')
library(reshape2)

df.m <- melt(whitewine, id.var = 'Quality_O')
head(df.m)

p <- ggplot(data = df.m, aes(x = factor(Quality_O), y = value)) +geom_boxplot(aes(fill=variable))
p + facet_wrap( ~ variable, scale ='free') + labs(x = 'quality')
