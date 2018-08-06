## EDA for white wines  

#https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/




#----
#cor plot  그림5 각 변수 별 상관정도

setwd('C:/Users/renz/Documents/GitHub/naverstudy/Naver_onlinetest(Hyun&Hyeri)')
whitewine <- read.csv('winequality-white.csv', sep = ',')
head(whitewine)
install.packages('corrplot')
library(corrplot)

M <- cor(whitewine)
corrplot(M, method = 'ellipse')


#----
#상중하로 나누어진 whitewine데이터에 대한 barplot 
setwd('C:/Users/renz/Documents/GitHub/Naver_assingment(Hyun&Hyeri)')
whitewine <- read.csv('WHITEWINE_ORDINAL.csv', sep =',')
head(whitewine)



install.packages('ggplot2'); library(ggplot2)

df.m <- melt(whitewine, id.var = 'Quality_O')
head(df.m)

p <- ggplot(data = df.m, aes(x = factor(Quality_O), y = value)) +geom_boxplot(aes(fill=variable))
p + facet_wrap( ~ variable, scale ='free') + labs(x = 'quality')
