# WQD1.EDA
# Sample R code for EDA


install.packages('doBy')
install.packages('xlsx')
library(MASS)
library(doBy)
library(xlsx)

WhiteWine = read.csv('winequality_white.csv', sep=';')
head(WhiteWine)

# Quality 범주를 묶어서 정리해야함. -> 추후 수정
# 내가 만든 요약 통계량 넣을거
# 내가 만든 자동 파일만들기 함수 넣기.. paste함수 필ㅇ

#------
# functions
brief <- function(target){
  summaryBy(target ~ quality, data = WhiteWine, 
           FUN = list(mean, max, min, median, sd))
}



#------
attach(WhiteWine)

# 1. Qualtiy 히스토그램
par(mfrow=c(1,1), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
barplot((table(quality)), ylim = c(0, 2500)
        , col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("Quality", side=1, outer=F, line=2, cex=0.8)
mtext("Counts", side=2, outer=F, line=2, cex= 1.0)

# 2. Fixed Acidity 히스토그램
truehist(fixed.acidity, h = 0.5, col="slategray3", ylim = c(0,0.6), xlim = c(2,12))
mtext("Fixed Acidity", side=1, outer=F, line=2, cex=0.8)
mtext("Freq", side=2, outer=F, line=2, cex=1.0)             # y축 Freq 맞는지확인필요

# 2. Fixed.Acidity 요약통계
brief(fixed.acidity)                                      # 함수 재정리하기

# 3. volatile.acidity 히스토그램 
truehist(volatile.acidity, h = 0.05, col="slategray3", ylim = c(0,5))
mtext("Volatile Acidity", side=1, outer=F, line=2, cex=0.8)         # y축 뭔지 확인필요

# 4. citric.acid 
truehist(citric.acid, h = 0.1, col="slategray3", xlim = c(0, 1.3), ylim = c(0,4))
mtext("Citric Acid", side=1, outer=F, line=2, cex=0.8)


# 왜 하다말았지?




#-------
# boxplot

par(mfrow=c(1,1), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(fixed.acidity, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)           # y축 확인 필요


boxplot(volatile.acidity, col="slategray2", pch=19)
mtext("Volatile Acidity", cex=0.8, side=1, line=2)        # y축 확인 필요

boxplot(citric.acid, col="slategray2", pch=19)
mtext("Citric Acid", cex=0.8, side=1, line=2)             # y축 확인 필ㅇ

boxplot(residual.sugar, col="slategray2", pch=19)
mtext("Residual Sugar", cex=0.8, side=1, line=2)요        # y축 확인 필요  

boxplot(chlorides, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)                # y축 확인 필ㅇ




# Sample R code for Summary Statistics & Correlations

summary(WhiteWine)
install.packages("psych") 
library(psych)


#----
#여기서부터 못함


describe(WhiteWine)

cor(WhiteWine[,-12])                                           # cor : corrleation.... # quality뺴고.
cor(WhiteWine[,-12], method="spearman")                        # spareman물어보기
pairs(WhiteWine[,-12], gap=0, pch=19, cex=0.4, col="darkblue")
title(sub="Scatterplot of Chemical Attributes", cex=0.8)

#Table: Pearson’s Correlation
#Table: Spearman Rank Correlation
#Scatterplot of Predictors

# Sample R code for Preparing Data

limout <- rep(0,11)
for (i in 1:11){
  t1 <- quantile(WhiteWine[,i], 0.75)
  t2 <- IQR(WhiteWine[,i], 0.75)
  limout[i] <- t1 + 1.5*t2
}
WhiteWineIndex <- matrix(0, 4898, 11)
for (i in 1:4898)
  for (j in 1:11){
    if (WhiteWine[i,j] > limout[j]) WhiteWineIndex[i,j] <- 1
  }
WWInd <- apply(WhiteWineIndex, 1, sum)
WhiteWineTemp <- cbind(WWInd, WhiteWine)
Indexes <- rep(0, 208)
j <- 1
for (i in 1:4898){
  if (WWInd[i] > 0) {Indexes[j]<- i
  j <- j + 1}
  else j <- j
}
WhiteWineLib <-WhiteWine[-Indexes,]   # Inside of Q3+1.5IQR
indexes = sample(1:nrow(WhiteWineLib), size=0.5*nrow(WhiteWineLib))
WWTrain50 <- WhiteWineLib[indexes,]
WWTest50 <- WhiteWineLib[-indexes,]


#WQD2.Multiple Regression
# Sample R code for Multiple Regression

Qfit1 <- lm(quality ~ ., data=WWTrain50)
summary(Qfit1)
vif(Qfit1)
Qfit3 <- step(lm(quality ~ 1, WWTrain50), scope=list(lower=~1,  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol), direction="forward")
Qfit4 <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + free.sulfur.dioxide +
              sulphates + chlorides + pH , data=WWTrain50)
summary(Qfit4)
vif(Qfit4)
Qfit5 <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + rt.sulfur.dioxide +
              sulphates + chlorides + pH , data=WWTrain50)
summary(Qfit5)
vif(Qfit5)
par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(residuals(Qfit4), h = 0.25, col="slategray3")
qqPlot(residuals(Qfit4), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals", outer=T, side=1, line = 2)
par(mfrow=c(1,1))
pred.val <- round(fitted(Qfit4))
plot(pred.val, residuals(Qfit4))
ts.plot(residuals(Qfit4))
residualPlots(Qfit4, pch=19, col="blue", cex=0.6)
influencePlot(Qfit4,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
boxcox(lm(quality~alcohol), data=WWTrain50, lambda=seq(-0.2, 1.0, len=20), plotit=T)
std.del.res<-studres(Qfit4)
truehist(std.del.res, h = 0.25, col="slategray3")
mtext("Histigram of Studentized Deleted Residuals", side=1, line=2, cex=0.8)
d.fit <- dffits(Qfit4)
truehist(std.del.res, h = 0.25, col="slategray3")
truehist(d.fit, h = 0.25, col="slategray3")
mtext("Histigram of Studentized Deleted Residuals", side=1, line=2, cex=0.8)
cook.d <- cooks.distance(Qfit4)
ts.plot(cook.d)
par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(std.del.res, h = 0.55, col="slategray3")
mtext("Studentized Deleted Residuals", side=1, line=2, cex=0.8)
truehist(d.fit, h = 0.05, col="slategray3")
mtext("DFITS", side=1, line=2, cex=0.8)
par(mfrow=c(1,1), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
ts.plot(cook.d, col="dark blue")

#Model I: All predictors in the model
#Model II: After removal of density VIFs improved
#Model III: Working model
#Model IV: Final model

#Sample R code for Final Model

Qfit6 <- lm(quality ~ poly(alcohol,2) + poly(volatile.acidity,2) + residual.sugar + poly(free.sulfur.dioxide,2) + chlorides + sulphates + poly(pH,2), data=WWTrain50In)
summary(Qfit6)
residualPlots(Qfit6, pch=19, col="blue", cex=0.6)

#WQD4. Applying Tree-Based Methods

#Sample R code for Tree-based Models and Random Forest

FactQ <- as.factor(quality)
WhiteWineLib <- cbind(WhiteWineLib, FactQ)
temp <- recode(WhiteWineLib$FactQ, "c('3','4','5')='10'; c('6')='20'; else='40'")
Ptemp <- recode(temp, "c('10')='5'; ('20')='6'; else='7'")
WhiteWineLib$FactQ <- Ptemp
prop.table(table(WhiteWineLib$FactQ))
WhiteWineTree <- tree(FactQ ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol+density, data=WhiteWineLib, method="class")
plot(WhiteWineTree)
text(WhiteWineTree, pretty=0, cex=0.6)
misclass.tree(WhiteWineTree, detail=T)
Treefit1 <- predict(WhiteWineTree, WhiteWineLib, type="class")
table(Treefit1, WhiteWineLib$FactQ)


#WQD5. Random Forest

WWrf50_super <- randomForest(FactQ ~ . , data=WWTrain50T[,-12], ntree=150, importance=T, proximity=T)
WWTest50_rf_pred_super <- predict(WWrf50_super, WWTest50, type="class")
table(WWTest50_rf_pred_super, WWTest50$FactQ1)
plot(WWrf50_super, main="")
varImpPlot(WWrf50_super,  main="", cex=0.8)


#WQD6. Classification
nn.result <- knn(WWTrain50S, WWTrain50S, cl=WWTrain5SC$FactQ, k=5)


