WWTRAIN_O<-read.table('C:/Users/USER/Documents/Naver(정&현)/Analysis of Wine quality Data/WHITEWINE_TRAIN_O.csv', header = T, sep = ',')
library(car)
options(digits=5)
options(scipen=999)

Qfit1 <- lm(TARGET ~ ., data=WWTRAIN_O)
summary(Qfit1)
vif(Qfit1)

Qfit2 <- lm(TARGET ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, data=WWTRAIN_O)
summary(Qfit2)
vif(Qfit2)

Qfit3_forward <- step(lm(TARGET ~ 1, WWTRAIN_O), scope=list(lower=~1,  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol), direction="forward")
#Qfit3_backward <- step(lm(TARGET ~ 1, WWTRAIN_O), scope=list(lower=~1,  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol), direction="backward")
Qfit3_both <- step(lm(TARGET ~ 1, WWTRAIN_O), scope=list(lower=~1,  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol), direction="both")

Qfit4 <- lm(TARGET ~ alcohol + volatile.acidity + residual.sugar + sulphates + 							
              free.sulfur.dioxide + total.sulfur.dioxide + pH + citric.acid							
            , data=WWTRAIN_O)
summary(Qfit4)
vif(Qfit4)

Qfit5 <- lm(TARGET ~ alcohol + volatile.acidity + total.sulfur.dioxide + free.sulfur.dioxide + citric.acid, 
          data=WWTRAIN_O)
summary(Qfit5)
vif(Qfit5)

Qfit6 <- lm(TARGET ~ alcohol + residual.sugar+volatile.acidity + sulphates+pH, 
            data=WWTRAIN_O)
summary(Qfit6)
vif(Qfit6)

Qfit7 <- lm(TARGET ~ alcohol + residual.sugar+volatile.acidity + sulphates, 
            data=WWTRAIN_O)
summary(Qfit7)
vif(Qfit7)



library(MASS)

par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(residuals(Qfit4), h = 0.25, col="slategray3")
qqPlot(residuals(Qfit4), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals", outer=T, side=1, line = 2)
par(mfrow=c(1,1))

#distribution of residuals
pred.val <- round(fitted(Qfit4))
plot(pred.val, residuals(Qfit4))
ts.plot(residuals(Qfit4))


residualPlots(Qfit4, pch=19, col="grey", cex=0.6)

#이상관측치
require(car)
influencePlot(Qfit4,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
boxcox(lm(TARGET~alcohol), data=WWTRAIN_O, lambda=seq(-0.2, 1.0, len=20), plotit=T)

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

