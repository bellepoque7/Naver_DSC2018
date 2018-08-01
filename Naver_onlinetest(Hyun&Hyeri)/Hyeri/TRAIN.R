WWTRAIN<-read.table('C:/Users/USER/Documents/Naver(정&현)/Analysis of Wine Quality Data/white_wine_stratified_TRAIN.csv', header = T, sep = ',')
library(car)
options(digits=5)
options(scipen=999)

Qfit1 <- lm(quality ~ ., data=WWTRAIN)
summary(Qfit1)
vif(Qfit1)

Qfit2 <- lm(quality ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, data=WWTRAIN)
summary(Qfit2)
vif(Qfit2)


Qfit3_forward <- step(lm(quality ~ 1, WWTRAIN), scope=list(lower=~1,  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol), direction="forward")
#Qfit3_backward <- step(lm(quality ~ 1, WWTRAIN), scope=list(lower=~1,  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol), direction="backward")
Qfit3_both <- step(lm(quality ~ 1, WWTRAIN), scope=list(lower=~1,  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol), direction="both")

Qfit4 <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + sulphates + 
              free.sulfur.dioxide + fixed.acidity + total.sulfur.dioxide + 
              chlorides, data=WWTRAIN)
summary(Qfit4)
vif(Qfit4)

Qfit5 <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + sulphates +free.sulfur.dioxide + fixed.acidity , data=WWTRAIN)
summary(Qfit5)

vif(Qfit5)

library(MASS)

par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(residuals(Qfit5), h = 0.25, col="slategray3")
qqPlot(residuals(Qfit5), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals", outer=T, side=1, line = 2)

#distribution of residuals
par(mfrow=c(1,1))
pred.val <- round(fitted(Qfit5))
plot(pred.val, residuals(Qfit5))
ts.plot(residuals(Qfit5))
residualPlots(Qfit5, pch=19, col="grey", cex=0.6)

#이상관측치
require(car)
influencePlot(Qfit5,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

std.del.res<-studres(Qfit5)
truehist(std.del.res, h = 0.25, col="slategray3")
mtext("Histigram of Studentized Deleted Residuals", side=1, line=2, cex=0.8)

d.fit <- dffits(Qfit5)
truehist(d.fit, h = 0.25, col="slategray3")
mtext("Histigram of Studentized Deleted Residuals", side=1, line=2, cex=0.8)

cook.d <- cooks.distance(Qfit5)
ts.plot(cook.d)


par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(std.del.res, h = 0.55, col="slategray3")
mtext("Studentized Deleted Residuals", side=1, line=2, cex=0.8)

truehist(d.fit, h = 0.05, col="slategray3")
mtext("DFITS", side=1, line=2, cex=0.8)

par(mfrow=c(1,1), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
ts.plot(cook.d, col="dark blue")

