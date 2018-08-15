WWTRAIN_O<-read.table('C:/Users/renz/Documents/GitHub/naverstudy/Naver_onlinetest(Hyun&Hyeri)/Hyeri/WHITEWINE_TRAIN_O.csv', header = T, sep = ',')
drops = 'X'
WWTRAIN_O <- WWTRAIN_O[,!(names(WWTRAIN) %in% drops)]



#-----
# 다중회귀 - 다중공선성 호가인
install.packages('car', dep = T)
library(car)
Qfit1 <- lm(TARGET ~ ., data=WWTRAIN_O)
summary(Qfit1)                         # density의 다중공선성이 너무 크게나와서 제
vif(Qfit1)                             # 다중 공선성이란 독립변수들관의 높은 선형관계가 있는           
                                       # residual sugar도 12인데 왜 제거안했을까?
                                       # A) sugar와 density가 높은 상관관계를 가져서 둘중하나만 빼면 되기 때
                                       # 교호작용은 왜 생각안할까?


Qfit2 <- lm(TARGET ~. - density, data=WWTRAIN_O)    # density만 빼고  다중회귀분
summary(Qfit2)
vif(Qfit2)

#-----
# 변수선택, 전진선택법 및 단계별 선택법

Qfit3_forward <- step(lm(TARGET ~ 1, WWTRAIN_O),   # 하나씩 변수를를 넣어가면서AIC가 가장 작은 값을 찾는다 
  scope=list(lower=~1,  
  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol)
  , direction="forward")                      # 결과 AIC =3208.1 이다

#------
# 후진선택법                                  # 이거는 왜 빼놨을까? -3208 AIC이면
                                              # forward보다 결과 좋은것인데
Qfit3_backward <- step(lm(TARGET ~. - density, WWTRAIN_O), 
                       direction="backward")

# 단계별 선택법 
# 이것도 왜 실행 안했고 안됐지..? 단계별은 어떻게 시작해야하ㅏ나?
Qfit3_both <- step(lm(TARGET ~ 1, WWTRAIN_O), scope=list(lower=~1,  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol), 법
                   direction="both")


Qfit4 <- lm(TARGET ~ alcohol + volatile.acidity + residual.sugar + sulphates + 							
              free.sulfur.dioxide + total.sulfur.dioxide + pH + citric.acid							
            , data=WWTRAIN_O) # 총 8개의 변수
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

#----
# student, Cook'sD, DFFITS 를 통해 잔차의 결과에 대해서확인

library(MASS)

par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)  # QQplot을 그리는게 전체 잔차에 대한것이라하
truehist(residuals(Qfit4), h = 0.25, col="slategray3")
qqPlot(residuals(Qfit4), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals", outer=T, side=1, line = 2)
par(mfrow=c(1,1))

#----
# 각 변수에대한 부분 잔차를 확인해보자
#distribution of residuals

pred.val <- round(fitted(Qfit4))
plot(pred.val, residuals(Qfit4))
ts.plot(residuals(Qfit4))

residualPlots(Qfit4, pch=19, col="grey", cex=0.6)

#-----
# 이건 뭐에대한거지?cook'd 같기도 한데... 
#?̻?????ġ
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

