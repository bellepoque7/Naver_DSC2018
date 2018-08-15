# 와인에서는 support vector machine이 나온다.(2학기에 나오는데, 모델링은 심오하나 쓰긴 안어렵다)
# 양이 얼마없어서 논문에서 더 추가해서 얘기해도 좋을 것이다.
# 결과는 페이퍼형태 리포트 한 5-10장 사이로......네??
# 좋은 방향은 논문을 따라가보고 확인해보는 것.

getwd()

white_wine = read.csv('winequality-white.csv', sep=';', header=T)
a = white_wine
summary(white_wine)
write.xlsx(summary(white_wine),'C:/Users/jayjunglim/wine_trash/new.xlsx', row.names = F )
head(a)
str(a)  # 와인데이터의 str 보기 
# 와인데이터는 1599개의 관측치와 12개의 변수로 이루어져있다.
#  11개의 num 데이터와 quality는 int 변수로 이루어져있다.

# quality 


wine_quality = table(a['quality'])
barplot(wine_quality, main = "red wine quaility", 
        xlab = "region", ylab = "freq", col = 'lightblue', ylim = c(0,700))
# wine의 quality는 가장 낮은 값 3부터 8까지 나타내며
# 가장 많은 값이 5이다.


# 함수만들기

install.packages('doBy')
install.packages('xlsx')
install.packages('ggplot2')
library(ggplot2)
library(doBy)
library(xlsx)


head(iris)
makinghist <-function(dataset){
  png(file="C:/Users/jayjunglim/wine_trash/hist.png",
      width=600, height=350)
  ggplot(red_wine,aes(x = red_wine$quality))+ geom_density(ase(color = quality))
   dev.off()
   
   

g =ggplot(red_wine, aes(quality,fixed.acidity)) + 
  geom_density(color = 'lightblue')
g

makingplot <- function(dataset){
  png(file="C:/Users/jayjunglim/wine_trash/plot.png",
      width=600, height=350)
  a = boxplot(dataset~white_wine$quality, col = 'lightblue',
              xlab = 'quality')
  dev.off()
}



# fixed acidity

makinghist(red_wine$fixed.acidity)
makingplot(white_wine$fixed.acidity)


sheet = summaryBy(fixed.acidity ~ quality, data = red_wine, 
          FUN = list(mean, max, min, median, sd))
write.xlsx(sheet,'C:/Users/jayjunglim/wine_trash/new.xlsx', row.names = F)

# volatile.acidity

makinghist(red_wine$volatile.acidity)
makingplot(white_wine$volatile.acidity)


# citric.acid

makinghist(red_wine$citric.acid)
makingplot(white_wine$citric.acid)

# residual.sugar

makinghist(red_wine$residual.sugar)
makingplot(white_wine$residual.sugar)


# chlorides

makinghist(red_wine$chlorides)
makingplot(white_wine$chlorides)


# free.sulfur.dioxide

makinghist(red_wine$free.sulfur.dioxide)
makingplot(white_wine$free.sulfur.dioxide)

# total.sulfur.dioxide
makinghist(red_wine$total.sulfur.dioxide)
makingplot(white_wine$total.sulfur.dioxide)


# density
makinghist(red_wine$density)
makingplot(white_wine$density)

# pH
makinghist(red_wine$pH)
makingplot(white_wine$pH)

# sulphates
makinghist(red_wine$sulphates)
makingplot(white_wine$sulphates)

# alcohol
makinghist(red_wine$alcohol)
makingplot(white_wine$alcohol)



#------
# 중회귀분석 적용해보기 (는 어렵다 ☆)
#https://www.kaggle.com/ranjitmishra/multiple-linear-regression-wine-quality 


install.packages('ggplot2')
install.packages('readr')
install.packages('corrgram')
install.packages('car')
install.packages('FNN')
install.packages('pROC')

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(corrgram) # Correlograms http://www.datavis.ca/papers/corrgram.pdf
library(car) #required for nearest neighbors
library(FNN) # nearest neighbors techniques
library(pROC) # to make ROC curve


system("ls ../input")
wine_data <- read.csv("winequality-red.csv", sep=",",header=T)
head(wine_data)
summary(wine_data$quality)

linear_quality = lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=wine_data)
summary(linear_quality)

# 이것들 다 뭐냐...
corrgram(wine_data, lower.panel=panel.shade, upper.panel=panel.ellipse)
linear_quality_1 = lm(quality ~ alcohol, data = wine_data)
summary(linear_quality_1)                

linear_quality_4 = lm(quality ~ alcohol + volatile.acidity + citric.acid + sulphates, data = wine_data)
summary(linear_quality_4)


linear_quality.res = resid(linear_quality) # gets residuals
linear_quality_1.res = resid(linear_quality_1)
linear_quality_4.res = resid(linear_quality_4)

plot(wine_data$alcohol, linear_quality.res) # plot residuals against alcohol variable
points(wine_data$alcohol, linear_quality_1.res, col="red") # add the residuals for 1-dimension
points(wine_data$alcohol, linear_quality_4.res, col="blue") # add residuals for 4 dimension


plot(linear_quality_4)
vif(linear_quality_4)


anova(linear_quality_4)


linear_quality_5 = lm(quality ~ alcohol + volatile.acidity + sulphates, data = wine_data)
summary(linear_quality_5)
anova(linear_quality_5)
vif(linear_quality_5)

summary(linear_quality_5)
# 중회귀분석 # 중회귀분석


#------
# https://github.com/jackdbd/wine-classification/blob/master/wine.Rmd

install.packages('corrplot')
install.packages('caret')
install.packages('klaR')
install.packages('nnet')
install.packages('randomForest')
install.packages('gridExtra')
install.packages('doSNOW')

library(corrplot)  # graphical display of the correlation matrix
library(caret)     # classification and regression training
library(klaR)      # naive bayes
library(nnet)      # neural networks (nnet and avNNet)
library(kernlab)   # support vector machines (svmLinear and svmRadial)
library(randomForest)  # random forest, also for recursive feature elimination
library(gridExtra) # save dataframes as images
library(doSNOW)    # parallel processing
today <- as.character(Sys.Date())

red = read.csv('winequality-red.csv', sep=',', header=T)
white = read.csv('winequality-white.csv', sep=';', header=T)
head(red)
head(white)

red[, 'color'] <- 'red'
white[, 'color'] <- 'white'

df <- rbind(red, white)
df$color <- as.factor(df$color)


good_ones <- df$quality >= 6
bad_ones <- df$quality < 6
df[good_ones, 'quality'] <- 'good'
df[bad_ones, 'quality'] <- 'bad'  
df$quality <- as.factor(df$quality)

dummies <- dummyVars(quality ~ ., data = df)
df_dummied <- data.frame(predict(dummies, newdata = df))
df_dummied[, 'quality'] <- df$quality


# set the seed for reproducibility
set.seed(1234) 
trainIndices <- createDataPartition(df_dummied$quality, p = 0.7, list = FALSE)
train <- df_dummied[trainIndices, ]
test <- df_dummied[-trainIndices, ]

numericColumns <- !colnames(train) %in% c('quality', 'color.red', 'color.white')
correlationMatrix <- cor(train[, numericColumns])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.6)
colnames(correlationMatrix)[highlyCorrelated]

png(paste0(today, '-', 'correlation-matrix.png'))
corrplot(correlationMatrix, method = 'number', tl.cex = 0.5)
dev.off()

#EDA

# Normalize the quantitative variables to be within the [0,1] range
train_normalized <- preProcess(train[, numericColumns], method = 'range')
train_plot <- predict(train_normalized, train[, numericColumns])

# Let'��s take an initial peek at how the predictors separate on the target
png(paste0(today, '-', 'feature-plot.png'))
featurePlot(train_plot, train$quality, 'box')
dev.off()

# fitControl <- trainControl(method = 'repeatedcv', number = 5, repeats = 3)  # very slow
fitControl <- trainControl(method = 'cv', number = 5)

# Here the Naive Bayes classifier works best without range
fit_nb <- train(x = train[, features], y = train$quality,
                method ='nb',
                trControl = fitControl)
predict_nb <- predict(fit_nb, newdata = test[, features])
confMat_nb <- confusionMatrix(predict_nb, test$quality, positive = 'good')
importance_nb <- varImp(fit_nb, scale = TRUE)

png(paste0(today, '-', 'importance-nb.png'))
plot(importance_nb, main = 'Feature importance for Naive Bayes')
dev.off()


# tunable parameter: k = number of neighbours (set k as an odd number to avoid ties)
fit_knn <- train(x = train[, features], y = train$quality,
                 method = 'knn',
                 preProcess = 'range', 
                 trControl = fitControl, 
                 tuneGrid = expand.grid(.k = c(3, 5, 7, 9, 11, 15, 21, 25, 31, 41, 51, 75, 101)))  
predict_knn <- predict(fit_knn, newdata = test[, features])
confMat_knn <- confusionMatrix(predict_knn, test$quality, positive = 'good')
importance_knn <- varImp(fit_knn, scale = TRUE)

png(paste0(today, '-', 'importance-knn.png'))
plot(importance_knn, main = 'Feature importance for K Nearest Neighbors')
dev.off()  # 모델링... 뭐 알아야하지.