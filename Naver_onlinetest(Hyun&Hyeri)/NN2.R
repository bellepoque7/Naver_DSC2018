install.packages('corrplot'); library(corrplot)  # graphical display of the correlation matrix
install.packages('caret'); library(caret)     # classification and regression traininglibrary(klaR)      # naive bayes
install.packages('nnet');library(nnet)      # neural networks (nnet and avNNet)
install.packages('kernlab');library(kernlab)   # support vector machines (svmLinear and svmRadial)
install.packages('randomForest');library(randomForest)  # random forest, also for recursive feature elimination
install.packages('gridExtra');library(gridExtra) # save dataframes as images
install.packages('doSNOW')
library(doSNOW)    # parallel processing
registerDoSNOW(makeCluster(3, type = 'SOCK')) 

red <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', header = TRUE, sep = ';')
white <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', header = TRUE, sep = ';')

red[, 'color'] <- 'red'
white[, 'color'] <- 'white'
df <- rbind(red, white)
df$color <- as.factor(df$color)

good_ones <- df$quality >= 6
bad_ones <- df$quality < 6
df[good_ones, 'quality'] <- 'good'
df[bad_ones, 'quality'] <- 'bad'  
df$quality <- as.factor(df$quality)

head(df)

dummies <- dummyVars(quality ~ ., data = df)
df_dummied <- data.frame(predict(dummies, newdata = df))
df_dummied[, 'quality'] <- df$quality
