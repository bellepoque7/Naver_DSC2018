# 출처: https://github.com/tirthajyoti/DeepNetworksR/blob/master/wine_classifier.R
# Load test and train set made by 현

whole <- read.csv('winequality-white.csv', sep= ',')
str(whole)

train <- read.csv("white_wine_stratified_train.csv", sep=',')
train <-as.data.frame(train)

test <- read.csv("white_wine_stratified_test.csv", sep=',')
test <- as.data.frame(test)

# Scale (normalize) the input data to make neural network work properly
train1<- as.data.frame(scale(train))      # 스케일링
test1<-as.data.frame(scale(test))         # 범주형 변수 Scale해도되나?
train<-train1
test<-test1

head(train)

# Create formula to use in the neural network model from column names of the data set
v<- colnames(test)

m<-v[2]
for (i in 3:length(v)){
  m<-paste(m,v[i], sep="+")
}
m<- paste(v[1],'~',m)
f<- as.formula(m)

# Load softmax neural network library and fit model
# 3 hidden layers of 5 neurons each, feel free to change and experiment
# Mini-batch size of 10; activation function = RELU, learning rate = 0.1
# Algorithm = rmsprop, maximum iterations = 500
# Feel free to play with these parameters and train your own model
install.packages('softmaxreg')
library(softmaxreg)

softmax.model<- softmaxReg(formula=f,data=train,hidden=c(2,1),
                           funName ="relu",type='class', 
                           batch=100 ,rang=1,rate=0.1,threshold = 0.01,maxit=1)

# Predit using the fitted model
# Show histogram of the predicted classes (as factors)
p<-predict(softmax.model,test[2:12])
hist(p,breaks=50,main = "Class of wine predicted by the neural network")

# Create classification error table for 3 classes and print overall accuracy
tab<- as.matrix(table(test$Class,p))
accu=(tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2])
print(paste("Accuracy:",round(accu,3)))