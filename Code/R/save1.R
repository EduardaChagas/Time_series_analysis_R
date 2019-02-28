library(probsvm)

# iris data #
data(iris)
iris.x=iris[c(1:20,51:70,101:120),-5]
iris.y=iris[c(1:20,51:70,101:120),5]
iris.test=iris[c(21:50,71:100,121:150),-5]
a = probsvm(iris.x,iris.y,type="ovo",
            Inum=10,fold=2,lambdas=2^seq(-10,10,by=3))
predict(a, iris.test)

library(e1071)

data(iris)
iris.x=iris[c(1:20,51:70,101:120),-5]
iris.y=iris[c(1:20,51:70,101:120),5]
model <- svm(iris.x, iris.y)

plot(model)

(pred <- predict(model, head(iris), decision.values = TRUE))

attr(pred, "decision.values")

tobj <- tune.svm(type ~ ., data = spam_train[1:300,], gamma = 10^(-6:-3), cost = 10^(1:2))
summary(tobj)

