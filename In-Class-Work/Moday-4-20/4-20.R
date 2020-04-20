library(ggplot2)
data(economics, package="ggplot2")
economics$index <- 1:nrow(economics)
economics <- economics[1:80,]
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10)
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25)
loessMod50 <- loess(uempmed ~ index, data = economics,span = 0.50)

smoothed10 <- predict(loessMod10)
smoothed25 <- predict(loessMod25)
smoothed50 <- predict(loessMod50)

plot(economics$uempmed,x=economics$date,type="l",main="Loess Smoothing and Prediction",
     xlab="Date",ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")


#cars example
data("cars")
str(cars)

plot(speed ~ dist,data = cars)
help("lowess")

lowess(cars$speed ~ cars$dist)
lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue")


lines(lowess(cars$speed ~ cars$dist, f=0.8), col="red")
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="green")
lines(lowess(cars$speed ~ cars$dist, f=0.1), col= 5)
lines(lowess(cars$speed ~ cars$dist, f=0.01), col= 6)

#LDA with iris dataset
library(MASS)
names(iris)
dim(iris)
head(iris)

set.seed(555)
Train <- sample(1:nrow(iris), nrow(iris)/2)
iris_Train <- iris[Train,]
irist_Test <- iris[-Train,]

help(lda)
fit1 <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data =iris_Train)

predict1 <- predict(fit1, iris_Train)
predict1_class <- predict1$class

table1 <- table(predict1_class, iris_Train$Species)
table1

sum(diag(table1))/sum(table1)





