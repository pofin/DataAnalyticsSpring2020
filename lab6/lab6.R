library(e1071)
set.seed(1)

x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
x
y

plot(x,col=(3-y))

dat <- data.frame(x = x,y = as.factor(y))
svmfit <- svm(y ~., data=dat, kernel="linear",cost=10,scale=FALSE)

plot(svmfit, dat)

svmfit$index
summary(svmfit)

#continued on
set.seed(1)
tune.out <- tune(svm, y ~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

svmfit <- svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)

#data is linearly separable
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

dat=data.frame(x=x,y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel="linear",cost=1e5)
summary(svmfit)
plot(svmfit,dat)

#this model has lower cost leading to more support vectors, 
#wider margins, and one more error than previous model
#but is better suited for the data despite the error
svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit, dat)

#Gene expression
library(ISLR)
names(Khan)

dim(Khan$xtrain)
dim(Khan$xtest)

length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

dat <- data.frame(x=Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y ~., data=dat, kernel="linear",cost=10)
summary(out)

dat.te=data.frame(x=Khan$xtest, y= as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

#Group3 rpart labs
#1
require(rpart)

Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart,branch = 0,uniform = TRUE)
help(plot.rpart)
help(text.rpart)
text(Swiss_rpart, digits = 3, fancy = TRUE) 

#2
# Regression Tree Example
require(rpart)
# build the  tree
fitM <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fitM) # display the results
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM) # visualize cross-validation results
# plot tree
plot(fitM, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitM, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfitM<- prune(fitM, cp=0.06) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)
post(pfitM, file = "ptree2.ps", title = "Pruned Regression Tree for Mileage")

#3
library(e1071)
library(rpart)
data(Glass, package="mlbench")
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
printcp(rpart.model)
plotcp(rpart.model)

rsq.rpart(rpart.model)
print(rpart.model)

plot(rpart.model,compress=TRUE)
text(rpart.model, use.n=TRUE)
plot(rpart.pred)

#4
fitK <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
printcp(fitK) # display the results
plotcp(fitK) # visualize cross-validation results
summary(fitK) # detailed summary of splits
# plot tree
plot(fitK, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fitK, use.n=TRUE, all=TRUE, cex=.8)
# create attractive postscript plot of tree
post(fitK, file = "kyphosistree.ps", title = "Classification Tree for Kyphosis") # might need to convert to PDF (distill)

pfitK<- prune(fitK, cp=   fitK$cptable[which.min(fitK$cptable[,"xerror"]),"CP"])
plot(pfitK, uniform=TRUE, main="Pruned Classification Tree for Kyphosis")
text(pfitK, use.n=TRUE, all=TRUE, cex=.8)
post(pfitK, file = "ptree.ps", title = "Pruned Classification Tree for Kyphosis")




