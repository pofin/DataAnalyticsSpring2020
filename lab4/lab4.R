set.seed(12345)
help(par)

par(mar = rep(0.2,4))
data_Matrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

#heatmap function
help("heatmap")
help(rep)

par(mar=rep(0.2,4))
heatmap(data_Matrix)

#coin flip
help("rbinom")

set.seed(678910)
for(i in 1:40){
  coin_Flip <- rbinom(1, size = 1,prob = 0.5)
  if(coin_Flip){
    data_Matrix[i,] <- data_Matrix[i,] + rep(c(0,3), each=5)
  }
}

par(mar=rep(0.2,4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

par(mar=rep(0.2,4))
heatmap(data_Matrix)

hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab = "The Row Mean", ylab = "Row", pch=19)
plot(colMeans(data_Matrix_Ordered), xlab = "Column", ylab = "Column Mean", pch =19)

#Bronx examples
library("readxl")
bronx1 <- read_xls("rollingsales_bronx.xls", range = "A5:U5273" ,col_names = TRUE)
bronx1
View(bronx1)

attach(bronx1)
`SALE
PRICE` <- sub("\\$","",`SALE
PRICE`)
`SALE
PRICE` <- as.numeric(gsub(",","", `SALE
PRICE`))
`GROSS SQUARE FEET` <- as.numeric(gsub(",","", `GROSS SQUARE FEET`))
`LAND SQUARE FEET` <- as.numeric(gsub(",","", `LAND SQUARE FEET`))
plot(log(`GROSS SQUARE FEET`), log(`SALE
PRICE`))

sum(is.na(log(`SALE
PRICE`)))

m1 <- lm(log(`SALE
PRICE`)~log(`GROSS SQUARE FEET`))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))


detach(bronx1)
#Titanic
data("Titanic")

#rpart
library(rpart)
library(rpart.plot)
str(Titanic)
Titanic.df <- as.data.frame(Titanic)

Titanic.df <- Titanic.df[,1:4]
Titanic.df

Titanic.df[Titanic.df['Survived'] == 'No', 'Survive'] = 0
Titanic.df[Titanic.df['Survived'] == 'Yes', 'Survive'] = 1
Titanic.df['Survived']= NULL

Titanic.df["Freq"]= NULL
head(Titanic.df)

Titanic.df$Survive <- as.numeric(Titanic.df$Survive)



Titanic_rpart <- rpart(Survive ~., data=Titanic.df, method = "class")
Titanic_rpart

rpart.plot(Titanic_rpart, type = 3, fallen.leaves = TRUE)

rpart.plot(Titanic_rpart, type= 3,digits =3, fallen.leaves = TRUE)

#ctree
require(party)
Titanic.df <- as.data.frame(Titanic)
Titanic.df
treeTitanic <- ctree(Survived ~., data = Titanic.df)
plot(treeTitanic)

#hclust
help(hclust)


#randomforest
library(randomForest)

set.seed(7117)
train <- sample(nrow(Titanic.df), 0.7*nrow(Titanic.df), replace = FALSE)
TrainSet <- Titanic.df[train,]
ValidSet <- Titanic.df[-train,]

modelT <- randomForest(Survived ~ ., data = TrainSet, importance = TRUE)
modelT

plot(modelT)
