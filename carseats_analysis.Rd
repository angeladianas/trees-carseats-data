library(ISLR)
library(tree)
help(Carseats)
attach(Carseats)
dim(Carseats)

## Part 1
High <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)
summary(Carseats)

random.seed(0820)
n = 300
train <- sample(1:nrow(Carseats), n)
test <- Carseats[-train,]
Hightest <- High[-train]
summary(test)

cstree <- tree(High~. - Sales, Carseats, subset = train)
summary(cstree)
cstree
plot(cstree)
text(cstree, col = "blue", adj = c(1.05,0), cex = 0.5)

cstree.pred <- predict(cstree, test, type = "class")
mean(cstree.pred == Hightest)

conftable <- table(cstree.pred, Hightest)
conftable
sens <- conftable[2,2]/(conftable[2,2] + conftable[1,2])
spec <- conftable[1,1]/(conftable[1,1] + conftable[2,1])
sens
spec
set.seed(0820)
cv.carseats <- cv.tree(cstree, FUN = prune.misclass, K = 10)
cv.carseats

plot(cv.carseats$size, cv.carseats$dev, type = "b", xlab = "Number of terminal nodes", ylab = "Deviance", main = "Cross-validation for Carseats data")
dev.new()
plot(cv.carseats$k, cv.carseats$dev, type = "b", xlab = "Parameter Alpha", ylab = "Deviance", main = "Cross-validation for Carseats data")

## best = 17 
prune.carseats <- prune.misclass(cstree, best = 17)
summary(prune.carseats)
prune.carseats
plot(prune.carseats)
text(prune.carseats, col = "blue", cex = 0.5)

prunecstree.pred <- predict(prune.carseats, test, type = "class")
mean(prunecstree.pred == Hightest)
prunecstree.pred
prune.table <- table(prunecstree.pred, Hightest)
prune.table

prune.sens <- prune.table[2,2] / (prune.table[2,2] + prune.table[1,2])
prune.spec <- prune.table[1,1] / (prune.table[1,1] + prune.table[2,1])
prune.sens
prune.spec

## best = 13
prune.carseats <- prune.misclass(cstree, best = 13)
summary(prune.carseats)
prune.carseats
plot(prune.carseats)
text(prune.carseats, col = "blue", cex = 0.5)

prunecstree.pred <- predict(prune.carseats, test, type = "class")
mean(prunecstree.pred == Hightest)
prunecstree.pred
prune.table <- table(prunecstree.pred, Hightest)
prune.table

prune.sens <- prune.table[2,2] / (prune.table[2,2] + prune.table[1,2])
prune.spec <- prune.table[1,1] / (prune.table[1,1] + prune.table[2,1])
prune.sens
prune.spec

## Part 2
tree.cars <- tree(Sales~. - High, Carseats, subset = train)
tree.cars
summary(tree.cars)
plot(tree.cars)
text(tree.cars, col = "blue", cex = 0.6)

cv.cars = cv.tree(tree.cars, K = 10)
cv.cars
plot(cv.cars$size, cv.cars$dev, type = "b", xlab = "Number of terminal nodes", ylab = "Deviance", main = "Cross-validation for Carseats data")
plot(cv.cars$k, cv.cars$dev, type = "b", xlab = "Parameter Alpha", ylab = "Deviance", main = "Cross-validation for Carseats data")

prune.cars <- prune.tree(tree.cars, best = 5)
summary(prune.cars)
prune.cars
plot(prune.cars)
text(prune.cars, col = "blue", cex = 0.6, adj = c(1.05, 0))

pred <- predict(tree.cars, newdata = Carseats[-train,])
cars.test <- Carseats[-train, "Sales"]
plot(pred, cars.test, xlab = " Prediction from Regression Tree", ylab = "Unit Sales (in thousands) at each location", main = "Prediction for Test dataset")
abline(0,1)

cars.train <- Carseats[train, "Sales"]

## MSE for data without pruning
mean((pred - cars.test)^2)
mean((pred - cars.train)^2)

ppred <- predict(prune.cars, newdata = Carseats[-train,])
plot(ppred, cars.test, xlab = " Prediction from Regression Tree", ylab = "Unit Sales (in thousands) at each location", main = "Prediction for Test dataset")
abline(0,1)

## MSE for data with pruning
mean((ppred - cars.test)^2)
mean((ppred - cars.train)^2)
