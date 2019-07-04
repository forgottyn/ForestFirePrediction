rm(list=ls())    

#load data
data <- read.table("forestfires_Modify.csv", sep = ",",header=TRUE)
#explore data
head(data, n = 3)

#first I would like to run a correlation test 
cor.data <- cor(data[, 1:13])
cor.data[upper.tri(cor.data)] <- NA  
cor.data

#fire season is highly correlated with month dc and temp,so we can delete that
data <- data[, -5]
cor.data <- cor(data[, 1:12])
cor.data[upper.tri(cor.data)] <- NA  
cor.data
#then dc(month is still a important info, so i thought we should delete dc)
data <- data[, -7]
cor.data <- cor(data[, 1:11])
cor.data[upper.tri(cor.data)] <- NA  
cor.data

head(data, n = 3)
####################################
########## Decision Tree ###########
####################################
library('rpart')
library('tree')
library('rattle')
set.seed(0415)
MAdata <- data
head(MAdata, n = 2)
train <- sample(1:nrow(MAdata), .8*nrow(MAdata))

tree1 <- tree(area~., 
              data = MAdata, 
              subset = train)
summary(tree1)
plot(tree1)
text(tree1, pretty = 0)
cv.fire <- cv.tree(tree1)
plot(cv.fire$size, cv.fire$dev, type = 'b')

prune.fire <- prune.tree(tree1, best = 3)
plot(prune.fire)
text(prune.fire, pretty = 0)

yhat= predict (prune.fire , newdata = MAdata[-train ,])
fire.test= MAdata[-train ,"area"]
plot(yhat ,fire.test)
abline (0 ,1)
(mse <- (mean((yhat-fire.test)^2))) #calculate MSE
(sd <- sqrt(mse))
(lb <- mean(fire.test - 2 * sd))
(ub <- mean(fire.test + 2 * sd))

# treer.fire <- rpart(area ~ .,
#                     data = MAdata[train, ],
#                     method = 'anova',
#                     parms = list(split = "information"),
#                     control = rpart.control(minsplit = 2, minbucket = 1, cp = 0))
# #par(mfrow=c(1,1))
# #fancyRpartPlot(treer.fire, main="Maximal Decision Tree")
# 
# #prune the tree
# treer.fire$cptable
# xerr<-treer.fire$cptable[,"xerror"]
# (minxerr<-which.min(xerr))
# mincp<-treer.fire$cptable[3,"CP"]
# 
# #use the minerror as 3 according to the one sd rule
# treerfire.prune<-prune(treer.fire,cp=mincp)
# fancyRpartPlot(treerfire.prune, main="Decision Tree With Minimum C.V. Error")
# asRules(treerfire.prune)
# 
# yhat=predict(treerfire.prune,newdata=MAdata[-train, ])
# area.test<-MAdata[-train, 12]
# plot(area.test, yhat, type='p')
# (mse <- (mean((yhat-area.test)^2))) #calculate MSE
# (sd <- sqrt(mse))
# (lb <- mean(area.test - 2 * sd))
# (ub <- mean(area.test + 2 * sd))

#############################
###########SVM###############
#############################
set.seed(0415)
svmdata <- MAdata
library('e1071')
#try radial
svmfit1 = svm(area~., 
              data = svmdata[train,], 
              kernel = "radial", 
              cost = 1, 
              gamma = 1)
summary(svmfit1)

yhat=predict(svmfit1,newdata=MAdata[-train, ])
area.test<-MAdata$area[-train]
plot(area.test, yhat, type='p')
(mse <- (mean((yhat-area.test)^2))) #calculate MSE
(sd <- sqrt(mse))
(lb <- mean(area.test - 2 * sd))
(ub <- mean(area.test + 2 * sd))

#after trying these two models, we find that the outliers could really
#affect model. so we decide to delete data that is too big
data <- data[-which(data$area > 110),]


####################################
######### Decision Tree v2###########
####################################
set.seed(0415)
MAdata <- data
head(MAdata, n = 2)
train <- sample(1:nrow(MAdata), .8*nrow(MAdata))

tree1 <- tree(area~., 
              data = MAdata, 
              subset = train)
summary(tree1)
plot(tree1)
text(tree1, pretty = 0)
cv.fire <- cv.tree(tree1)
plot(cv.fire$size, cv.fire$dev, type = 'b')

prune.fire <- prune.tree(tree1, best = 3)
plot(prune.fire)
text(prune.fire, pretty = 0)

yhat= predict (prune.fire , newdata = MAdata[-train ,])
fire.test= MAdata[-train ,"area"]
plot(yhat ,fire.test)
abline (0 ,1)
(mse <- (mean((yhat-fire.test)^2))) #calculate MSE
(sd <- sqrt(mse))
(lb <- mean(fire.test - 2 * sd))
(ub <- mean(fire.test + 2 * sd))

# treer.fire <- rpart(area ~ .,
#                     data = MAdata[train, ],
#                     method = 'anova',
#                     parms = list(split = "information"),
#                     control = rpart.control(minsplit = 2, minbucket = 1, cp = 0))
# #fancyRpartPlot(treer.fire, main="Maximal Decision Tree")
# 
# #prune the tree
# treer.fire$cptable
# xerr<-treer.fire$cptable[,"xerror"]
# (minxerr<-which.min(xerr))
# mincp<-treer.fire$cptable[2,"CP"] 
# #use the minerror as 3 according to the one sd rule
# treerfire.prune<-prune(treer.fire,cp=mincp)
# fancyRpartPlot(treerfire.prune, main="Decision Tree With Minimum C.V. Error")
# asRules(treerfire.prune)
# 
# yhat=predict(treerfire.prune,newdata=MAdata[-train, ])
# area.test<-MAdata[-train, 12]
# plot(area.test, yhat, type='p')
# (mse <- (mean((yhat-area.test)^2))) #calculate MSE
# (sd <- sqrt(mse))
# (lb <- mean(area.test - 2 * sd))
# (ub <- mean(area.test + 2 * sd))

#############################
###########SVM v2###############
#############################
set.seed(0415)
svmdata <- MAdata
library('e1071')
#try radial
svmfit1 = svm(area~., 
              data = svmdata[train,], 
              kernel = "radial", 
              cost = 1, 
              gamma = 1)
summary(svmfit1)

yhat=predict(svmfit1,newdata=MAdata[-train, ])
area.test<-MAdata$area[-train]
plot(area.test, yhat, type='p')
(mse <- (mean((yhat-area.test)^2))) #calculate MSE
(sd <- sqrt(mse))
(lb <- mean(area.test - 2 * sd))
(ub <- mean(area.test + 2 * sd))

#it's still not quite good, so we turned to classfication model

