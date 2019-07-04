rm(list=ls())

#load data
data <- read.table("forestfires_Modify.csv", sep = ",",header=TRUE)
#explore data
head(data, n = 2)
datas <- data[-which(data$area > 110),]
plot(sort(datas$area))
#first I would like to run a correlation test 
#cor.data <- cor(data[, 1:13])
#cor.data[upper.tri(cor.data)] <- NA  
#cor.data

#fire season is highly correlated with month dc and temp,so we can delete that
datas <- datas[, -5]
#cor.data <- cor(data[, 1:12])
#cor.data[upper.tri(cor.data)] <- NA  
#cor.data
#then dc(month is still a important info, so i thought we should delete dc)
datas <- datas[, -7]
#cor.data <- cor(data[, 1:11])
#cor.data[upper.tri(cor.data)] <- NA  
#cor.data
head(datas, n = 2)
FireLevel <- ifelse(datas$area > 0, 1, 0)
classdata <- data.frame(datas[, 1:11], FireLevel)
head(classdata, n = 2)  

#############################
######classification tree####
library('rattle')
library('rpart')
library('pROC')
set.seed(0415)
treedata <- classdata
head(treedata, n = 2)

#set train and test data
nobs <- nrow(treedata)
train <- sample(1:nobs, .85*nobs)
test <- setdiff(1:nobs, train)
train_tree <- treedata[train,]
test_tree <- treedata[test,]

#train model
treec.fire <- rpart(FireLevel ~ .,
                    data = train_tree,
                    method = 'class',
                    parms = list(split = "information"),
                    control = rpart.control(minsplit = 2, minbucket = 1, cp = 0))

#Prune it Back
min.xerror <-  which.min(treec.fire$cptable[,'xerror'])
min.xerror
treec.fire$cptable 
#Row 3 has xerror of 0.8691589 and std of 0.04804930; 
#no row prior to it is within 1 std, so use it)
best_cp <- treec.fire$cptable[3,'CP']
prune_tree <- prune(treec.fire, cp = best_cp)

#Visualize It and print Rules
#par(mfrow=c(1,1))
#fancyRpartPlot(prune_tree)
#asRules(prune_tree)

#Evaluate Test Set
prune.preds <- predict(prune_tree, newdata = test_tree, type = 'class')
mytable <- table(test_tree$FireLevel, prune.preds,dnn=c("Actual", "Predicted"))
mytable

#calculate the rates for accuracy
ov_co <- (mytable[1, 1] + mytable[2, 2])/sum(mytable)
ov_er <- 1 - ov_co
tp1_er <- mytable[1, 2]/sum(mytable[1, ])
tp2_er <- mytable[2, 1]/sum(mytable[2, ])

Model<- c()
CorrectRate <- c()
ErrorRate <- c()
Type1 <- c()
Type2 <- c()

Model[1] <- "DeTree"
CorrectRate[1] <- ov_co
ErrorRate[1] <- ov_er
Type1[1] <- tp1_er
Type2[1] <- tp2_er

#############################
######random forest##########
#############################
library('randomForest')
set.seed(0416)
randata <- classdata
head(randata, n = 2)
train <- sample(1:nobs, .85*nobs)
test <- setdiff(1:nobs, train)
train_tree <- randata[train,]
test_tree <- randata[test,]

###Grow a Random Forest with ntree=500, mtry=4
ranfor_fire <- randomForest(as.factor(FireLevel) ~ .,
                            data=train_tree,
                            ntree=2500,mtry=4,
                            importance=TRUE, 
                            na.action=na.roughfix,
                            replace=TRUE)
ranfor_fire

oob.err<-rep(0,13)
min.ntree<-rep(0,13)
for(mtry in 1:13){
  rf_mtry <- randomForest(as.factor(FireLevel) ~ .,
                          data=train_tree, 
                          ntree=2000, mtry=mtry,
                          importance=TRUE, 
                          na.action=na.roughfix,
                          replace=TRUE)
  min.ntree[mtry] <- which(rf_mtry$err.rate[,"OOB"]== min(rf_mtry$err.rate[,"OOB"]))[1]
  oob.err[mtry] <- rf_mtry$err.rate[which(rf_mtry$err.rate[,"OOB"]== min(rf_mtry$err.rate[,"OOB"]))[1]]
}
matplot(1:mtry,c(oob.err),pch=19,col="blue",type="b",ylab="OOB error",xlab = "mtry")

#one standard deciation rule 
sd(oob.err)+min(oob.err)
result<-cbind(oob.err,min.ntree)
names(result)<-c('OOB error','ntree')
result
# obb.err = 0.3874710  when min.ntree = 1283
rf.final <- randomForest(as.factor(FireLevel) ~ .,
                        data=train_tree, 
                        ntree=1293, mtry=6,
                        importance=TRUE, 
                        na.action=na.roughfix,
                        replace=TRUE)
###List the importance of the variables.
rn <- round(importance(rf.final), 2)
rn[order(rn[,3], decreasing=TRUE),]
###Display a chart of Variable Importance
varImpPlot(rf.final, main="Variable Importance in the Random Forest")

#Evaluate Test Set
random.preds <- predict(rf.final, newdata = test_tree, type = 'class')
mytable <- table(test_tree$FireLevel, random.preds,dnn=c("Actual", "Predicted"))
mytable

#calculate the rates for accuracy
ov_co <- (mytable[1, 1] + mytable[2, 2])/sum(mytable)
ov_er <- 1 - ov_co
tp1_er <- mytable[1, 2]/sum(mytable[1, ])
tp2_er <- mytable[2, 1]/sum(mytable[2, ])

Model[2] <- "Random"
CorrectRate[2] <- ov_co
ErrorRate[2] <- ov_er
Type1[2] <- tp1_er
Type2[2] <- tp2_er

#############################
###########SVM###############
#############################
set.seed(0415)
svmdata <- classdata
library('e1071')
#try radial
svmfit1 = svm(as.factor(FireLevel)~., 
              data = svmdata[train,], 
              kernel = "radial", 
              cost = 1, 
              gamma = 1)
summary(svmfit1)

dat.te <- svmdata[test, ]
pred.te= predict(svmfit1, newdata = dat.te)
mytable <- table(pred.te, dat.te$FireLevel)
mytable
#calculate the rates for accuracy
ov_co <- (mytable[1, 1] + mytable[2, 2])/sum(mytable)
ov_er <- 1 - ov_co
tp1_er <- mytable[1, 2]/sum(mytable[1, ])
tp2_er <- mytable[2, 1]/sum(mytable[2, ])

Model[3] <- "SVM"
CorrectRate[3] <- ov_co
ErrorRate[3] <- ov_er
Type1[3] <- tp1_er
Type2[3] <- tp2_er

###comprare different model
comparison <- data.frame(Model, CorrectRate, ErrorRate, Type1, Type2)
comparison
