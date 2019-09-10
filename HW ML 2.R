#Decision Tree
x <- read.csv("https://raw.githubusercontent.com/lidyaoctaviani/dataset/master/Data.csv", sep = ";")

x1 <- sapply(x[,8],switch,"IT"=1,"RandD"=2,"accounting"=3,"techninal"=4, "hr"=5, "management"=6, "product_mng"=7, "marketing"=8, "support"=9, "sales"=10)
x2 <- sapply(x[,9], switch, "low"=1, "medium"=2, "high"=3)
  
# Split in train + test set
x$left <- as.factor(x$left)
idxs <- sample(1:nrow(x),as.integer(0.8*nrow(x)))
trainData <- x[idxs,]
testData <- x[-idxs,]

#Predict
library(rpart)
tree <- rpart(left ~ ., 
              data = data.frame(trainData), method = "class")
predict(tree, data.frame(testData),type="class" )

#Confusion Matrix
conf1 <- table(testData[,'left'],predict(tree, data.frame(testData),type="class"))
conf1

#Accuracy Decision Tree
TP1 <- conf1[1, 1] 
FN1 <- conf1[1, 2] 
FP1 <- conf1[2,1] 
TN1 <- conf1[2,2] 
acc1 <- (TP1+TN1)/(TP1+FN1+FP1+TN1)
acc1


#Random Forest 
library(randomForest)

# randomforest training 
randomFor <- randomForest(left ~ ., data = data.frame(trainData), ntree=10000, importance = TRUE)

#Predict
predict(randomFor, data.frame(testData),type="class")

#Confusion Matrix
conf2 <- table(testData[,'left'],predict(tree, data.frame(testData), type="class"))
conf2

#Accuracy Random Forest
TP2 <- conf2[1, 1] 
FN2 <- conf2[1, 2] 
FP2 <- conf2[2,1] 
TN2 <- conf2[2,2] 
acc2 <- (TP2+TN2)/(TP2+FN2+FP2+TN2)
acc2


#Logistic Regression 

# training model with logistic regression
logitMod <- glm(left ~ satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+sales+salary, data=x, family=binomial(link="logit"))
summary(logitMod)

#predict 
glm.probs <- predict(logitMod,type = "response")
glm.probs[1:5]

#Confusion Matrix
glm.pred <- ifelse(glm.probs > 0.5, "1", "0")
attach(x)
conf3 <- table(glm.pred,x$left)

TP3 <- conf3[1, 1] 
FN3 <- conf3[1, 2] 
FP3 <- conf3[2,1] 
TN3 <- conf3[2,2] 
acc3 <- (TP3+TN3)/(TP3+FN3+FP3+TN3)
acc3 
