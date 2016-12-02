#Stage 4: Mortality Prediction 
#Classification using GLM and SVM
install.packages("e1071")
install.packages("caret")
install.packages("rpart")
install.packages("ada")
install.packages("ROCR")
library(e1071)
library(caret)
library(rpart)
library(ada)
library(ROCR)

##STEP 0: Prepare train and test data for two different feature sets (baseline and severity score)

#baseline train and test data
bl_data <- icustays1

#severity score train test data
ss_data <- icustays[,c(3,4,6,8,17,18)]

set.seed(123)
traindata_idx <- sample(nrow(bl_data), floor(nrow(bl_data)*0.70))

#there are 5147 records in training set
length(traindata_idx)

traindata_bl <- bl_data[traindata_idx,]
testdata_bl <- bl_data[-traindata_idx,]
traindata_ss <- ss_data[traindata_idx,]
testdata_ss <- ss_data[-traindata_idx,]

#convert categorical variables: hospital_expire_flag and mechvent into factor data type
convert_bl <- c(1,12)
traindata_bl[,convert_bl] <- data.frame(apply(traindata_bl[,convert_bl], 2, as.factor))
testdata_bl[,convert_bl] <- data.frame(apply(testdata_bl[,convert_bl], 2, as.factor))

traindata_ss$hospital_expire_flag <- factor(traindata_ss$hospital_expire_flag)
testdata_ss$hospital_expire_flag <- factor(testdata_ss$hospital_expire_flag)


##STEP 1: Logistic Regression

# PART A: for baseline model
model.lr_bl <- glm(hospital_expire_flag ~ ., data=traindata_bl, family="binomial")
prediction.lr_bl <- predict(model.lr_bl, newdata = testdata_bl[,-1], type = "response")
pred <- prediction(prediction.lr_bl, testdata_bl[,1])
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
# 0.8275102
auc

#create confusion matrix for logistic regression
model_pred_mortality <- rep(0, 2207)
model_pred_mortality[prediction.lr_bl > 0.5] = 1

#test values top columns
tab_lr_bl <- table(model_pred_mortality, testdata_bl[,1])
confusionMatrix(tab_lr_bl)

#misclassification error, needs to be low ~ 0.165836
mean(model_pred_mortality != testdata_bl[,1])

#PART B: for severity score model
model.lr_ss <- glm(hospital_expire_flag ~ ., data=traindata_ss, family="binomial")
prediction.lr_ss <- predict(model.lr_ss, newdata = testdata_ss[,-1], type = "response")
pred_ss <- prediction(prediction.lr_ss, testdata_ss[,1])
perf_ss <- performance(pred_ss, measure = "tpr", x.measure = "fpr")
plot(perf_ss)
auc_ss <- performance(pred_ss, measure = "auc")
auc_ss <- auc_ss@y.values[[1]]
# 0.7954815
auc

#create confusion matrix for logistic regression
model_pred_mortality_ss <- rep(0, 2207)
model_pred_mortality_ss[prediction.lr_ss > 0.5] = 1

#test values top columns
tab_lr_ss <- table(model_pred_mortality_ss, testdata_ss[,1])
confusionMatrix(tab_lr_ss)

#misclassification error, needs to be low ~ 0.183507
mean(model_pred_mortality_ss != testdata_ss[,1])


##STEP 2: SVM

#Part A: baseline model

#choose parameters using tune() function (cost, gamma) 
tuned_bl <- tune.svm(hospital_expire_flag~., data = traindata_bl, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned_bl)

#best parameters are gamma = 0.01 and cost = 10
svm.model_bl  <- svm(hospital_expire_flag~., data=traindata_bl, kernel = "radial", gamma = 0.01, cost = 10) 
summary(svm.model_bl)
prediction_bl <- predict(svm.model_bl, testdata_bl[,-1])
tab_bl <- table(pred = prediction_bl, true = testdata_bl[,1])
tab_bl

#accuracy of 0.8414
confusionMatrix(tab_bl)

tuned_bl$best.performance


#Part B: severity scores model

#choose parameters using tune() function (cost, gamma) 
tuned_ss <- tune.svm(hospital_expire_flag~., data = traindata_ss, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned_ss)

#best parameters are gamma = 0.1 and cost = 10
svm.model_ss  <- svm(hospital_expire_flag~., data=traindata_ss, kernel = "radial", gamma = 0.1, cost = 10) 
summary(svm.model_ss)
prediction_ss <- predict(svm.model_ss, testdata_ss[,-1])
tab_ss <- table(pred = prediction_ss, true = testdata_ss[,1])
tab_ss

#accuracy of 0.8169
confusionMatrix(tab_ss)
tuned_ss$best.performance

