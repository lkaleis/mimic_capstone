#Stage 4
install.packages("ROCR")
library(ROCR)

#logistic regression
model <- glm(hospital_expire_flag ~.,family=binomial(link='logit'),data=traindata)


fitted.results <- predict(model,newdata=subset(testdata,select=c(2:25)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != testdata$hospital_expire_flag)
print(paste('Accuracy',1-misClasificError))
#accuracy is 0.82646"

p <- predict(model, newdata=subset(testdata,select=c(2:25)), type="response")
pr <- prediction(p, testdata$hospital_expire_flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
