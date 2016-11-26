#Stage 4 Feature Selection using all base variables

install.packages("ROCR")
install.packages("Boruta")
library(ROCR)
library(Boruta)

#remove hemoglobin, meanbp_mean, and chloride_max due to high correlations
icustays1 <- icustays1[,c(-12,-24,-26)]
#remove ID attributes
icustays1 <- icustays1[,c(-1,-2)]

#Boruta feature selection on training set
traindata_idx <- sample(nrow(icustays1), floor(nrow(icustays1)*0.70))
#there are 5147 records in training set
length(traindata_idx)

traindata <- icustays1[traindata_idx,]
testdata <- icustays1[-traindata_idx,]
#convert categorical variables into factor data type
convert <- c(1,14)
traindata[,convert] <- data.frame(apply(traindata[,convert], 2, as.factor))
testdata[,convert] <- data.frame(apply(testdata[,convert], 2, as.factor))

#implement Boruta
set.seed(123)
boruta.train <- Boruta(hospital_expire_flag~., data = traindata, doTrace=2)
print(boruta.train)

#plot variable importance chart
#blue boxplots are minimal, avg, and max Z score of shadow attribute
#red, yellow, green boxplots are the Z scores for rejected, tentative, and confirmed attributes
plot(boruta.train, main ="Tentative Variable Importance Chart", xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i) boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz, median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

#finalize decision about tentative attributes
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

#plot final boruta variable importance chart
#green = confirmed attributes, red = rejected attributes
plot(final.boruta, main= "Final Variable Importance Chart", xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i) final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz, median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)

#list of confirmed attributes
getSelectedAttributes(final.boruta, withTentative = F)

#dataframe of final results
boruta.df <- attStats(final.boruta)
print(boruta.df)

#remove gender and ethnicity because Boruta rejected them
traindata <- traindata[,c(-3,-5)]
testdata <- testdata[,c(-3,-5)]
