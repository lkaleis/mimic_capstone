#Stage 3: Data Cleaning & Pre-processing

install.packages("Boruta")
install.packages("corrplot")
library(corrplot)
library(Boruta)

icustays <- read.csv(file ='D:/files/icupatients.csv')
summary(icustays)

#check data characteristics
head(icustays)
tail(icustays)
str(icustays)

#find all missing values
icustays[icustays == ""] <- NA
sapply(icustays, function(x) sum(is.na(x)))
anyNA(icustays)


#since almost half of records missing height, remove column
icustays <- icustays[,-10]

#keep only records that are complete
icustays <- icustays[complete.cases(icustays),]
#or use this to do the same
icustays <- na.omit(icustays)
anyNA(icustays)

#number of patients who died = 5826 lived and 1528 died
table(icustays$hospital_expire_flag)

#remove los_icu and los_hospital because this is prediction after first 24 hours only
icustays <- icustays[,c(-3,-8)]

#remove all three severity scores because they are outcome variables based on variables included here
icustays1 <- icustays[,c(-17,-18,-19)]

#correlation matrix to show correlation between numerical attributes other than hospital_expiry_flag and
#categorical attributes
cor_matrix <- cor(icustays1[,c(6,8:16,18:32)])
corrplot(cor_matrix, type ="lower", order="hclust", tl.col="black", tl.srt=27)
corrplot(cor_matrix, type="lower", order="hclust", tl.col="black", tl.srt=27, method = "number", number.cex=0.7)

#remove hemoglobin, meanbp_mean, and chloride_max due to high correlations
icustays1 <- icustays1[,c(-12,-24,-26)]
#remove ID attributes
icustays1 <- icustays1[,c(-1,-2)]

# # mat : is a matrix of data
# # ... : further arguments to pass to the native R cor.test function
# cor.mtest <- function(mat, ...) {
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat<- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       tmp <- cor.test(mat[, i], mat[, j], ...)
#       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#     }
#   }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# # matrix of the p-value of the correlation
# p.mat <- cor.mtest(icustays1[,c(6,8:16,18:32)])
# head(p.mat[, 1:5])
# 
# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(cor_matrix, method="color", col=col(200),number.cex=0.5,type="upper", order="hclust", addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, #Text label color and rotation
#          # Combine with significance
#          p.mat = p.mat, sig.level = 0.01, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE 
# )

# #remove highly correlated features 
# install.packages("mlbench")
# install.packages("caret")
# library(caret)
# set.seed(7)
# corrmatrix <- cor(icustays1[,c(6,8:16,18:32)])
# print(corrmatrix)
# corrplot(corrmatrix, method="number", p.mat=p.mat, sig.level = 0.5, type="upper", order="hclust", tl.col="black")
# highcorr <- findCorrelation(corrmatrix, cutoff=0.75)
# print(highcorr)