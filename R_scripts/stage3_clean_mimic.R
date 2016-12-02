#Stage 3: Data Cleaning & Pre-processing
require(ggplot2)
require(lattice)
install.packages("corrplot")
library(corrplot)


icustays <- read.csv(file ='D:/files/icupatients.csv')
summary(icustays)

#check data characteristics
head(icustays)
tail(icustays)
str(icustays)

#for age above 89, set to '90'
icustays$age[icustays$age>90] <- 90

# icustays$age <- cut(icustays$age, breaks = c(18, 20, 30, 40, 50, 60, 70, 80, 90),
#                    labels = c("under 20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "over 80"),
#                    right = TRUE)

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

#separate continuous vars to plot histograms
icustays2 <- icustays1[,c(6,8:16,18:32)]

#histograms with density lines for all continuous baseline variables
par(mfrow=c(4, 7))
colnames <- dimnames(icustays2)[[2]]
for (i in 1:25) {
  hist(icustays2[,i], main=colnames[i], probability=TRUE, col="gray", border="white")
  d <- density(icustays2[,i])
  lines(d, col="red")
}

#histograms for severity scores
#SOFA SCORE
par(mfrow=c(1,2))
hist(icustays[,17], main="SOFA Score", col="gray", border="white")
hist(icustays[,18], main="SAPS Score", col="gray", border="white")
#hist(icustays[,19], main="OASIS Score", col="gray", border="white")

#pie chart for admission types
ggplot(icustays, aes(x=factor(1), fill=admission_type))+
  geom_bar(width = 1)+
  coord_polar("y") + ggtitle("Patient Admission Types")

#pie chart for mortality outcomes
icustays_mort <- icustays
icustays_mort$hospital_expire_flag <- factor(icustays_mort$hospital_expire_flag)
ggplot(icustays_mort, aes(x=factor(1), fill=hospital_expire_flag))+
  geom_bar(width = 1)+
  coord_polar("y") + ggtitle("Patient Outcomes")

#correlation matrix to show correlation between numerical attributes other than hospital_expiry_flag and
#categorical attributes
cor_matrix <- cor(icustays1[,c(6,8:16,18:32)])
corrplot(cor_matrix, type ="lower", order="hclust", tl.col="black", tl.srt=27)
corrplot(cor_matrix, type="lower", order="hclust", tl.col="black", tl.srt=27, method = "number", number.cex=0.7)

#features will be removed in Stage 4 scripts
