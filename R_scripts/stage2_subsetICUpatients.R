#STAGE 2 (Building MIMIC Database in Postgres)

#import icustay_detail created in Postgres and take only subset of patients meeting conditions
icustays <- read.csv(file = "D:/files/icustaysfull.csv")

#must be adults over 18 and this must be their first ICU stay
icustays_subset <- subset(icustays, age > 18 & first_icu_stay=='Y' & first_hosp_stay =='Y')

#cohort = lived: 34053 and died: 4415
table(icustays_subset$hospital_expire_flag)

#import original ICUSTAYS CSV file from stage 1
icustayoriginal <- read.csv(file ='D:/ICUSTAYS-full.csv/ICUSTAYS.csv')

#create new dataframe where only patient IDs from above subset match ICUSTAYS original 
new_icustays <- icustayoriginal[icustayoriginal$ICUSTAY_ID %in% icustays_subset$icustay_id,]

#save to drive to later load data into table icustays in Postgres
#now there is less data to work with & more appropriate data for predictive model
write.csv(new_icustays, file='D:/icustaysnew.csv')
