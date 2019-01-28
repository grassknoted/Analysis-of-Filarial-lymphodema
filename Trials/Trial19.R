#Question 20, lack of data

library(RMySQL)
library(dplyr)
library(MASS)

#Establishing a connection to the local MySQL Server
con <- dbConnect(MySQL(), user = "root", password = "sood", dbname = "Elephantiasis", host = "127.0.0.1")

#Gathering Data
result <- dbSendQuery(con, "SELECT * FROM dermato_examinations;")
dermato_examinations <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM followup_yoga_details;")
followup_yoga_details <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM followup_compression_details;")
followup_compression_details <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM followup_imld_details;")
followup_imld_details <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM limb_vol_change;")
limb_vol_change <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM limb_data_revised;")
limb_data_revised <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

dbDisconnect(con)

names(dermato_examinations)[1] <- "patient_id"
names(dermato_examinations)[4] <- "followup_no"
names(limb_data_revised)[1] <- "patient_id"
names(limb_data_revised)[4] <- "followup_no"

#Inflammatory Episodes
#Generating Set 1
set1 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 2 & frequency_ie == 1)

#Generating Set 2
a <- subset(followup_yoga_details, practicing_yoga == 2 & followup_no == 1)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 1)
c <- subset(followup_compression_details, compression_practice == 1 & followup_no == 1)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 3 & frequency_ie == 1)

a <- merge(a, b, by = "patient_id")
set2 <- merge(a, c, by = "patient_id")
#set2 <- merge(a, d, by = "patient_id")

#Generating Set 3
a <- subset(followup_yoga_details, practicing_yoga == 2 & followup_no == 2)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 2)
c <- subset(followup_compression_details, compression_practice == 1 & followup_no == 2)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & frequency_ie == 1 & followup_no == 4)

a <- merge(a, b, by = "patient_id")
set3 <- merge(a, c, by = "patient_id")
#set3 <- merge(a, d, by = "patient_id")

#Generating Set 4
a <- subset(followup_yoga_details, practicing_yoga == 2 & followup_no == 3)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 3)
c <- subset(followup_compression_details, compression_practice == 1 & followup_no == 3)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & frequency_ie == 1 & followup_no == 5)

a <- merge(a, b, by = "patient_id")
set4 <- merge(a, c, by = "patient_id")
#set4 <- merge(a, d, by = "patient_id")

#Limb Volume
#Generating Set 5
set5 <- subset(limb_data_revised, affected_nonaffected_limb = 1, followup_no == 2)

#Generating Set 6
a <- subset(followup_yoga_details, practicing_yoga == 2 & followup_no == 1)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 1)
c <- subset(followup_compression_details, compression_practice == 1 & followup_no == 1)
d <- subset(limb_data_revised, affected_nonaffected_limb == 1 & followup_no == 3)

a <- merge(a, b, by = "patient_id")
set6 <- merge(a, c, by = "patient_id")
#set6 <- merge(a, d, by = "patient_id")

#Generating Set 7
a <- subset(followup_yoga_details, practicing_yoga == 2 & followup_no == 2)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 2)
c <- subset(followup_compression_details, compression_practice == 1 & followup_no == 2)
d <- subset(limb_data_revised, affected_nonaffected_limb == 1 & followup_no == 4)

a <- merge(a, b, by = "patient_id")
set7 <- merge(a, c, by = "patient_id")
#set7 <- merge(a, d, by = "patient_id")

#Generating Set 8
a <- subset(followup_yoga_details, practicing_yoga == 2 & followup_no == 3)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 3)
c <- subset(followup_compression_details, compression_practice == 1 & followup_no == 3)
d <- subset(limb_data_revised, affected_nonaffected_limb == 1 & followup_no == 5)

a <- merge(a, b, by = "patient_id")
set8 <- merge(a, c, by = "patient_id")
#set8 <- merge(a, d, by = "patient_id")

#BEP
#Generating Set 9
set9 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & bep == 1 & followup_no == 2)

#Generating Set 10
a <- subset(followup_yoga_details, practicing_yoga == 2 & followup_no == 1)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 1)
c <- subset(followup_compression_details, compression_practice == 1 & followup_no == 1)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & bep == 1 & followup_no == 3)

a <- merge(a, b, by = "patient_id")
set10 <- merge(a, c, by = "patient_id")
#set10 <- merge(a, d, by = "patient_id")

#Generating Set 11
a <- subset(followup_yoga_details, practicing_yoga == 2 & followup_no == 2)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 2)
c <- subset(followup_compression_details, compression_practice == 1 & followup_no == 2)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & bep == 1 & followup_no == 4)

a <- merge(a, b, by = "patient_id")
set11 <- merge(a, c, by = "patient_id")
#set11 <- merge(a, d, by = "patient_id")

#Generating Set 12
a <- subset(followup_yoga_details, practicing_yoga == 2 & followup_no == 3)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 3)
c <- subset(followup_compression_details, compression_practice == 1 & followup_no == 3)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & bep == 1 & followup_no == 5)

a <- merge(a, b, by = "patient_id")
set12 <- merge(a, c, by = "patient_id")
#set12 <- merge(a, d, by = "patient_id")

#Removing unrequired variables
rm(a, b, c, d, con, result, huh)