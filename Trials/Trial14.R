#Question 11

library(RMySQL)
library(dplyr)

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

#Generating Set 1
set1 <- subset(dermato_examinations, affected_nonaffected_limb == 2 & followup_code == 2 & frequency_ie == 1)

#Generating Set 2
a <- subset(followup_yoga_details, practicing_yoga == 1 & followup_no == 1)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 1)
c <- subset(followup_compression_details, compression_practice == 2 & followup_no == 1)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 3 & frequency_ie == 1)

names(d)[1] <- "patient_id" #Renaming columns of d before joining
names(d)[4] <- "followup_no"

a <- merge(a, b, by = c("patient_id"))
a <- merge(a, c, by = c("patient_id"))
set2 <- merge(a, d, by = c("patient_id"))

#Generating Set 3
a <- subset(followup_yoga_details, practicing_yoga == 1 & followup_no == 2)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 2)
c <- subset(followup_compression_details, compression_practice == 2 & followup_no == 2)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 4 & frequency_ie == 1)

names(d)[1] <- "patient_id" #Renaming columns of d before joining
names(d)[4] <- "followup_no"

a <- merge(a, b, by = c("patient_id"))
a <- merge(a, c, by = c("patient_id"))
set3 <- merge(a, d, by = c("patient_id"))

#Generating Set 4
a <- subset(followup_yoga_details, practicing_yoga == 1 & followup_no == 3)
b <- subset(followup_imld_details, practicing_imld == 2 & followup_no == 3)
c <- subset(followup_compression_details, compression_practice == 2 & followup_no == 3)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 5 & bep == 1)

names(d)[1] <- "patient_id" #Renaming columns of d before joining
names(d)[4] <- "followup_no"

a <- merge(a, b, by = c("patient_id"))
a <- merge(a, c, by = c("patient_id"))
set4 <- merge(a, d, by = c("patient_id"))


dbDisconnect(con)
#rm(a, b, c, d, con, huh, result)