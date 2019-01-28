#Question 16  

library(RMySQL)
library(dplyr)

#Establishing a connection to the local MySQL Server
con <- dbConnect(MySQL(), user = "root", password = "sood", dbname = "Elephantiasis", host = "127.0.0.1")

#Gathering Data
result <- dbSendQuery(con, "SELECT * FROM dermato_examinations;")
dermato_examinations <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM contact;")
contact <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM admission_environmental;")
admission_environmental <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

dbDisconnect(con)

names(dermato_examinations)[1] <- "patient_id" #Renaming columns of a before joining

a <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 1 & frequency_ie == 1)
b <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 2 & frequency_ie == 1)
c <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 3 & frequency_ie == 1)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 4 & frequency_ie == 1)
e <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 5 & frequency_ie == 1)

#Generating Set 1
set1 <- merge(a, admission_environmental, by = c("patient_id"))
set1 <- set1[c("patient_id", "followup_code", "affected_nonaffected_limb", "frequency_ie", "living_area")]

#Generating Set 2
set2 <- merge(b, admission_environmental, by = c("patient_id"))
set2 <- set1[c("patient_id", "followup_code", "affected_nonaffected_limb", "frequency_ie", "living_area")]

#Generating Set 3
set3 <- merge(c, admission_environmental, by = c("patient_id"))
set3 <- set1[c("patient_id", "followup_code", "affected_nonaffected_limb", "frequency_ie", "living_area")]

#Generating Set 4
set4 <- merge(d, admission_environmental, by = c("patient_id"))
set4 <- set1[c("patient_id", "followup_code", "affected_nonaffected_limb", "frequency_ie", "living_area")]

#Generating Set 5
set5 <- merge(e, admission_environmental, by = c("patient_id"))
set5 <- set1[c("patient_id", "followup_code", "affected_nonaffected_limb", "frequency_ie", "living_area")]

#rm(a, b, c, d, con, huh, result)