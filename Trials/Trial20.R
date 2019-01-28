#Question 22, Correlation between outcome variables

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

result <- dbSendQuery(con, "SELECT * FROM limb_vol_change;")
limb_vol_change <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM limb_data_revised;")
limb_data_revised <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM grade_skinthickness;")
grade_skinthickness <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

dbDisconnect(con)

#Renaming columns for continuity among tables
names(dermato_examinations)[1] <- "patient_id"
names(dermato_examinations)[4] <- "followup_no"
names(limb_data_revised)[1] <- "patient_id"
names(limb_data_revised)[4] <- "followup_no"
names(grade_skinthickness)[1] <- "patient_id"
names(grade_skinthickness)[4] <- "followup_no"

#Keeping only the columns required for analysis
dermato_examinations <- dermato_examinations[, c("patient_id", "followup_no", "affected_nonaffected_limb", "bep", "frequency_ie")]
limb_data_revised <- limb_data_revised[, c("patient_id", "followup_no", "affected_nonaffected_limb", "volume")]

#BEP Variable in dermato_examinations
bep1 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 1 & bep == 1)
bep2 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 2 & bep == 1)
bep3 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 3 & bep == 1)
bep4 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 4 & bep == 1)
bep5 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 5 & bep == 1)

#Volume in limb_data_revised
ldr1 <- subset(limb_data_revised, affected_nonaffected_limb == 1 & followup_no == 1)
ldr2 <- subset(limb_data_revised, affected_nonaffected_limb == 1 & followup_no == 2)
ldr3 <- subset(limb_data_revised, affected_nonaffected_limb == 1 & followup_no == 3)
ldr4 <- subset(limb_data_revised, affected_nonaffected_limb == 1 & followup_no == 4)
ldr5 <- subset(limb_data_revised, affected_nonaffected_limb == 1 & followup_no == 5)

#Frequncey_ie variable in dermato_examinations
fie1 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 1 & frequency_ie == 1)
fie2 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 2 & frequency_ie == 1)
fie3 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 3 & frequency_ie == 1)
fie4 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 4 & frequency_ie == 1)
fie5 <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_no == 5 & frequency_ie == 1)

#Grade variable in grade_skinthickness
#Grade variable is not populated so should not be counted
#gs1 <- subset(grade_skinthickness, affected_nonaffected_limb == 1 & followup_no == 1 & grade == 1)
#gs2 <- subset(grade_skinthickness, affected_nonaffected_limb == 1 & followup_no == 2 & grade == 1)
#gs3 <- subset(grade_skinthickness, affected_nonaffected_limb == 1 & followup_no == 3 & grade == 1)
#gs4 <- subset(grade_skinthickness, affected_nonaffected_limb == 1 & followup_no == 4 & grade == 1)
#gs5 <- subset(grade_skinthickness, affected_nonaffected_limb == 1 & followup_no == 5 & grade == 1)

#Relation between bep being 1 and volume
bep_ldr1 <- merge(bep1, ldr1, by = c("patient_id", "followup_no", "affected_nonaffected_limb"))
bep_ldr2 <- merge(bep2, ldr2, by = c("patient_id", "followup_no", "affected_nonaffected_limb"))
bep_ldr3 <- merge(bep3, ldr3, by = c("patient_id", "followup_no", "affected_nonaffected_limb"))
bep_ldr4 <- merge(bep4, ldr4, by = c("patient_id", "followup_no", "affected_nonaffected_limb"))
bep_ldr5 <- merge(bep5, ldr5, by = c("patient_id", "followup_no", "affected_nonaffected_limb"))
