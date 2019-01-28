#Repeated Measures ANOVA to find out the change in frequency_ie over followups

library(RMySQL)
library(dplyr)
library(car)

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

a <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 1)
b <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 2)
c <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 3)
d <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 4)
e <- subset(dermato_examinations, affected_nonaffected_limb == 1 & followup_code == 5)

#Generating Set 1
set1 <- merge(a, admission_environmental, by = c("patient_id"))
set1 <- set1[c("patient_id", "frequency_ie")]
names(set1)[2] <- "frequency_ie1"
set1 <- set1[complete.cases(set1[, 2]), ]

#Generating Set 2
set2 <- merge(b, admission_environmental, by = c("patient_id"))
set2 <- set2[c("patient_id", "frequency_ie")]
names(set2)[2] <- "frequency_ie2"
set2 <- set2[complete.cases(set2[, 2]), ]

#Generating Set 3
set3 <- merge(c, admission_environmental, by = c("patient_id"))
set3 <- set3[c("patient_id", "frequency_ie")]
names(set3)[2] <- "frequency_ie3"
set3 <- set3[complete.cases(set3[, 2]), ]

#Generating Set 4
set4 <- merge(d, admission_environmental, by = c("patient_id"))
set4 <- set4[c("patient_id", "frequency_ie")]
names(set4)[2] <- "frequency_ie4"
set4 <- set4[complete.cases(set4[, 2]), ]

#Generating Set 5
set5 <- merge(e, admission_environmental, by = c("patient_id"))
set5 <- set5[c("patient_id", "frequency_ie")]
names(set5)[2] <- "frequency_ie5"
set5 <- set5[complete.cases(set5[, 2]), ]

#Generating the final set
set <- merge(set1, set2, by = "patient_id")
set <- merge(set, set3, by = "patient_id")
set <- merge(set, set4, by = "patient_id")
set <- merge(set, set5, by = "patient_id")

#Generating the possible levels of frequency_ie
freq_levels <-  c(1, 2) #Levels
freq_factor <- as.factor(freq_levels)
freq_frame <- data.frame(freq_factor)

freq_bind <- cbind(set$frequency_ie1, set$frequency_ie2, set$frequency_ie3, set$frequency_ie4, set$frequency_ie5)

freq_model <- lm(freq_bind~1)

analysis <- aov(freq_model, idata = freq_frame, idesign = ~freq_factor)

rm(a, b, c, d, e, set1, set2, set3, set4, set5, con, huh, result)