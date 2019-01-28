#Question 18 and 19, Chi-Squared Test and ANOVA

library(RMySQL)
library(dplyr)
library(MASS)
library(psych)
library(nlme)
library(car)

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
names(limb_vol_change)[1] <- "patient_id"
names(limb_vol_change)[3] <- "followup_no"

#Preprocessing of Pre and Post Yoga
followup_yoga_details$time_pre_yoga <- as.integer(followup_yoga_details$time_pre_yoga)
followup_yoga_details$time_post_yoga <- as.integer(followup_yoga_details$time_post_yoga)

followup_yoga_details <- followup_yoga_details[complete.cases(followup_yoga_details[, 7:8]), ]

followup_yoga_details$time_pre_yoga[followup_yoga_details$time_pre_yoga <= 30] <- 1
followup_yoga_details$time_pre_yoga[followup_yoga_details$time_pre_yoga > 30 & followup_yoga_details$time_pre_yoga <=60] <- 2
followup_yoga_details$time_pre_yoga[followup_yoga_details$time_pre_yoga > 60] <- 3

followup_yoga_details$time_post_yoga[followup_yoga_details$time_post_yoga <= 30] <- 1
followup_yoga_details$time_post_yoga[followup_yoga_details$time_post_yoga > 30 & followup_yoga_details$time_post_yoga <=60] <- 2
followup_yoga_details$time_post_yoga[followup_yoga_details$time_post_yoga > 60] <- 3

#Joining Yoga Table with Volume Change Table
merged_db <- merge(followup_yoga_details, limb_vol_change, by = c("patient_id", "followup_no"))
merged_db <- merged_db[, c("patient_id", "followup_no", "time_pre_yoga", "time_post_yoga", "vol_change")]

merged_db <- merged_db[complete.cases(merged_db[, 5]), ]
merged_db$vol_change <- as.numeric(merged_db$vol_change)

merged_db$vol_change[merged_db$vol_change >= 1.3] <- 5
merged_db$vol_change[merged_db$vol_change < 1.3 & merged_db$vol_change >= 0.89] <- 4
merged_db$vol_change[merged_db$vol_change < 0.89 & merged_db$vol_change >= 0.5] <- 3
merged_db$vol_change[merged_db$vol_change < 0.5 & merged_db$vol_change >= 0] <- 2
merged_db$vol_change[merged_db$vol_change < 0] <- 1

#Analysis for Pre-Yoga, Chi-Square Test
#Gathering data in form of time_pre_yoga and vol_change
tbl1 <- table(merged_db$time_pre_yoga, merged_db$vol_change)
chisq.test(tbl1)

#Analysis for Post-Yoga, Chi Square Test
#Gathering data in form of tome_post_yoga and vol_change
tbl2 <- table(merged_db$time_post_yoga, merged_db$vol_change)
chisq.test(tbl2)

#For Repeated Measures ANOVA
merged_db_pre <- merged_db[ , c(3, 1, 2, 5)]
merged_db_post <- merged_db[ , c(4, 1, 2, 5)]

merged_db_pre <- merged_db_pre [!duplicated(merged_db_pre[c(2, 3)]),]
merged_db_post <- merged_db_post [!duplicated(merged_db_post[c(2, 3)]),]

merged_db_pre$time_pre_yoga <- factor(merged_db_pre$time_pre_yoga, levels = unique(merged_db_pre$time_pre_yoga))
merged_db_post$time_post_yoga <- factor(merged_db_post$time_post_yoga, levels = unique(merged_db_post$time_post_yoga))

#Building ANOVA Model for time_pre_yoga
model_pre <- gls(vol_change ~ time_pre_yoga + followup_no + time_pre_yoga*followup_no, correlation = corAR1(form = ~ followup_no | patient_id, value = 0.8990), data = merged_db_pre, method = "REML")
anova_pre <- Anova(model_pre)

#Building ANOVA Model for time_post_yoga
model_post <- gls(vol_change ~ time_post_yoga + followup_no + time_post_yoga*followup_no, correlation = corAR1(form = ~ followup_no | patient_id, value = 0.8990), data = merged_db_post, method = "REML")
anova_post <- Anova(model_post)

#Removing unrequired variables
rm(con, result, huh)