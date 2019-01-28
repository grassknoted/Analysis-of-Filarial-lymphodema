#Question 33, effect of varicose on recovery from Elephantiasis

library(RMySQL)
library(dplyr)
library(MASS)
library(psych)
library(nlme)
library(car)

#Establishing a connection to the local MySQL Server
con <- dbConnect(MySQL(), user = "root", password = "sood", dbname = "Elephantiasis", host = "127.0.0.1")

#Gathering Data
result <- dbSendQuery(con, "SELECT * FROM baseline_comorbidities_data;")
baseline_comorbidities_data <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM limb_vol_change;")
limb_vol_change <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

dbDisconnect(con)

names(limb_vol_change)[1] <- "patient_id"
names(limb_vol_change)[3] <- "followup_no"

has_vericose <- subset(baseline_comorbidities_data, vericocity == 1)
has_vericose <- merge(has_vericose, limb_vol_change, by = "patient_id")
has_vericose$followup_no <- as.integer(has_vericose$followup_no)
has_vericose <- has_vericose[, c("patient_id", "followup_no", "vol_change")]

no_vericose <- subset(baseline_comorbidities_data, vericocity == 2)
no_vericose <- merge(no_vericose, limb_vol_change, by = "patient_id")
no_vericose$followup_no <- as.integer(no_vericose$followup_no)
no_vericose <- no_vericose[, c("patient_id", "followup_no", "vol_change")]

#Preprocessing before plotting the graph 
avg_vericose <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6), "avg_change" = 0, "count" = 0)
avg_no_vericose <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6), "avg_change" = 0, "count" = 0)

for(i in 1:nrow(has_vericose)){
  avg_vericose$avg_change[has_vericose$followup_no[i]] <- avg_vericose$avg_change[has_vericose$followup_no[i]] + has_vericose$vol_change[i]
  avg_vericose$count[has_vericose$followup_no[i]] <- avg_vericose$count[has_vericose$followup_no[i]] + 1
}

for(i in 1:nrow(no_vericose)){
  avg_no_vericose$avg_change[no_vericose$followup_no[i]] <- avg_no_vericose$avg_change[no_vericose$followup_no[i]] + no_vericose$vol_change[i]
  avg_no_vericose$count[no_vericose$followup_no[i]] <- avg_no_vericose$count[no_vericose$followup_no[i]] + 1
}

for(i in 1:6){
  avg_vericose$avg_change[i] <- avg_vericose$avg_change[i]/avg_vericose$count[i]
  avg_no_vericose$avg_change[i] <- avg_no_vericose$avg_change[i]/avg_no_vericose$count[i]
}

avg_vericose$avg_change[1] <- 0
avg_no_vericose$avg_change[1] <- 0

maxi <- max(avg_vericose$avg_change, avg_no_vericose$avg_change) + 0.25
mini <- min(avg_vericose$avg_change, avg_no_vericose$avg_change) - 0.25

plot(x = avg_vericose$followup_no, y = avg_vericose$avg_change, type = "o", main = "Change in Volume vs Followups for Patient affected by Vericose in the Past", xlab = "Followup Code", ylab = "Volume Change", col = "blue", ylim = c(mini, maxi))
par(new = TRUE)
plot(x = avg_no_vericose$followup_no, y = avg_no_vericose$avg_change, type = "o", main = "Change in Volume vs Followups for Patient affected by Vericose in the Past", xlab = "Followup Code", ylab = "Volume Change", col = "red", ylim = c(mini, maxi))
legend("topright", c("Had Vericose", "Did Not Have Vericose"), col = c("blue", "red"), lwd = 3)

#Removing unrequired variables
rm(con, huh, result, i, mini, maxi)