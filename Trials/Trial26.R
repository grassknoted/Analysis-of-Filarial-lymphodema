#Question 40 Gender

library(RMySQL)
library(dplyr)
library(MASS)
#library(psych)
library(nlme)
library(car)
library(ggplot2)

#Establishing a connection to the local MySQL Server
con <- dbConnect(MySQL(), user = "root", password = "hello", dbname = "ElephantiasisAnalysis", host = "127.0.0.1")

#Gathering Data
result <- dbSendQuery(con, "SELECT * FROM contact;")
contact <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM dermato_examinations;")
dermato_examinations <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM limb_data_revised;")
limb_data_revised <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM limb_vol_change;")
limb_vol_change <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

dbDisconnect(con)

names(dermato_examinations)[1] <- "patient_id"
names(dermato_examinations)[4] <- "followup_no"
names(limb_data_revised)[1] <- "patient_id"
names(limb_data_revised)[4] <- "followup_no"
names(limb_vol_change)[1] <- "patient_id"
names(limb_vol_change)[3] <- "followup_no"
names(contact)[1] <- "patient_id"

#Streamlining the contact table to only the required variables
contact <- contact[, c("patient_id", "gender")]
contact <- contact[complete.cases(contact[, 1:2]), ]
limb_vol_change <- limb_vol_change[, c("patient_id", "followup_no", "vol_change")]

contact$gender[contact$gender == 1] <- "Male"
contact$gender[contact$gender == 2] <- "Female"

#Preprocessing and merging of remaining tables
limb_data_revised <- merge(limb_data_revised, contact, by = "patient_id")
limb_data_revised <- merge(limb_data_revised, limb_vol_change, by = c("patient_id", "followup_no"))
limb_data_revised <- limb_data_revised[, c("patient_id", "affected_nonaffected_limb", "followup_no", "gender", "vol_change")]

#Creating a list of dataframes to store average values for each age group
avg <- list()
num_affected <- data.frame("Gender" = "None", "Number_Affected" = 0)
num_affected <- num_affected[-c(1), ]
colors <- c("red", "blue", "yellow", "purple", "green", "orange", "black")
genders <- c("Male", "Female")
mini <- 1000
maxi <- -1
ctr <- 1

for(i in genders){
  set <- subset(limb_data_revised, gender == i)
  add_row <- data.frame("State" = i, "Number_Affected" = length(unique(set$patient_id)))
  num_affected <- rbind(num_affected, add_row)
  
  avg[[i]] <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6), "avg_change" = 0, "count" = 0)
  
  for(j in 1:nrow(set)){
    avg[[i]]$avg_change[set$followup_no[j]] <- avg[[i]]$avg_change[set$followup_no[j]] + set$vol_change[j]
    avg[[i]]$count[set$followup_no[j]] <- avg[[i]]$count[set$followup_no[j]] + 1
  }
  
  for(j in 1:6){
    avg[[i]]$avg_change[j] <- avg[[i]]$avg_change[j]/avg[[i]]$count[j]
  }
  
  avg[[i]]$avg_change[1] <- 0
  
  maxi <- max(avg[[i]]$avg_change, maxi)
  mini <- min(avg[[i]]$avg_change, mini)
}

barplot(num_affected$Number_Affected, main = "Gender Wise Comparison of Number of People Affected", xlab = "Gender", ylab = "Number of People Affected", names.arg = genders, ylim = c(0, 1400))

for(i in genders){
  plot(x = avg[[i]]$followup_no, y = avg[[i]]$avg_change, type = "o", main = "Gender Wise Change in Volume vs Followups", xlab = "Followup Code", ylab = "Volume Change", col = colors[ctr], ylim = c(mini, maxi), lwd = 2)
  par(new = TRUE)
  ctr <- ctr + 1
}

legend("topright", genders, col = colors, lwd = 3)

#Removing unrequired variabls
rm(con, result, huh, i, j, maxi, mini, colors, ctr, genders)