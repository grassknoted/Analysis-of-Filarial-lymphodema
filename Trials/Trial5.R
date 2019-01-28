#Volume of 1 of Affected_nonaffected_limb in Limb data table and 2 in ulcer variable of dermato_examinations V/S Volume of 1 of Affected_nonaffected_limb in Limb data table and 1 in ulcer variable of dermato_examinations

library(RMySQL)
library(dplyr)

#Establishing a connection to the local MySQL Server
con <- dbConnect(MySQL(), user = "root", password = "sood", dbname = "Elephantiasis", host = "127.0.0.1")

#Gathering Data
result <- dbSendQuery(con, "SELECT * FROM limb_data_revised;")
limb_data_revised <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM dermato_examinations;")
dermato_examinations <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

#Sorting Dataset based on patient_limb_id
limb_data_revised <- limb_data_revised[order(limb_data_revised$patient_limb_id), ]
dermato_examinations <- dermato_examinations[order(dermato_examinations$patient_limb_id), ]

#Preprocessing of dermato_examinations
dermato_examinations$ulcer[is.na(dermato_examinations$ulcer)] <- 3
#limb_data_revised$metatarsal[is.na(limb_data_revised$metatarsal)] <- median(subset(limb_data_revised$metatarsal, !is.na(limb_data_revised$metatarsal)))

#Gathering remaining data required to solve the problem
ctr_ulcer1 <- 0
ctr_ulcer2 <- 0
avg_ulcer1 <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)
avg_ulcer2 <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)

#Plotting a graph to compare change in volume over the course of the followups
for(i in 1:7000){
  iter1 <- subset(limb_data_revised, patient_limb_id == i & affected_nonaffected_limb == 1)
  ulcer1 <- subset(dermato_examinations, patient_limb_id == i & ulcer == 1 & affected_nonaffected_limb == 1)
  ulcer2 <- subset(dermato_examinations, patient_limb_id == i & ulcer == 2 & affected_nonaffected_limb == 1)
  
  #Preprocessing of iter1, ulcer1 and ulcer2 to remove unrequired columns
  iter1 <- iter1[ , c(1, 3, 4, 14)]
  ulcer1 <- ulcer1[ , c(1, 3, 4, 7)]
  ulcer2 <- ulcer2[ , c(1, 3, 4, 7)]
  
  #Preprocessing to ensure followups are continuous
  if(nrow(iter1) > 0){
    if(!identical(iter1$followup_code, c(1: max(iter1$followup_code)))){
      next
    }
  }
  
  #Average for affected_nonaffected_limb = 1 and ulcer = 1
  if(nrow(ulcer1) > 0){
    if(nrow(iter1) > 1){
      ctr_ulcer1 <- ctr_ulcer1 + 1
      for(j in iter1$followup_code){
        if(j != 1 && j %in% ulcer1$followup_code)
          avg_ulcer1$avg_change[j] <- avg_ulcer1$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
      }
    }
  }
  
  #Average for affected_nonaffected_limb = 1 and ulcer = 2
  if(nrow(ulcer2) > 0){
    if(nrow(iter1) > 1){
      ctr_ulcer2 <- ctr_ulcer2 + 1
      for(j in iter1$followup_code){
        if(j != 1 && j %in% ulcer2$followup_code)
          avg_ulcer2$avg_change[j] <- avg_ulcer2$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
      }
    }
  }
}

#Calculating average change for affected_nonaffected_limb = 1 and ulcer = 1
avg_ulcer1$avg_change <- round(avg_ulcer1$avg_change/ctr_ulcer1, 3)

#Calculating average change for affected_nonaffected_limb = 1 and ulcer = 2
avg_ulcer2$avg_change <- round(avg_ulcer2$avg_change/ctr_ulcer2, 3)

#Finding maximum and minimum change
maxi <- round(max(avg_ulcer1$avg_change, avg_ulcer2$avg_change))
mini <- min(avg_ulcer1$avg_change, avg_ulcer2$avg_change)

#Plotting the Graphs
plot(x = avg_ulcer1$followup_no, y = avg_ulcer1$avg_change, type = "o", main = "Change in Volume vs Followups for Patient (Ulcers)", xlab = "Followup Code", ylab = "Volume Change", col = "blue", ylim = c(mini, maxi))
par(new = TRUE)
plot(x = avg_ulcer2$followup_no, y = avg_ulcer2$avg_change, type = "o", main = "Change in Volume vs Followups for Patient (Ulcers)", xlab = "Followup Code", ylab = "Volume Change", col = "red", ylim = c(mini, maxi))
par(new = TRUE)
legend("topright", c("Ulcer 1", "Ulcer 2"), col = c("blue", "red"), lwd = 3)

#Closing the connection and removing unneccsary variables
dbDisconnect(con)

#Removing variables
rm(i, j, maxi, mini, result, con)