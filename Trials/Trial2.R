#Comparison of Change in Volume for patients who have used compression and those who have not used C-Bandage Stocking
#Constraint to the patient being regular and us having data of continuous followups

library(RMySQL)
library(dplyr)

#Establishing a connection to the local MySQL Server
con <- dbConnect(MySQL(), user = "root", password = "sood", dbname = "Elephantiasis", host = "127.0.0.1")

#Getting the list of all table names
result <- dbSendQuery(con, "SHOW TABLES;")
table_data <- as.data.frame(fetch(result))

#Gathering Data
result <- dbSendQuery(con, "SELECT * FROM limb_data;")
limb_data <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)
result <- dbSendQuery(con, "SELECT * FROM followup_compression_details;")
followup_compression_details <- fetch(result, n = -1)
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


#Sorting Dataset based on patient_limb_id
limb_data_revised <- limb_data_revised[order(limb_data_revised$patient_limb_id), ]
limb_vol_change <- limb_vol_change[order(limb_vol_change$patient_limb_id), ]

#Preprocessing of limb_data_revised
limb_data_revised$metatarsal[is.na(limb_data_revised$metatarsal)] <- median(subset(limb_data_revised$metatarsal, !is.na(limb_data_revised$metatarsal)))
limb_data_revised$mid_foot[is.na(limb_data_revised$mid_foot)] <- median(subset(limb_data_revised$mid_foot, !is.na(limb_data_revised$mid_foot))) 
limb_data_revised$ankle[is.na(limb_data_revised$ankle)] <- median(subset(limb_data_revised$ankle, !is.na(limb_data_revised$ankle)))
limb_data_revised$end_of_calfbulk[is.na(limb_data_revised$end_of_calfbulk)] <- median(subset(limb_data_revised$end_of_calfbulk, !is.na(limb_data_revised$end_of_calfbulk)))
limb_data_revised$maximal_bulk[is.na(limb_data_revised$maximal_bulk)] <- median(subset(limb_data_revised$maximal_bulk, !is.na(limb_data_revised$maximal_bulk)))
limb_data_revised$patelar_region[is.na(limb_data_revised$patelar_region)] <- median(subset(limb_data_revised$patelar_region, !is.na(limb_data_revised$patelar_region)))
limb_data_revised$mid_thigh[is.na(limb_data_revised$mid_thigh)] <- median(subset(limb_data_revised$mid_thigh, !is.na(limb_data_revised$mid_thigh)))
limb_data_revised$max_bulkdtanding[is.na(limb_data_revised$max_bulkdtanding)] <- median(subset(limb_data_revised$max_bulkdtanding, !is.na(limb_data_revised$max_bulkdtanding)))

remove_rows <- which(is.na(limb_data_revised$date_a))
limb_data_revised <- limb_data_revised[-remove_rows, ]

#Preprocessing of followup_compression_details
followup_compression_details$c_bandage_stocking[is.na(followup_compression_details$c_bandage_stocking)] <- 1
remove_rows <- which(is.na(followup_compression_details$date))
followup_compression_details <- followup_compression_details[-remove_rows, ]
comp <- c()
nocomp <- c()
avg_comp <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)
avg_nocomp <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)
ctr_comp <- 0
ctr_nocomp <- 0

#Plotting a graph to compare the change in volume over the course of visits
for(i in 1:4000){
  iter1 <- subset(limb_data_revised, patient_limb_id == i & limb_code == 1)
  iter2 <- subset(limb_data_revised, patient_limb_id == i & limb_code == 2)
  compr <- subset(followup_compression_details, patient_id == i & c_bandage_stocking == 2)
  
  #Finding the list of patients that did C_Bandage_Stocking
  if(nrow(compr) > 0){
    comp <- c(comp, i)
  }
  else{
    nocomp <- c(nocomp, i)
  }
  
  #Preprocessing to ensure that followups are continuous 
  if(nrow(iter1) > 0){
    if(!identical(iter1$followup_code, c(1: max(iter1$followup_code)))){
      next
    }
  }
  if(nrow(iter2) > 0){
    if(!identical(iter2$followup_code, c(1 : max(iter2$followup_code)))){
      next
    }
  }

#Checking if compression was done by the patient in question
  if(nrow(compr) > 0){
    #Avg for Limb 2
    if(nrow(iter2) > 1){
      for(j in iter2$followup_code)
        if(j != 1)
          avg_comp$avg_change[j] <- avg_comp$avg_change[j] + iter2$volume[j-1] - iter2$volume[j]
      ctr_comp <- ctr_comp + 1
    }
    #Avg for Limb 1
    if(nrow(iter1) > 1){
      for(j in iter1$followup_code)
        if(j != 1)
          avg_comp$avg_change[j] <- avg_comp$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
      ctr_comp <- ctr_comp + 1
    }
  }
  else{
    #Avg for Limb 2
    if(nrow(iter2) > 1){
      for(j in iter2$followup_code)
        if(j != 1)
          avg_nocomp$avg_change[j] <- avg_nocomp$avg_change[j] + iter2$volume[j-1] - iter2$volume[j]
      ctr_nocomp <- ctr_nocomp + 1
    }
    #Avg for Limb 1
    if(nrow(iter1) > 1){
      for(j in iter1$followup_code)
        if(j != 1)
          avg_nocomp$avg_change[j] <- avg_nocomp$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
       ctr_nocomp <- ctr_nocomp + 1
    }
  }
}

#Calculating the average change in volume
if(ctr_comp > 0){
  avg_comp$avg_change <- round(avg_comp$avg_change/ctr_comp, 3)
}
if(ctr_nocomp > 0){
  avg_nocomp$avg_change <- round(avg_nocomp$avg_change/ctr_nocomp, 3)
}
maxi <- max(avg_comp$avg_change, avg_nocomp$avg_change)
mini <- min(avg_comp$avg_change, avg_nocomp$avg_change)

#Plotting the average change in volume over meetings when compression is done and when it is not

plot(x = avg_comp$followup_no, y = avg_comp$avg_change, type = "o", main = "Change in Volume vs Followups for Patient ", xlab = "Followup Code", ylab = "Volume Change", col = "blue", ylim = c(mini, maxi))
par(new = TRUE)
plot(x = avg_nocomp$followup_no, y = avg_nocomp$avg_change, type = "o", main = "Change in Volume vs Followups for Patient ", xlab = "Followup Code", ylab = "Volume Change", col = "red", ylim = c(mini, maxi))
legend("topright", c("With C-Bandage Stocking", "Without C-Bandage Stocking"), col = c("blue", "red"), lwd = 3)

#Closing the connection and removing unneccsary variables
dbDisconnect(con)
rm(huh, con, result, iter1, iter2, maxi, mini, i, j, comp, nocomp, remove_rows)