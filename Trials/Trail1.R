#Comparison of Change in Volume based on each followup of the patient
#Added constraint is that the patient has come for at least two followups

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


#Plotting a graph to compare the change in volume over the course of visits
for(i in 1:20){
  iter1 <- subset(limb_data_revised, patient_limb_id == i & limb_code == 1)
  iter2 <- subset(limb_data_revised, patient_limb_id == i & limb_code == 2)
  
  #Setting the limit for the graph that is to be plotted
  mini <- min(min(iter1$volume), min(iter2$volume))
  maxi <- max(max(iter1$volume), max(iter2$volume))
  
  #Plotting the graph
  plot.new()
  if(nrow(iter1) > 0){
    if(!identical(iter1$followup_code, c(1: max(iter1$followup_code)))){
      next
    }
    plot(x = iter1$followup_code, y = iter1$volume, type = "l", main = paste("Change in Volume vs Followups for Patient ", i), xlab = "Followup Code", ylab = "Volume", col = "blue", ylim = c(mini, maxi))
  }
  par(new = TRUE)
  if(nrow(iter2) > 0){
    if(!identical(iter2$followup_code, c(1 : max(iter2$followup_code)))){
      next
    }
    plot(x = iter2$followup_code, y = iter2$volume, main = paste("Change in Volume vs Followups for Patient ", i), type = "l", xlab = "Followup Code", ylab = "Volume", col = "red", ylim = c(mini, maxi))
  }
  legend("topleft", c("Limb 1", "Limb 2"), col = c("blue", "red"), lwd = 3)
}

#Closing the connection and removing unneccsary variables
dbDisconnect(con)
rm(huh, con, result, iter1, iter2, add, maxi, mini, i, j, comp, nocomp, remove_rows)