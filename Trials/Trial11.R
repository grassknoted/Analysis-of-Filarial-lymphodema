#Comparison of Change in Volume for patients who have used spong_mould_fold and those who have not used spong_mould_fold

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
followup_compression_details$spong_mould_folds[is.na(followup_compression_details$spong_mould_folds)] <- 3
remove_rows <- which(is.na(followup_compression_details$date))
followup_compression_details <- followup_compression_details[-remove_rows, ]

limb1_spong1 <- 0
limb2_spong1 <- 0
limb1_spong2 <- 0
limb2_spong2 <- 0
limb1_spong3 <- 0
limb2_spong3 <- 0
avg_spong1 <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)
avg_spong2 <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)
avg_spong3 <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)

#Plotting a graph to compare the change in volume over the course of visits
for(i in 1:7000){
  iter1 <- subset(limb_data_revised, patient_limb_id == i & limb_code == 1)
  iter2 <- subset(limb_data_revised, patient_limb_id == i & limb_code == 2)
  
  #To check for wrapping cloth
  spong1 <- subset(followup_compression_details, patient_id == i & spong_mould_folds == 1)
  spong2 <- subset(followup_compression_details, patient_id == i & spong_mould_folds == 2)
  spong3 <- subset(followup_compression_details, patient_id == i & spong_mould_folds == 3)
  
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
  
  #Gathering patient IDs based on type of compression used
  if(nrow(spong1) > 0){
    spong1 <- c(spong1, i)
    
    #Average for Limb 1
    if(nrow(iter1) > 1){
      limb1_spong1 <- limb1_spong1 + 1
      for(j in iter1$followup_code)
        if(j != 1)
          avg_spong1$avg_change[j] <- avg_spong1$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
    }
    
    #Average for Limb 2
    if(nrow(iter2) > 1){
      limb2_spong1 <- limb2_spong1 + 1
      for(j in iter2$followup_code)
        if(j != 1)
          avg_spong1$avg_change[j] <- avg_spong1$avg_change[j] + iter2$volume[j-1] - iter2$volume[j]
    }
  }
  
  if(nrow(spong2) > 0){
    spong2 <- c(spong2, i)
    
    #Average for Limb 1
    if(nrow(iter1) > 1){
      limb1_spong2 <- limb1_spong2 + 1
      for(j in iter1$followup_code)
        if(j != 1)
          avg_spong2$avg_change[j] <- avg_spong2$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
    }
    
    #Average for Limb 2
    if(nrow(iter2) > 1){
      limb2_spong2 <- limb2_spong2 + 1
      for(j in iter2$followup_code)
        if(j != 1)
          avg_spong2$avg_change[j] <- avg_spong2$avg_change[j] + iter2$volume[j-1] - iter2$volume[j]
    }
  }
  
  if(nrow(spong3) > 0){
    spong3 <- c(spong3, i)
    
    #Average for Limb 1
    if(nrow(iter1) > 1){
      limb1_spong3 <- limb1_spong3 + 1
      for(j in iter1$followup_code)
        if(j != 1)
          avg_spong3$avg_change[j] <- avg_spong3$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
    }
    
    #Average for Limb 2
    if(nrow(iter2) > 1){
      limb2_spong3 <- limb2_spong3 + 1
      for(j in iter2$followup_code)
        if(j != 1)
          avg_spong3$avg_change[j] <- avg_spong3$avg_change[j] + iter2$volume[j-1] - iter2$volume[j]
    }
  }
}

#Calculating Weightage
total_uses <- limb1_spong1 + limb2_spong1 + limb1_spong2 + limb2_spong2 + limb1_spong3 + limb2_spong3
type1 = (limb1_spong1 + limb2_spong1) / total_uses
type2 = (limb1_spong2 + limb2_spong2) / total_uses
type3 = (limb1_spong3 + limb2_spong3) / total_uses


#Calculating the average change in volume for Sponge Mould Fold Type 1
if(limb1_spong1 + limb2_spong1 > 0){
  avg_spong1$avg_change <- round(avg_spong1$avg_change/(limb1_spong1 + limb2_spong1)*type1, 3) 
}

#Calculating the average change in volume for Sponge Mould Fold Type 2
if(limb1_spong2 + limb2_spong2 > 0){
  avg_spong2$avg_change <- round(avg_spong2$avg_change/(limb1_spong2 + limb2_spong2)*type2, 3)
}

#Calculating the average change in volume for Sponge Mould Fold Type 3
if(limb1_spong3 + limb2_spong3 > 0){
  avg_spong3$avg_change <- round(avg_spong3$avg_change/(limb1_spong3 + limb2_spong3)*type3, 3)
}

#Finding Minimum change and Maximum Change
maxi <- max(avg_spong1$avg_change, avg_spong2$avg_change, avg_spong3$avg_change)
mini <- min(avg_spong1$avg_change, avg_spong2$avg_change, avg_spong3$avg_change)

#Plotting the average change in volume over meetings when compression is done and when it is not (normailzed)

plot(x = avg_spong1$followup_no, y = avg_spong1$avg_change, type = "o", main = "Change in Volume vs Followups for Patient (Normalized)", xlab = "Followup Code", ylab = "Volume Change", col = "blue", ylim = c(mini, maxi))
par(new = TRUE)
plot(x = avg_spong2$followup_no, y = avg_spong2$avg_change, type = "o", main = "Change in Volume vs Followups for Patient (Normalized)", xlab = "Followup Code", ylab = "Volume Change", col = "red", ylim = c(mini, maxi))
par(new = TRUE)
plot(x = avg_spong3$followup_no, y = avg_spong3$avg_change, type = "o", main = "Change in Volume vs Followups for Patient (Normalized)", xlab = "Followup Code", ylab = "Volume Change", col = "black", ylim = c(mini, maxi))
legend("topright", c("Sponge Mould Fold Type 1", "Sponge Mould Fold Type 2", "Sponge Mould Fold Type 3"), col = c("blue", "red", "black"), lwd = 3)

#Closing the connection and removing unneccsary variables
dbDisconnect(con)
rm(huh, con, result, iter1, iter2, maxi, mini, i, j, remove_rows)