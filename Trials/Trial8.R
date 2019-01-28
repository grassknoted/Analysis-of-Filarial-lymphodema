#Comparison of Change in Volume for patients who have used wrapping cloth and those who have not used wrapping cloth

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
followup_compression_details$wrapping_cloth[is.na(followup_compression_details$wrapping_cloth)] <- 3
remove_rows <- which(is.na(followup_compression_details$date))
followup_compression_details <- followup_compression_details[-remove_rows, ]

limb1_wrap1 <- 0
limb2_wrap1 <- 0
limb1_wrap2 <- 0
limb2_wrap2 <- 0
limb1_wrap3 <- 0
limb2_wrap3 <- 0
avg_wrap1 <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)
avg_wrap2 <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)
avg_wrap3 <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6, 7), "avg_change" = 0)

#Plotting a graph to compare the change in volume over the course of visits
for(i in 1:7000){
  iter1 <- subset(limb_data_revised, patient_limb_id == i & limb_code == 1)
  iter2 <- subset(limb_data_revised, patient_limb_id == i & limb_code == 2)
 
  #To check for wrapping cloth
  wrap1 <- subset(followup_compression_details, patient_id == i & wrapping_cloth == 1)
  wrap2 <- subset(followup_compression_details, patient_id == i & wrapping_cloth == 2)
  wrap3 <- subset(followup_compression_details, patient_id == i & wrapping_cloth == 3)
  
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
  if(nrow(wrap1) > 0){
    wrap1 <- c(wrap1, i)
    
    #Average for Limb 1
    if(nrow(iter1) > 1){
      limb1_wrap1 <- limb1_wrap1 + 1
      for(j in iter1$followup_code)
        if(j != 1)
          avg_wrap1$avg_change[j] <- avg_wrap1$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
    }
    
    #Average for Limb 2
    if(nrow(iter2) > 1){
      limb2_wrap1 <- limb2_wrap1 + 1
      for(j in iter2$followup_code)
        if(j != 1)
          avg_wrap1$avg_change[j] <- avg_wrap1$avg_change[j] + iter2$volume[j-1] - iter2$volume[j]
    }
  }
  
  if(nrow(wrap2) > 0){
    wrap2 <- c(wrap2, i)
    
    #Average for Limb 1
    if(nrow(iter1) > 1){
      limb1_wrap2 <- limb1_wrap2 + 1
      for(j in iter1$followup_code)
        if(j != 1)
          avg_wrap2$avg_change[j] <- avg_wrap2$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
    }
    
    #Average for Limb 2
    if(nrow(iter2) > 1){
      limb2_wrap2 <- limb2_wrap2 + 1
      for(j in iter2$followup_code)
        if(j != 1)
          avg_wrap2$avg_change[j] <- avg_wrap2$avg_change[j] + iter2$volume[j-1] - iter2$volume[j]
    }
  }
  
  if(nrow(wrap3) > 0){
    wrap3 <- c(wrap3, i)
    
    #Average for Limb 1
    if(nrow(iter1) > 1){
      limb1_wrap3 <- limb1_wrap3 + 1
      for(j in iter1$followup_code)
        if(j != 1)
          avg_wrap3$avg_change[j] <- avg_wrap3$avg_change[j] + iter1$volume[j-1] - iter1$volume[j]
    }
    
    #Average for Limb 2
    if(nrow(iter2) > 1){
      limb2_wrap3 <- limb2_wrap3 + 1
      for(j in iter2$followup_code)
        if(j != 1)
          avg_wrap3$avg_change[j] <- avg_wrap3$avg_change[j] + iter2$volume[j-1] - iter2$volume[j]
    }
  }
}

#Calculating Weightage
total_uses <- limb1_wrap1 + limb2_wrap1 + limb1_wrap2 + limb2_wrap2 + limb1_wrap3 + limb2_wrap3
type1 = (limb1_wrap1 + limb2_wrap1) / total_uses
type2 = (limb1_wrap2 + limb2_wrap2) / total_uses
type3 = (limb1_wrap3 + limb2_wrap3) / total_uses


#Calculating the average change in volume for wrapping cloth Type 1
if(limb1_wrap1 + limb2_wrap1 > 0){
  avg_wrap1$avg_change <- round(avg_wrap1$avg_change/(limb1_wrap1 + limb2_wrap1)*type1, 3) 
}

#Calculating the average change in volume for wrapping cloth Type 2
if(limb1_wrap2 + limb2_wrap2 > 0){
  avg_wrap2$avg_change <- round(avg_wrap2$avg_change/(limb1_wrap2 + limb2_wrap2)*type2, 3)
}

#Calculating the average change in volume for wrapping cloth Type 3
if(limb1_wrap3 + limb2_wrap3 > 0){
  avg_wrap3$avg_change <- round(avg_wrap3$avg_change/(limb1_wrap3 + limb2_wrap3)*type3, 3)
}

#Finding Minimum change and Maximum Change
maxi <- max(avg_wrap1$avg_change, avg_wrap2$avg_change, avg_wrap3$avg_change)
mini <- min(avg_wrap1$avg_change, avg_wrap2$avg_change, avg_wrap3$avg_change)

#Plotting the average change in volume over meetings when compression is done and when it is not

plot(x = avg_wrap1$followup_no, y = avg_wrap1$avg_change, type = "o", main = "Change in Volume vs Followups for Patient ", xlab = "Followup Code", ylab = "Volume Change", col = "blue", ylim = c(mini, maxi))
par(new = TRUE)
plot(x = avg_wrap2$followup_no, y = avg_wrap2$avg_change, type = "o", main = "Change in Volume vs Followups for Patient ", xlab = "Followup Code", ylab = "Volume Change", col = "red", ylim = c(mini, maxi))
par(new = TRUE)
plot(x = avg_wrap3$followup_no, y = avg_wrap3$avg_change, type = "o", main = "Change in Volume vs Followups for Patient ", xlab = "Followup Code", ylab = "Volume Change", col = "black", ylim = c(mini, maxi))
legend("topright", c("Wrapping Cloth Type 1", "Wrapping Cloth Type 2", "Wrapping Cloth Type 3"), col = c("blue", "red", "black"), lwd = 3)

#Closing the connection and removing unneccsary variables
dbDisconnect(con)
rm(huh, con, result, iter1, iter2, maxi, mini, i, j, comp1, comp2, comp3, remove_rows)