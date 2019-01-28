#Topical ointments from (Medicne_intertrigo in Followup_BEPcare_details) AND Intertrigo_score of Dermato_examinations

library(RMySQL)
library(dplyr)
library(stringr)

#Establishing a connection to the local MySQL Server
con <- dbConnect(MySQL(), user = "root", password = "sood", dbname = "Elephantiasis", host = "127.0.0.1")

#Gathering Data
result <- dbSendQuery(con, "SELECT * FROM dermato_examinations;")
dermato_examinations <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

result <- dbSendQuery(con, "SELECT * FROM followup_bepcare_details;")
followup_bepcare <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

#Creating variables that will be required while cleaning followup_bepcare_details
triben <- 1:nrow(followup_bepcare)
rasnadi <- 1:nrow(followup_bepcare)
loceryl <- 1:nrow(followup_bepcare)
candid <- 1:nrow(followup_bepcare)
tricawin <- 1:nrow(followup_bepcare)
kz <- 1:nrow(followup_bepcare)
na <- 1:nrow(followup_bepcare)
no <- 1:nrow(followup_bepcare)
other <- 1:nrow(followup_bepcare)

#Cleaning table followup_bepcare_details for our use
followup_bepcare <- followup_bepcare[ , c(1, 2, 4, 8, 16)]
followup_bepcare <- cbind(followup_bepcare, triben, rasnadi, loceryl, candid, tricawin, kz, other, no, na)
rm(triben, rasnadi, loceryl, candid, tricawin, kz, other, no, na)
followup_bepcare$medicne_intertrigo[is.na(followup_bepcare$medicne_intertrigo)] <- "na"
followup_bepcare$triben <- 0
followup_bepcare$rasnadi <- 0
followup_bepcare$loceryl <- 0
followup_bepcare$candid <- 0
followup_bepcare$tricawin <- 0
followup_bepcare$kz <- 0
followup_bepcare$na <- 0
followup_bepcare$no <- 0
followup_bepcare$other <- 0

for(i in 1:nrow(followup_bepcare)){
  if(grepl("triben", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$triben[i] <- 1
  if(grepl("rasnadi", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$rasnadi[i] <- 1
  if(grepl("tricawin", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$tricawin[i] <- 1
  if(grepl("loceryl", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$loceryl[i] <- 1
  if(grepl("candid", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$candid[i] <- 1
  if(grepl("kz", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$kz[i] <- 1
  if(grepl("keto", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$kz[i] <- 1
  if(grepl("^na$", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$na[i] <- 1
  if(grepl("not available", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$na[i] <- 1
  if(grepl("^no$", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$no[i] <- 1
  if(grepl("not using", followup_bepcare$medicne_intertrigo[i]))
    followup_bepcare$no[i] <- 1
  if(sum(followup_bepcare[i, c(6:14)]) == 0)
    followup_bepcare$other[i] <- 1
}

#followup_bepcare <- followup_bepcare[ , -c(3, 4, 5)]

#Cleaning dermato_examinations for our use
dermato_examinations$intertrigo_score[is.na(dermato_examinations$intertrigo_score)] <- 0

for(i in 1:nrow(dermato_examinations)){
  if(dermato_examinations$intertrigo_score[i] > 1)
    dermato_examinations$intertrigo_score[i] <- 1
  else
    dermato_examinations$intertrigo_score[i] <- 0
}

dermato_examinations <- dermato_examinations[ , c(1, 2, 3, 4, 14)]

#Varibles needed for finding effectiveness
ctr_triben <- 0
eff_triben <- 0
ctr_rasnadi <- 0
eff_rasnadi <- 0
ctr_loceryl <- 0
eff_loceryl <- 0
ctr_candid <- 0
eff_candid <- 0
ctr_tricawin <- 0
eff_tricawin <- 0
ctr_kz <- 0
eff_kz <- 0
ctr_other <- 0
eff_other <- 0


#Finding effectiveness of each ointment
for(i in 1:nrow(followup_bepcare)){
  iter_de1 <- subset(dermato_examinations, patient_limb_id == i & limb_code == 1)
  iter_de2 <- subset(dermato_examinations, patient_limb_id == i & limb_code == 2)
  iter_bep <- subset(followup_bepcare, patient_id == i)
  
  #Ensuring that Ointment was used
  if(nrow(iter_bep) == 0)
    next
  
  #Preprocessing to ensure that the followups are continuous
  if(nrow(iter_de1) > 0)
    if(!identical(iter_de1$followup_code, c(1: max(iter_de1$followup_code))))
      next
      
  if(nrow(iter_de2) > 0)
    if(!identical(iter_de2$followup_code, c(1: max(iter_de2$followup_code))))
      next
      
  if(nrow(iter_bep) > 0)
    if(!identical(iter_bep$followup_no, c(1: max(iter_bep$followup_no))))
      next
  
  #Finding the times that an ointment was effective
  #Finding effectiveness on Limb 1
  if(nrow(iter_bep) > 0 & nrow(iter_de1) > 0){
    for(i in iter_bep$followup_no){
      #Triben
      if(iter_bep$triben[i] == 1 & iter_de1$intertrigo_score == 1){
        ctr_triben <- ctr_triben + 1
        eff_triben <- eff_triben + 1
      }
      else if(iter_bep$triben[i] == 1 & iter_de1$intertrigo_score != 1)
        ctr_triben <- ctr_triben + 1
      
      #Rasnadi
      if(iter_bep$rasnadi[i] == 1 & iter_de1$intertrigo_score == 1){
        ctr_rasnadi <- ctr_rasnadi + 1
        eff_rasnadi <- eff_rasnadi + 1
      }
      else if(iter_bep$rasnadi[i] == 1 & iter_de1$intertrigo_score != 1)
        ctr_rasnadi <- ctr_rasnadi + 1
      
      #Loceryl
      if(iter_bep$loceryl[i] == 1 & iter_de1$intertrigo_score == 1){
        ctr_loceryl <- ctr_loceryl + 1
        eff_loceryl <- eff_loceryl + 1
      }
      else if(iter_bep$loceryl[i] == 1 & iter_de1$intertrigo_score != 1)
        ctr_loceryl <- ctr_loceryl + 1
      
      #Candid
      if(iter_bep$candid[i] == 1 & iter_de1$intertrigo_score == 1){
        ctr_candid <- ctr_candid + 1
        eff_candid <- eff_candid + 1
      }
      else if(iter_bep$candid[i] == 1 & iter_de1$intertrigo_score != 1)
        ctr_candid <- ctr_candid + 1
      
      #Tricawin
      if(iter_bep$tricawin[i] == 1 & iter_de1$intertrigo_score == 1){
        ctr_tricawin <- ctr_tricawin + 1
        eff_tricawin <- eff_tricawin + 1
      }
      else if(iter_bep$tricawin[i] == 1 & iter_de1$intertrigo_score != 1)
        ctr_tricawin <- ctr_tricawin + 1
      
      #kz
      if(iter_bep$kz[i] == 1 & iter_de1$intertrigo_score == 1){
        ctr_kz <- ctr_kz + 1
        eff_kz <- eff_kz + 1
      }
      else if(iter_bep$kz[i] == 1 & iter_de1$intertrigo_score != 1)
        ctr_kz <- ctr_kz + 1
      
      #Other
      if(iter_bep$other[i] == 1 & iter_de1$intertrigo_score == 1){
        ctr_other <- ctr_other + 1
        eff_other <- eff_other + 1
      }
      else if(iter_bep$other[i] == 1 & iter_de1$intertrigo_score != 1)
        ctr_other <- ctr_other + 1
    }
  }
  
  #Finding Effectiveness on Limb 2
  if(nrow(iter_bep) > 0 & nrow(iter_de2) > 0){
    for(i in iter_bep$followup_no){
      #Triben
      if(iter_bep$triben[i] == 1 & iter_de2$intertrigo_score == 1){
        ctr_triben <- ctr_triben + 1
        eff_triben <- eff_triben + 1
      }
      else if(iter_bep$triben[i] == 1 & iter_de2$intertrigo_score != 1)
        ctr_triben <- ctr_triben + 1
      
      #Rasnadi
      if(iter_bep$rasnadi[i] == 1 & iter_de2$intertrigo_score == 1){
        ctr_rasnadi <- ctr_rasnadi + 1
        eff_rasnadi <- eff_rasnadi + 1
      }
      else if(iter_bep$rasnadi[i] == 1 & iter_de2$intertrigo_score != 1)
        ctr_rasnadi <- ctr_rasnadi + 1
      
      #Loceryl
      if(iter_bep$loceryl[i] == 1 & iter_de2$intertrigo_score == 1){
        ctr_loceryl <- ctr_loceryl + 1
        eff_loceryl <- eff_loceryl + 1
      }
      else if(iter_bep$loceryl[i] == 1 & iter_de2$intertrigo_score != 1)
        ctr_loceryl <- ctr_loceryl + 1
      
      #Candid
      if(iter_bep$candid[i] == 1 & iter_de2$intertrigo_score == 1){
        ctr_candid <- ctr_candid + 1
        eff_candid <- eff_candid + 1
      }
      else if(iter_bep$candid[i] == 1 & iter_de2$intertrigo_score != 1)
        ctr_candid <- ctr_candid + 1
      
      #Tricawin
      if(iter_bep$tricawin[i] == 1 & iter_de2$intertrigo_score == 1){
        ctr_tricawin <- ctr_tricawin + 1
        eff_tricawin <- eff_tricawin + 1
      }
      else if(iter_bep$tricawin[i] == 1 & iter_de2$intertrigo_score != 1)
        ctr_tricawin <- ctr_tricawin + 1
      
      #kz
      if(iter_bep$kz[i] == 1 & iter_de2$intertrigo_score == 1){
        ctr_kz <- ctr_kz + 1
        eff_kz <- eff_kz + 1
      }
      else if(iter_bep$kz[i] == 1 & iter_de2$intertrigo_score != 1)
        ctr_kz <- ctr_kz + 1
      
      #Other
      if(iter_bep$other[i] == 1 & iter_de2$intertrigo_score == 1){
        ctr_other <- ctr_other + 1
        eff_other <- eff_other + 1
      }
      else if(iter_bep$other[i] == 1 & iter_de2$intertrigo_score != 1)
        ctr_other <- ctr_other + 1
    }
  }
}

#Creating dataframe for results
results <- data.frame("Ointment Type" = c("Triben", "Rasnadi", "Loceryl", "Candid", "Tricawin", "Keto Based", "Others"), "Times Used" = c(ctr_triben, ctr_rasnadi, ctr_loceryl, ctr_candid, ctr_tricawin, ctr_kz, ctr_other), "Times Effective" = c(eff_triben, eff_rasnadi, eff_loceryl, eff_candid, eff_tricawin, eff_kz, eff_other))

#Calculating the Effectiveness
#Triben
eff_triben <- 100 * eff_triben/ctr_triben

#Rasnadi
eff_rasnadi <- 100 * eff_rasnadi/ctr_rasnadi

#Loceryl
eff_loceryl <- 100 * eff_loceryl/ctr_loceryl

#Candid
eff_candid <- 100 * eff_candid/ctr_candid

#Tricawin
eff_tricawin<- 100 * eff_tricawin/ctr_tricawin

#kz
eff_kz <- 100 * eff_kz/ctr_kz

#Other
eff_other <- 100 * eff_other/ctr_other

#Putting all the results in one Dataframe
results <- cbind(results, "Effectiveness (%)" = c(eff_triben, eff_rasnadi, eff_loceryl, eff_candid, eff_tricawin, eff_kz, eff_other))

#Disconnecting the server
dbDisconnect(con)

#Removing unrequired variables
rm(con, huh, result, i)