#Comparison of Change in Volume for patients who have used spong_mould_fold and those who have not used spong_mould_fold

library(RMySQL)
library(dplyr)
library(arules)
library(stringr)

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

dbDisconnect(con)

#Renaming column to allow for joining
names(limb_vol_change)[1] <- "patient_id"
names(limb_vol_change)[3] <- "followup_no"

#Inner joining followup_compression_details with limb_data_revised on patient_id, followup_no
merged_db <- merge(followup_compression_details, limb_vol_change, by = c("patient_id", "followup_no"))
merged_db <- merged_db[order(merged_db$patient_id), ]

#Keeping only the required columns
merged_db <- merged_db[ , c("patient_id", "followup_no", "limb_code", "compression_practice", "c_bandage_stocking", "toe_compression", "wrapping_cloth", "sterilization", "changing_cleaning", "spong_mould_folds", "using__mould", "using_compression", "vol_change")]

#Preprocessing done, filling NA-value cells with 3
merged_db$compression_practice[is.na(merged_db$compression_practice)] <- 3
merged_db$c_bandage_stocking[is.na(merged_db$c_bandage_stocking)] <- 3
merged_db$toe_compression[is.na(merged_db$toe_compression)] <- 3
merged_db$wrapping_cloth[is.na(merged_db$wrapping_cloth)] <- 3
merged_db$sterilization[is.na(merged_db$sterilization)] <- 3
merged_db$changing_cleaning[is.na(merged_db$changing_cleaning)] <- 3
merged_db$spong_mould_folds[is.na(merged_db$spong_mould_folds)] <- 3
merged_db$using__mould[is.na(merged_db$using__mould)] <- 3

#Preparing the data for use by the apriori algorithm
#Changing Compression Column
merged_db$compression_practice <- as.character(merged_db$compression_practice)
merged_db$compression_practice[merged_db$compression_practice == "1"] <- "Used_Compression"
merged_db$compression_practice[merged_db$compression_practice == "2"] <- "Did_Not_Use_Compression"
merged_db$compression_practice[merged_db$compression_practice == "3"] <- "Not_Available"

#Changing C-Bandage Stocking Column
merged_db$c_bandage_stocking <- as.character(merged_db$c_bandage_stocking)
merged_db$c_bandage_stocking[merged_db$c_bandage_stocking == "1"] <- "Used_C_Bandage_Stocking"
merged_db$c_bandage_stocking[merged_db$c_bandage_stocking == "2"] <- "Did_Not_Use_C_Bandage_Stocking"
merged_db$c_bandage_stocking[merged_db$c_bandage_stocking == "3"] <- "Not_Available"

#Changing Toe Compression Column
merged_db$toe_compression <- as.character(merged_db$toe_compression)
merged_db$toe_compression[merged_db$toe_compression == "1"] <- "Used_Toe_Compression"
merged_db$toe_compression[merged_db$toe_compression == "2"] <- "Did_Not_Use_Toe_Compression"
merged_db$toe_compression[merged_db$toe_compression == "3"] <- "Not_Available"

#Changing Wrapping Cloth Column
merged_db$wrapping_cloth <- as.character(merged_db$wrapping_cloth)
merged_db$wrapping_cloth[merged_db$wrapping_cloth == "1"] <- "Used_Wrapping_Cloth"
merged_db$wrapping_cloth[merged_db$wrapping_cloth == "2"] <- "Did_Not_Use_Wrapping_Cloth"
merged_db$wrapping_cloth[merged_db$wrapping_cloth == "3"] <- "Not Available"

#Changing Sterilization Column
merged_db$sterilization <- as.character(merged_db$sterilization)
merged_db$sterilization[merged_db$sterilization == "1"] <- "Used_Sterilization"
merged_db$sterilization[merged_db$sterilization == "2"] <- "Did_Not_Use_Sterilization"
merged_db$sterilization[merged_db$sterilization == "3"] <- "Not_Available"

#ChangingChanging Changing Cleaning Column
merged_db$changing_cleaning <- as.character(merged_db$changing_cleaning)
merged_db$changing_cleaning[merged_db$changing_cleaning == "1"] <- "Changed_Cleaning"
merged_db$changing_cleaning[merged_db$changing_cleaning == "2"] <- "Did_Not_Change_Cleaning"
merged_db$changing_cleaning[merged_db$changing_cleaning == "3"] <- "Not_Available"

#Changing Sponge Mould Folds Column
merged_db$spong_mould_folds <- as.character(merged_db$spong_mould_folds)
merged_db$spong_mould_folds[merged_db$spong_mould_folds == "1"] <- "Used_Sponge_Mould_Folds"
merged_db$spong_mould_folds[merged_db$spong_mould_folds == "2"] <- "Did_Not_Use_Sponge_Mould_Folds"
merged_db$spong_mould_folds[merged_db$spong_mould_folds == "3"] <- "Not_Available"

#Changing Using Mould Column
merged_db$using__mould <- as.character(merged_db$using__mould)
merged_db$using__mould[merged_db$using__mould == "1"] <- "Used_Mould"
merged_db$using__mould[merged_db$using__mould == "2"] <- "Did_Not_Use_Mould"
merged_db$using__mould[merged_db$using__mould == "3"] <- "Not_Available"

#Changing Vol Change column
#merged_db <- merged_db[complete.cases(merged_db[, -1]), ]
#merged_db$vol_change <- as.numeric(merged_db$vol_change)

merged_db$vol_change[merged_db$vol_change >= 1.3] <- "Excellent_Change"
merged_db$vol_change[merged_db$vol_change < 1.3 & merged_db$vol_change >= 0.89] <- "Good_Change"
merged_db$vol_change[merged_db$vol_change < 0.89 & merged_db$vol_change >= 0.5] <- "Decent_Change"
merged_db$vol_change[merged_db$vol_change < 0.5 & merged_db$vol_change >= 0] <- "Poor_Change"
#merged_db$vol_change[merged_db$vol_change < 0] <- "Made Worse"

#Making all required columns Factors
merged_db <- merged_db[c("compression_practice", "c_bandage_stocking", "toe_compression", "wrapping_cloth", "sterilization", "changing_cleaning", "spong_mould_folds", "using__mould", "vol_change")]
merged_db <- as.data.frame(unclass(merged_db))

#Converting the Data Frame to a series of Transactions
transactions <- as(merged_db, "transactions")

#Running the apriori algorithm
rules <- apriori(transactions, parameter = list(sup = 0.2, conf= 0.9, target = "rules"))
#rules1 <- apriori(transactions)

#Calculating Association Rules

#
rules1 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("vol_change=Excellent_Change")))

rules2 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(lhs=c("vol_change=Excellent_Change")))

rules3 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(lhs=c("vol_change=Good_Change"), rhs=(c("toe_compression=Used_Toe_Compression", "compression_practice=Used_Compression"))))

rules4 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("vol_change=Good_Change", "vol_change=Decent_Change")))

rules5 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(lhs=c("toe_compression=Used_Toe_Compression"), rhs= c("compression_practice=Used_Compression")))

rules6 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("toe_compression=Used_Toe_Compression"), lhs = c("compression_practice=Used_Compression")))

rules7 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(lhs=c("wrapping_cloth=Used_Wrapping_Cloth"), rhs= c("compression_practice=Used_Compression")))

rules8 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("toe_compression=Used_Toe_Compression"), lhs= c("vol_change=Poor_Change")))

rules9 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("toe_compression=Used_Toe_Compression"), lhs= c("vol_change=Decent_Change")))

rules10 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("toe_compression=Used_Toe_Compression"), lhs= c("vol_change=Excellent_Change")))

rules11 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("toe_compression=Used_Toe_Compression"), lhs= c("vol_change=Excellent_Change")))

rules12 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("toe_compression=Used_Toe_Compression", "wrapping_cloth=Used_Wrapping_Cloth"), lhs= c("vol_change=Excellent_Change")))

rules13 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("toe_compression=Used_Toe_Compression", "sterilization=Used_Sterilization"), lhs= c("vol_change=Excellent_Change")))

rules14 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("sterilization=Used_Sterilization"), lhs= c("vol_change=Excellent_Change")))

rules15 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("spong_mould_folds=Used_Sponge_Mould_Folds"), lhs= c("vol_change=Excellent_Change")))

rules16 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(rhs=c("spong_mould_folds=Used_Sponge_Mould_Folds"), lhs= c("vol_change=Decent_Change")))

rules17 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(lhs=c("vol_change=Decent_Change")))

rules18 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(lhs=c("vol_change=Good_Change")))

rules19 <- apriori(transactions, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2), appearance = list(lhs=c("vol_change=Poor_Change"), rhs = c("c_bandage_stocking=Not_Available", "wrapping_cloth=Used_Wrapping_Cloth")))

#Converting "rules" to a dataframe
dataframeRules <- as(rules, "data.frame")
#df <- as(rules1, "data.frame")

#Closing the connection and removing unneccsary variables
rm(huh, con, result)