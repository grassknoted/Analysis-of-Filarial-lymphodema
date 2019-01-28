library(RMySQL)
library(dplyr)
library(stringr)

#Establishing a connection to the local MySQL Server
con <- dbConnect(MySQL(), user = "root", password = "sood", dbname = "Elephantiasis", host = "127.0.0.1")

#Gathering Data
result <- dbSendQuery(con, "SELECT * FROM admission_pe_history_illness;")
admission_history <- fetch(result, n = -1)
huh <- dbHasCompleted(result)
dbClearResult(result)

#Removing unrequired variables
rm(con, huh, result)