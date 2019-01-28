library("RMySQL")
library("plotly")
library("dplyr")
library("RecordLinkage")

# Connection to MySQL
ElephantiasisAnalysis <- dbConnect(MySQL(), user='root', password='hello', dbname='ElephantiasisAnalysis', host='localhost')

# Table of contacts of patients
patient_contact <- dbGetQuery(ElephantiasisAnalysis, "select * from admission_information_disease;")

# Table of limb data
limb_data <- dbGetQuery(ElephantiasisAnalysis, "select * from limb_data_revised;")

# Table of volume change
limb_volume_change <- dbGetQuery(ElephantiasisAnalysis, "select * from limb_vol_change;")

# List to store correctly spelled cities
actual_cities <- c()

count <- 0
similarity <- 0
correct_city <- ""
k <- 1

patient_swelling <- patient_contact[complete.cases(patient_contact[, "swelling_duration"]), ]

patient_swelling <- patient_swelling[patient_swelling$swelling_duration %in% names(table(patient_swelling$swelling_duration))[table(patient_swelling$swelling_duration) >= 10],]

names(patient_swelling)[1] = "patient_limb_id"
limb_data_revised <- merge(limb_data, patient_swelling, by = "patient_limb_id")
limb_data_revised <- merge(limb_data_revised, limb_volume_change, by = c("patient_limb_id", "followup_code"))
limb_data_revised <- limb_data_revised[, c("patient_limb_id", "affected_nonaffected_limb", "followup_code", "swelling_duration", "vol_change")]

# Average volume change per district
avg <- list()
ctr <- 1
colors <- c("red", "blue", "yellow", "purple", "green", "orange", "black")

num_affected <- data.frame("Swelling Duration" = "None", "Number_Affected" = 0)
num_affected <- num_affected[-c(1), ]

unique_states <- unique(limb_data_revised$swelling_duration)

for(i in c(1:nrow(limb_data_revised))) {
  if(limb_data_revised[i, "swelling_duration"] == ">3 months") {
    limb_data_revised[i, "sd_rating"] <- 1
  }
  else if(limb_data_revised[i, "swelling_duration"] == "3-12 months") {
    limb_data_revised[i, "sd_rating"] <- 2
  }
  else if(limb_data_revised[i, "swelling_duration"] == "1-5 years") {
    limb_data_revised[i, "sd_rating"] <- 3
  }
  else if(limb_data_revised[i, "swelling_duration"] == "5-10 years") {
    limb_data_revised[i, "sd_rating"] <- 4
  }
  else if(limb_data_revised[i, "swelling_duration"] == "above 10 years") {
    limb_data_revised[i, "sd_rating"] <- 5
  }
  else if(limb_data_revised[i, "swelling_duration"] == "30 years") {
    limb_data_revised[i, "sd_rating"] <- 6
  }
}

for(i in unique_states){
  set <- subset(limb_data_revised, swelling_duration == i)
  add_row <- data.frame("Swelling_Duration" = i, "Number_Affected" = length(unique(set$patient_limb_id)))
  num_affected <- rbind(num_affected, add_row)
  
  avg[[i]] <- data.frame("followup_no" = c(1, 2, 3, 4, 5, 6), "avg_change" = 0, "count" = 0)
  
  for(j in 1:nrow(set)){
    avg[[i]]$avg_change[set$followup_code[j]] <- avg[[i]]$avg_change[set$followup_code[j]] + set$vol_change[j]
    avg[[i]]$count[set$followup_code[j]] <- avg[[i]]$count[set$followup_code[j]] + 1
  }
  
  for(j in 1:6){
    avg[[i]]$avg_change[j] <- avg[[i]]$avg_change[j]/avg[[i]]$count[j]
  }
  
  avg[[i]]$avg_change[1] <- 0
  
}

for(i in c(1:4)){
  plot(x = avg[[unique_states[i]]]$followup_no, y = avg[[unique_states[i]]]$avg_change, type = "o", main = "Swelling Duration wise Change in Volume vs Followups", xlab = "Followup Code", ylab = "Volume Change", col = colors[ctr], ylim = c(-1.0, 2.0), lwd = 1)
  par(new = TRUE)
  ctr <- ctr + 1
}

legend("topright", unique_states[1:4], col = colors, lwd = 2, cex = 0.75)

durations <- c("1-5 y", "5-10 y", " >10y", "3-12 m", ">3 m", "30 y")

barplot(num_affected$Number_Affected[1:6], names.arg = durations, main="Distribution of Patients", xlab="Durations of Swelling", ylab="No. of Patients")

# Close the connection with MySQL
list <- dbListConnections(MySQL())
for(con in list) {
  dbDisconnect(con)
}