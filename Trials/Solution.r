library("RMySQL")
library("plotly")
library("dplyr")

# Connection to MySQL
ElephantiasisAnalysis <- dbConnect(MySQL(), user='root', password='hello', dbname='ElephantiasisAnalysis', host='localhost')

# Connect to table followup_bepcare_details
followup_bepcare_details <- dbReadTable(ElephantiasisAnalysis, 'followup_bepcare_details')

# Connect to table dermato_examinations
dermato_examinations <- dbReadTable(ElephantiasisAnalysis, 'dermato_examinations')

# Connect to table Baseline treatments
baseline_treatments <- dbReadTable(ElephantiasisAnalysis, 'baseline_treatments')

# Connect to table limb data
limb_data <- dbReadTable(ElephantiasisAnalysis, 'limb_data')

# Connect to table of revised limb data
limb_data_revised <- dbReadTable(ElephantiasisAnalysis, 'limb_data_revised')

# Intertigo medicide
# Intertigo Score:
# 0, 1= no
# 2, 3, 4, 5, 6= yes
all_possibilities <- unique(followup_bepcare_details$frequency)
# prope
a <- [1, 1, 15, 30, 0, 0, 7, 1, 3, 15, 0, 1, 0, 7, 10, 10, 0, 1, 0, 7, 7, 1, 0, 30, 15, 0, 7, 20, 15, 7, 15, 0, 0, 30, 0, 0, 30, 10, 45, 15, 0, 0, 0, 0, 0, 0, 1, 0, 15, ]


# Pattern matching 
grep(ointment, ointment_list, ignore.case = TRUE )