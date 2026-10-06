# Load function
source("calculate_rw_speed.R")

# Read data
Data <- read.csv("example_data.csv")

# Calculate real-world walking speed and other parameters
outcome <- calculate_rw_speed(Data$Gaitspeed)


