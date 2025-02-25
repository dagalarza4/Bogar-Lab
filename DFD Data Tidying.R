library(readxl)
library(dplyr)
library(tidyverse)

#### Open datasets #### 
Harvest_Data <- read_excel("Harvest Data.xlsx")

treatments = read_csv("Treatment_key.csv") %>%
  mutate(ID = Plant_ID)

#### Harvest_Data Manipulation ####

# Add Total Dry Biomass to Harvest_Data
Harvest_Data_LB = Harvest_Data %>% mutate(total_dry_mass = Shoot_DW + Root_DW)

# Change Percent Colonization to 100% max
Harvest_Data_LB$perccol = (Harvest_Data_LB$`%_colonization` / 100) / ((Harvest_Data_LB$`%_colonization` / 100) + 1) * 100

save(Harvest_Data_LB, file = "Harvest_Data_LB.RData")


#### Calculating transpiration rates ####
library(readxl)

# Read data
Lysimetry_Updated <- read_excel("Lysimetry_Updated.xlsx")
Lysimetry_Super_Update <- Lysimetry_Updated

# Convert the date/time columns to POSIXct format
Lysimetry_Super_Update$Time06_19_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/19/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_21_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/21/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_22_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/22/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_23_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/23/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_26_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/26/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_27_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/27/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_28_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/28/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_29_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/29/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_03_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/03/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_05_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/05/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_06_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/06/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_07_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/07/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_10_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/10/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_11_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/11/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_12_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/12/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_13_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/13/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_17_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/17/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_18_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/18/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_19_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/19/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_20_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/20/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_24_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/24/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_25_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/25/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_26_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/26/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_27_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/27/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_31_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/31/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_01_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/01/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_02_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/02/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_03_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/03/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_07_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/07/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_08_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/08/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_09_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/09/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_10_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/10/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_14_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/14/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_15_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/15/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_16_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/16/23`, format="%m/%d/%y %H:%M")

# List of time columns in order
time_columns <- c("Time06_19_23", "Time06_21_23", "Time06_22_23", "Time06_23_23", "Time06_26_23", 
                  "Time06_27_23", "Time06_28_23", "Time06_29_23", "Time07_03_23", "Time07_05_23", 
                  "Time07_06_23", "Time07_07_23", "Time07_10_23", "Time07_11_23", "Time07_12_23", 
                  "Time07_13_23", "Time07_17_23", "Time07_18_23", "Time07_19_23", "Time07_20_23", 
                  "Time07_24_23", "Time07_25_23", "Time07_26_23", "Time07_27_23", "Time07_31_23", 
                  "Time08_01_23", "Time08_02_23", "Time08_03_23", "Time08_07_23", "Time08_08_23", 
                  "Time08_09_23", "Time08_10_23", "Time08_14_23", "Time08_15_23", "Time08_16_23")

# List of date columns in the format "TimeMM/DD/YY"
time_columns <- grep("Time", colnames(Lysimetry_Super_Update), value = TRUE)

# Create columns for time difference
Lysimetry_Super_Update$Time_diff_06_19_06_21 <- 
  Lysimetry_Super_Update$Time06_21_23 - Lysimetry_Super_Update$Time06_19_23

Lysimetry_Super_Update$Time_diff_06_21_06_22 <- 
  Lysimetry_Super_Update$Time06_22_23 - Lysimetry_Super_Update$Time06_21_23

Lysimetry_Super_Update$Time_diff_06_22_06_23 <- 
  Lysimetry_Super_Update$Time06_23_23 - Lysimetry_Super_Update$Time06_22_23

Lysimetry_Super_Update$Time_diff_06_23_06_26 <- 
  Lysimetry_Super_Update$Time06_26_23 - Lysimetry_Super_Update$Time06_23_23

Lysimetry_Super_Update$Time_diff_06_26_06_27 <- 
  Lysimetry_Super_Update$Time06_27_23 - Lysimetry_Super_Update$Time06_26_23

Lysimetry_Super_Update$Time_diff_06_27_06_28 <- 
  Lysimetry_Super_Update$Time06_28_23 - Lysimetry_Super_Update$Time06_27_23

Lysimetry_Super_Update$Time_diff_06_28_06_29 <- 
  Lysimetry_Super_Update$Time06_29_23 - Lysimetry_Super_Update$Time06_28_23

Lysimetry_Super_Update$Time_diff_06_29_07_03 <- 
  Lysimetry_Super_Update$Time07_03_23 - Lysimetry_Super_Update$Time06_29_23

Lysimetry_Super_Update$Time_diff_07_03_07_05 <- 
  Lysimetry_Super_Update$Time07_05_23 - Lysimetry_Super_Update$Time07_03_23

Lysimetry_Super_Update$Time_diff_07_05_07_06 <- 
  Lysimetry_Super_Update$Time07_06_23 - Lysimetry_Super_Update$Time07_05_23

Lysimetry_Super_Update$Time_diff_07_06_07_07 <- 
  Lysimetry_Super_Update$Time07_07_23 - Lysimetry_Super_Update$Time07_06_23

Lysimetry_Super_Update$Time_diff_07_07_07_10 <- 
  Lysimetry_Super_Update$Time07_10_23 - Lysimetry_Super_Update$Time07_07_23

Lysimetry_Super_Update$Time_diff_07_10_07_11 <- 
  Lysimetry_Super_Update$Time07_11_23 - Lysimetry_Super_Update$Time07_10_23

Lysimetry_Super_Update$Time_diff_07_10_07_12 <- 
  Lysimetry_Super_Update$Time07_12_23 - Lysimetry_Super_Update$Time07_10_23

Lysimetry_Super_Update$Time_diff_07_11_07_12 <- 
  Lysimetry_Super_Update$Time07_12_23 - Lysimetry_Super_Update$Time07_11_23

Lysimetry_Super_Update$Time_diff_07_11_07_13 <- 
  Lysimetry_Super_Update$Time07_13_23 - Lysimetry_Super_Update$Time07_11_23

Lysimetry_Super_Update$Time_diff_07_13_07_17 <- 
  Lysimetry_Super_Update$Time07_17_23 - Lysimetry_Super_Update$Time07_13_23

Lysimetry_Super_Update$Time_diff_07_17_07_18 <- 
  Lysimetry_Super_Update$Time07_18_23 - Lysimetry_Super_Update$Time07_17_23

Lysimetry_Super_Update$Time_diff_07_18_07_19 <- 
  Lysimetry_Super_Update$Time07_19_23 - Lysimetry_Super_Update$Time07_18_23

Lysimetry_Super_Update$Time_diff_07_18_07_20 <- 
  Lysimetry_Super_Update$Time07_20_23 - Lysimetry_Super_Update$Time07_18_23

Lysimetry_Super_Update$Time_diff_07_20_07_24 <- 
  Lysimetry_Super_Update$Time07_24_23 - Lysimetry_Super_Update$Time07_20_23

Lysimetry_Super_Update$Time_diff_07_24_07_25 <- 
  Lysimetry_Super_Update$Time07_25_23 - Lysimetry_Super_Update$Time07_24_23

Lysimetry_Super_Update$Time_diff_07_25_07_26 <- 
  Lysimetry_Super_Update$Time07_26_23 - Lysimetry_Super_Update$Time07_25_23

Lysimetry_Super_Update$Time_diff_07_24_07_27 <- 
  Lysimetry_Super_Update$Time07_27_23 - Lysimetry_Super_Update$Time07_24_23

Lysimetry_Super_Update$Time_diff_07_27_07_31 <- 
  Lysimetry_Super_Update$Time07_31_23 - Lysimetry_Super_Update$Time07_27_23

Lysimetry_Super_Update$Time_diff_07_31_08_01 <- 
  Lysimetry_Super_Update$Time08_01_23 - Lysimetry_Super_Update$Time07_31_23

Lysimetry_Super_Update$Time_diff_08_01_08_02 <- 
  Lysimetry_Super_Update$Time08_02_23 - Lysimetry_Super_Update$Time08_01_23

Lysimetry_Super_Update$Time_diff_08_01_08_03 <- 
  Lysimetry_Super_Update$Time08_03_23 - Lysimetry_Super_Update$Time08_01_23

Lysimetry_Super_Update$Time_diff_08_03_08_07 <- 
  Lysimetry_Super_Update$Time08_07_23 - Lysimetry_Super_Update$Time08_03_23

Lysimetry_Super_Update$Time_diff_08_07_08_08 <- 
  Lysimetry_Super_Update$Time08_08_23 - Lysimetry_Super_Update$Time08_07_23

Lysimetry_Super_Update$Time_diff_08_08_08_09 <- 
  Lysimetry_Super_Update$Time08_09_23 - Lysimetry_Super_Update$Time08_08_23

Lysimetry_Super_Update$Time_diff_08_07_08_10 <- 
  Lysimetry_Super_Update$Time08_10_23 - Lysimetry_Super_Update$Time08_07_23

Lysimetry_Super_Update$Time_diff_08_10_08_14 <- 
  Lysimetry_Super_Update$Time08_14_23 - Lysimetry_Super_Update$Time08_10_23

Lysimetry_Super_Update$Time_diff_08_14_08_15 <- 
  Lysimetry_Super_Update$Time08_15_23 - Lysimetry_Super_Update$Time08_14_23

Lysimetry_Super_Update$Time_diff_08_15_08_16 <- 
  Lysimetry_Super_Update$Time08_16_23 - Lysimetry_Super_Update$Time08_15_23

# Convert difftime to numeric in hours
Lysimetry_Super_Update$Time_diff_06_19_06_21 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_19_06_21, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_21_06_22 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_21_06_22, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_22_06_23 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_22_06_23, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_23_06_26 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_23_06_26, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_26_06_27 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_26_06_27, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_27_06_28 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_27_06_28, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_28_06_29 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_28_06_29, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_29_07_03 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_29_07_03, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_03_07_05 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_03_07_05, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_05_07_06 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_05_07_06, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_06_07_07 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_06_07_07, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_07_07_10 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_07_07_10, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_10_07_11 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_10_07_11, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_10_07_12 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_10_07_12, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_11_07_12 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_11_07_12, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_11_07_13 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_11_07_13, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_13_07_17 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_13_07_17, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_17_07_18 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_17_07_18, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_18_07_19 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_18_07_19, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_18_07_20 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_18_07_20, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_20_07_24 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_20_07_24, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_24_07_25 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_24_07_25, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_25_07_26 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_25_07_26, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_24_07_27 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_24_07_27, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_27_07_31 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_27_07_31, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_31_08_01 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_31_08_01, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_01_08_02 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_01_08_02, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_01_08_03 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_01_08_03, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_03_08_07 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_03_08_07, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_07_08_08 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_07_08_08, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_08_08_09 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_08_08_09, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_07_08_10 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_07_08_10, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_10_08_14 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_10_08_14, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_14_08_15 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_14_08_15, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_15_08_16 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_15_08_16, 
  units = "hours"
)

#  Calculate weight differences
Lysimetry_Super_Update$Weight_diff_06_19_06_21 <- 
  Lysimetry_Super_Update$`sample06/19/23` - Lysimetry_Super_Update$`sample06/21/23`

Lysimetry_Super_Update$Weight_diff_06_21_06_22 <- 
  Lysimetry_Super_Update$`sample06/21/23` - Lysimetry_Super_Update$`sample06/22/23`

Lysimetry_Super_Update$Weight_diff_06_22_06_23 <- 
  Lysimetry_Super_Update$`sample06/22/23` - Lysimetry_Super_Update$`sample06/23/23`

Lysimetry_Super_Update$Weight_diff_06_23_06_26 <- 
  Lysimetry_Super_Update$`water06/23/23` - Lysimetry_Super_Update$`sample06/26/23`

Lysimetry_Super_Update$Weight_diff_06_26_06_27 <- 
  Lysimetry_Super_Update$`water06/26/23` - Lysimetry_Super_Update$`sample06/27/23`

Lysimetry_Super_Update$Weight_diff_06_27_06_28 <- 
  Lysimetry_Super_Update$`sample06/27/23` - Lysimetry_Super_Update$`sample06/28/23`

Lysimetry_Super_Update$Weight_diff_06_28_06_29 <- 
  Lysimetry_Super_Update$`sample06/28/23` - Lysimetry_Super_Update$`sample06/29/23`

Lysimetry_Super_Update$Weight_diff_06_29_07_03 <- 
  Lysimetry_Super_Update$`water06/29/23` - Lysimetry_Super_Update$`sample07/03/23`

Lysimetry_Super_Update$Weight_diff_07_03_07_05 <- 
  Lysimetry_Super_Update$`water07/03/23` - Lysimetry_Super_Update$`sample07/05/23`

Lysimetry_Super_Update$Weight_diff_07_05_07_06 <- 
  Lysimetry_Super_Update$`sample07/05/23` - Lysimetry_Super_Update$`sample07/06/23`

Lysimetry_Super_Update$Weight_diff_07_06_07_07 <- 
  Lysimetry_Super_Update$`water07/06/23` - Lysimetry_Super_Update$`sample07/07/23`

Lysimetry_Super_Update$Weight_diff_07_07_07_10 <- 
  Lysimetry_Super_Update$`sample07/07/23` - Lysimetry_Super_Update$`sample07/10/23`

Lysimetry_Super_Update$Weight_diff_07_10_07_11 <- 
  Lysimetry_Super_Update$`water07/10/23` - Lysimetry_Super_Update$`sample07/11/23`

Lysimetry_Super_Update$Weight_diff_07_10_07_12 <- 
  Lysimetry_Super_Update$`sample07/10/23` - Lysimetry_Super_Update$`sample07/12/23`

Lysimetry_Super_Update$Weight_diff_07_11_07_12 <- 
  Lysimetry_Super_Update$`sample07/11/23` - Lysimetry_Super_Update$`sample07/12/23`

Lysimetry_Super_Update$Weight_diff_07_11_07_13 <- 
  Lysimetry_Super_Update$`sample07/11/23` - Lysimetry_Super_Update$`sample07/13/23`

Lysimetry_Super_Update$Weight_diff_07_13_07_17 <- 
  Lysimetry_Super_Update$`water07/13/23` - Lysimetry_Super_Update$`sample07/17/23`

Lysimetry_Super_Update$Weight_diff_07_17_07_18 <- 
  Lysimetry_Super_Update$`water07/17/23` - Lysimetry_Super_Update$`sample07/18/23`

Lysimetry_Super_Update$Weight_diff_07_18_07_19 <- 
  Lysimetry_Super_Update$`sample07/18/23` - Lysimetry_Super_Update$`sample07/19/23`

Lysimetry_Super_Update$Weight_diff_07_18_07_20 <- 
  Lysimetry_Super_Update$`sample07/18/23` - Lysimetry_Super_Update$`sample07/20/23`

Lysimetry_Super_Update$Weight_diff_07_20_07_24 <- 
  Lysimetry_Super_Update$`water07/20/23` - Lysimetry_Super_Update$`sample07/24/23`

Lysimetry_Super_Update$Weight_diff_07_24_07_25 <- 
  Lysimetry_Super_Update$`water07/24/23` - Lysimetry_Super_Update$`sample07/25/23`

Lysimetry_Super_Update$Weight_diff_07_25_07_26 <- 
  Lysimetry_Super_Update$`sample07/25/23` - Lysimetry_Super_Update$`sample07/26/23`

Lysimetry_Super_Update$Weight_diff_07_24_07_27 <- 
  Lysimetry_Super_Update$`water07/24/23` - Lysimetry_Super_Update$`sample07/27/23`

Lysimetry_Super_Update$Weight_diff_07_27_07_31 <- 
  Lysimetry_Super_Update$`water07/27/23` - Lysimetry_Super_Update$`sample07/31/23`

Lysimetry_Super_Update$Weight_diff_07_31_08_01 <- 
  Lysimetry_Super_Update$`water07/31/23` - Lysimetry_Super_Update$`sample08/01/23`

Lysimetry_Super_Update$Weight_diff_08_01_08_02 <- 
  Lysimetry_Super_Update$`sample08/01/23` - Lysimetry_Super_Update$`sample08/02/23`

Lysimetry_Super_Update$Weight_diff_08_01_08_03 <- 
  Lysimetry_Super_Update$`sample08/01/23` - Lysimetry_Super_Update$`sample08/03/23`

Lysimetry_Super_Update$Weight_diff_08_03_08_07 <- 
  Lysimetry_Super_Update$`water08/03/23` - Lysimetry_Super_Update$`sample08/07/23`

Lysimetry_Super_Update$Weight_diff_08_07_08_08 <- 
  Lysimetry_Super_Update$`water08/07/23` - Lysimetry_Super_Update$`sample08/08/23`

Lysimetry_Super_Update$Weight_diff_08_08_08_09 <- 
  Lysimetry_Super_Update$`sample08/08/23` - Lysimetry_Super_Update$`sample08/09/23`

Lysimetry_Super_Update$Weight_diff_08_07_08_10 <- 
  Lysimetry_Super_Update$`water08/07/23` - Lysimetry_Super_Update$`sample08/10/23`

Lysimetry_Super_Update$Weight_diff_08_10_08_14 <- 
  Lysimetry_Super_Update$`water08/10/23` - Lysimetry_Super_Update$`sample08/14/23`

Lysimetry_Super_Update$Weight_diff_08_14_08_15 <- 
  Lysimetry_Super_Update$`sample08/14/23` - Lysimetry_Super_Update$`sample08/15/23`

Lysimetry_Super_Update$Weight_diff_08_15_08_16 <- 
  Lysimetry_Super_Update$`sample08/15/23` - Lysimetry_Super_Update$`sample08/16/23`

# Divide time difference by transpiration rate 
Lysimetry_Super_Update$Transpiration_rate_06_19_06_21 <- 
  Lysimetry_Super_Update$Weight_diff_06_19_06_21 / as.numeric(Lysimetry_Super_Update$Time_diff_06_19_06_21, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_21_06_22 <- 
  Lysimetry_Super_Update$Weight_diff_06_21_06_22 / as.numeric(Lysimetry_Super_Update$Time_diff_06_21_06_22, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_22_06_23 <- 
  Lysimetry_Super_Update$Weight_diff_06_22_06_23 / as.numeric(Lysimetry_Super_Update$Time_diff_06_22_06_23, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_23_06_26 <- 
  Lysimetry_Super_Update$Weight_diff_06_23_06_26 / as.numeric(Lysimetry_Super_Update$Time_diff_06_23_06_26, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_26_06_27 <- 
  Lysimetry_Super_Update$Weight_diff_06_26_06_27 / as.numeric(Lysimetry_Super_Update$Time_diff_06_26_06_27, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_27_06_28 <- 
  Lysimetry_Super_Update$Weight_diff_06_27_06_28 / as.numeric(Lysimetry_Super_Update$Time_diff_06_27_06_28, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_28_06_29 <- 
  Lysimetry_Super_Update$Weight_diff_06_28_06_29 / as.numeric(Lysimetry_Super_Update$Time_diff_06_28_06_29, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_29_07_03 <- 
  Lysimetry_Super_Update$Weight_diff_06_29_07_03 / as.numeric(Lysimetry_Super_Update$Time_diff_06_29_07_03, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_03_07_05 <- 
  Lysimetry_Super_Update$Weight_diff_07_03_07_05 / as.numeric(Lysimetry_Super_Update$Time_diff_07_03_07_05, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_05_07_06 <- 
  Lysimetry_Super_Update$Weight_diff_07_05_07_06 / as.numeric(Lysimetry_Super_Update$Time_diff_07_05_07_06, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_06_07_07 <- 
  Lysimetry_Super_Update$Weight_diff_07_06_07_07 / as.numeric(Lysimetry_Super_Update$Time_diff_07_06_07_07, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_07_07_10 <- 
  Lysimetry_Super_Update$Weight_diff_07_07_07_10 / as.numeric(Lysimetry_Super_Update$Time_diff_07_07_07_10, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_10_07_11 <- 
  Lysimetry_Super_Update$Weight_diff_07_10_07_11 / as.numeric(Lysimetry_Super_Update$Time_diff_07_10_07_11, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_10_07_12 <- 
  Lysimetry_Super_Update$Weight_diff_07_10_07_12 / as.numeric(Lysimetry_Super_Update$Time_diff_07_10_07_12, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_11_07_12 <- 
  Lysimetry_Super_Update$Weight_diff_07_11_07_12 / as.numeric(Lysimetry_Super_Update$Time_diff_07_11_07_12, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_11_07_13 <- 
  Lysimetry_Super_Update$Weight_diff_07_11_07_13 / as.numeric(Lysimetry_Super_Update$Time_diff_07_11_07_13, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_13_07_17 <- 
  Lysimetry_Super_Update$Weight_diff_07_13_07_17 / as.numeric(Lysimetry_Super_Update$Time_diff_07_13_07_17, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_17_07_18 <- 
  Lysimetry_Super_Update$Weight_diff_07_17_07_18 / as.numeric(Lysimetry_Super_Update$Time_diff_07_17_07_18, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_18_07_19 <- 
  Lysimetry_Super_Update$Weight_diff_07_18_07_19 / as.numeric(Lysimetry_Super_Update$Time_diff_07_18_07_19, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_18_07_20 <- 
  Lysimetry_Super_Update$Weight_diff_07_18_07_20 / as.numeric(Lysimetry_Super_Update$Time_diff_07_18_07_20, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_20_07_24 <- 
  Lysimetry_Super_Update$Weight_diff_07_20_07_24 / as.numeric(Lysimetry_Super_Update$Time_diff_07_20_07_24, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_24_07_25 <- 
  Lysimetry_Super_Update$Weight_diff_07_24_07_25 / as.numeric(Lysimetry_Super_Update$Time_diff_07_24_07_25, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_25_07_26 <- 
  Lysimetry_Super_Update$Weight_diff_07_25_07_26 / as.numeric(Lysimetry_Super_Update$Time_diff_07_25_07_26, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_24_07_27 <- 
  Lysimetry_Super_Update$Weight_diff_07_24_07_27 / as.numeric(Lysimetry_Super_Update$Time_diff_07_24_07_27, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_27_07_31 <- 
  Lysimetry_Super_Update$Weight_diff_07_27_07_31 / as.numeric(Lysimetry_Super_Update$Time_diff_07_27_07_31, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_31_08_01 <- 
  Lysimetry_Super_Update$Weight_diff_07_31_08_01 / as.numeric(Lysimetry_Super_Update$Time_diff_07_31_08_01, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_01_08_02 <- 
  Lysimetry_Super_Update$Weight_diff_08_01_08_02 / as.numeric(Lysimetry_Super_Update$Time_diff_08_01_08_02, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_01_08_03 <- 
  Lysimetry_Super_Update$Weight_diff_08_01_08_03 / as.numeric(Lysimetry_Super_Update$Time_diff_08_01_08_03, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_03_08_07 <- 
  Lysimetry_Super_Update$Weight_diff_08_03_08_07 / as.numeric(Lysimetry_Super_Update$Time_diff_08_03_08_07, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_07_08_08 <- 
  Lysimetry_Super_Update$Weight_diff_08_07_08_08 / as.numeric(Lysimetry_Super_Update$Time_diff_08_07_08_08, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_08_08_09 <- 
  Lysimetry_Super_Update$Weight_diff_08_08_08_09 / as.numeric(Lysimetry_Super_Update$Time_diff_08_08_08_09, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_07_08_10 <- 
  Lysimetry_Super_Update$Weight_diff_08_07_08_10 / as.numeric(Lysimetry_Super_Update$Time_diff_08_07_08_10, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_10_08_14 <- 
  Lysimetry_Super_Update$Weight_diff_08_10_08_14 / as.numeric(Lysimetry_Super_Update$Time_diff_08_10_08_14, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_14_08_15 <- 
  Lysimetry_Super_Update$Weight_diff_08_14_08_15 / as.numeric(Lysimetry_Super_Update$Time_diff_08_14_08_15, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_15_08_16 <- 
  Lysimetry_Super_Update$Weight_diff_08_15_08_16 / as.numeric(Lysimetry_Super_Update$Time_diff_08_15_08_16, units = "hours")

# Create a dataframe with the columns of interest
Lysimetry_calculated <- data.frame(
  ID = Lysimetry_Super_Update$ID, # Plant IDs
  Interval_1 = "06/19 - 06/21", # Add a label for the first time interval
  Weight_Diff_1 = Lysimetry_Super_Update$Weight_diff_06_19_06_21,
  Time_Diff_1 = as.numeric(Lysimetry_Super_Update$Time_diff_06_19_06_21, units = "hours"),
  Transpiration_Rate_1 = Lysimetry_Super_Update$Transpiration_rate_06_19_06_21,
  
  Interval_2 = "06/21 - 06/22", # Add a label for the second time interval
  Weight_Diff_2 = Lysimetry_Super_Update$Weight_diff_06_21_06_22,
  Time_Diff_2 = as.numeric(Lysimetry_Super_Update$Time_diff_06_21_06_22, units = "hours"),
  Transpiration_Rate_2 = Lysimetry_Super_Update$Transpiration_rate_06_21_06_22,
  
  Interval_3 = "06/22 - 06/23", # Add a label for the second time interval
  Weight_Diff_3 = Lysimetry_Super_Update$Weight_diff_06_22_06_23,
  Time_Diff_3 = as.numeric(Lysimetry_Super_Update$Time_diff_06_22_06_23, units = "hours"),
  Transpiration_Rate_3 = Lysimetry_Super_Update$Transpiration_rate_06_22_06_23,
  
  Interval_4 = "06/23 - 06/26", # Add a label for the second time interval
  Weight_Diff_4 = Lysimetry_Super_Update$Weight_diff_06_23_06_26,
  Time_Diff_4 = as.numeric(Lysimetry_Super_Update$Time_diff_06_23_06_26, units = "hours"),
  Transpiration_Rate_4 = Lysimetry_Super_Update$Transpiration_rate_06_23_06_26,
  
  Interval_5 = "06/26 - 06/27", # Add a label for the second time interval
  Weight_Diff_5 = Lysimetry_Super_Update$Weight_diff_06_26_06_27,
  Time_Diff_5 = as.numeric(Lysimetry_Super_Update$Time_diff_06_26_06_27, units = "hours"),
  Transpiration_Rate_5 = Lysimetry_Super_Update$Transpiration_rate_06_26_06_27,
  
  Interval_6 = "06/27 - 06/28", # Add a label for the second time interval
  Weight_Diff_6 = Lysimetry_Super_Update$Weight_diff_06_27_06_28,
  Time_Diff_6 = as.numeric(Lysimetry_Super_Update$Time_diff_06_27_06_28, units = "hours"),
  Transpiration_Rate_6 = Lysimetry_Super_Update$Transpiration_rate_06_27_06_28,
  
  Interval_7 = "06/28 - 06/29", # Add a label for the second time interval
  Weight_Diff_7 = Lysimetry_Super_Update$Weight_diff_06_28_06_29,
  Time_Diff_7 = as.numeric(Lysimetry_Super_Update$Time_diff_06_28_06_29, units = "hours"),
  Transpiration_Rate_7 = Lysimetry_Super_Update$Transpiration_rate_06_28_06_29,
  
  Interval_8 = "06/29 - 07/03", # Add a label for the second time interval
  Weight_Diff_8 = Lysimetry_Super_Update$Weight_diff_06_29_07_03,
  Time_Diff_8 = as.numeric(Lysimetry_Super_Update$Time_diff_06_29_07_03, units = "hours"),
  Transpiration_Rate_8 = Lysimetry_Super_Update$Transpiration_rate_06_29_07_03,
  
  Interval_9 = "07/03 - 07/05", # Add a label for the second time interval
  Weight_Diff_9 = Lysimetry_Super_Update$Weight_diff_07_03_07_05,
  Time_Diff_9 = as.numeric(Lysimetry_Super_Update$Time_diff_07_03_07_05, units = "hours"),
  Transpiration_Rate_9 = Lysimetry_Super_Update$Transpiration_rate_07_03_07_05,
  
  Interval_10 = "07/05 - 07/06", # Add a label for the second time interval
  Weight_Diff_10 = Lysimetry_Super_Update$Weight_diff_07_05_07_06,
  Time_Diff_10 = as.numeric(Lysimetry_Super_Update$Time_diff_07_05_07_06, units = "hours"),
  Transpiration_Rate_10 = Lysimetry_Super_Update$Transpiration_rate_07_05_07_06,
  
  Interval_11 = "07/06 - 07/07", # Add a label for the second time interval
  Weight_Diff_11 = Lysimetry_Super_Update$Weight_diff_07_06_07_07,
  Time_Diff_11 = as.numeric(Lysimetry_Super_Update$Time_diff_07_06_07_07, units = "hours"),
  Transpiration_Rate_11 = Lysimetry_Super_Update$Transpiration_rate_07_06_07_07,
  
  Interval_12 = "07/07 - 07/10", # Add a label for the second time interval
  Weight_Diff_12 = Lysimetry_Super_Update$Weight_diff_07_07_07_10,
  Time_Diff_12 = as.numeric(Lysimetry_Super_Update$Time_diff_07_07_07_10, units = "hours"),
  Transpiration_Rate_12 = Lysimetry_Super_Update$Transpiration_rate_07_07_07_10,
  
  Interval_13 = "07/10 - 07/11", # Add a label for the second time interval
  Weight_Diff_13 = Lysimetry_Super_Update$Weight_diff_07_10_07_11,
  Time_Diff_13 = as.numeric(Lysimetry_Super_Update$Time_diff_07_10_07_11, units = "hours"),
  Transpiration_Rate_13 = Lysimetry_Super_Update$Transpiration_rate_07_10_07_11,
  
  Interval_14 = "07/10 - 07/12", # Add a label for the second time interval
  Weight_Diff_14 = Lysimetry_Super_Update$Weight_diff_07_10_07_12,
  Time_Diff_14 = as.numeric(Lysimetry_Super_Update$Time_diff_07_10_07_12, units = "hours"),
  Transpiration_Rate_14 = Lysimetry_Super_Update$Transpiration_rate_07_10_07_12,
  
  Interval_15 = "07/11 - 07/12", # Add a label for the second time interval
  Weight_Diff_15 = Lysimetry_Super_Update$Weight_diff_07_11_07_12,
  Time_Diff_15 = as.numeric(Lysimetry_Super_Update$Time_diff_07_11_07_12, units = "hours"),
  Transpiration_Rate_15 = Lysimetry_Super_Update$Transpiration_rate_07_11_07_12, 
  
  Interval_16 = "07/11 - 07/13", # Add a label for the second time interval
  Weight_Diff_16 = Lysimetry_Super_Update$Weight_diff_07_11_07_13,
  Time_Diff_16 = as.numeric(Lysimetry_Super_Update$Time_diff_07_11_07_13, units = "hours"),
  Transpiration_Rate_16 = Lysimetry_Super_Update$Transpiration_rate_07_11_07_13,
  
  Interval_17 = "07/13 - 07/17", # Add a label for the second time interval
  Weight_Diff_17 = Lysimetry_Super_Update$Weight_diff_07_13_07_17,
  Time_Diff_17 = as.numeric(Lysimetry_Super_Update$Time_diff_07_13_07_17, units = "hours"),
  Transpiration_Rate_17 = Lysimetry_Super_Update$Transpiration_rate_07_13_07_17,
  
  Interval_18 = "07/17 - 07/18", # Add a label for the second time interval
  Weight_Diff_18 = Lysimetry_Super_Update$Weight_diff_07_17_07_18,
  Time_Diff_18 = as.numeric(Lysimetry_Super_Update$Time_diff_07_17_07_18, units = "hours"),
  Transpiration_Rate_18 = Lysimetry_Super_Update$Transpiration_rate_07_17_07_18,
  
  Interval_19 = "07/18 - 07/19", # Add a label for the second time interval
  Weight_Diff_19 = Lysimetry_Super_Update$Weight_diff_07_18_07_19,
  Time_Diff_19 = as.numeric(Lysimetry_Super_Update$Time_diff_07_18_07_19, units = "hours"),
  Transpiration_Rate_19 = Lysimetry_Super_Update$Transpiration_rate_07_18_07_19,
  
  Interval_20 = "07/18 - 07/20", # Add a label for the second time interval
  Weight_Diff_20 = Lysimetry_Super_Update$Weight_diff_07_18_07_20,
  Time_Diff_20 = as.numeric(Lysimetry_Super_Update$Time_diff_07_18_07_20, units = "hours"),
  Transpiration_Rate_20 = Lysimetry_Super_Update$Transpiration_rate_07_18_07_20,
  
  Interval_21 = "07/20 - 07/24", # Add a label for the second time interval
  Weight_Diff_21 = Lysimetry_Super_Update$Weight_diff_07_20_07_24,
  Time_Diff_21 = as.numeric(Lysimetry_Super_Update$Time_diff_07_20_07_24, units = "hours"),
  Transpiration_Rate_21 = Lysimetry_Super_Update$Transpiration_rate_07_20_07_24,
  
  Interval_22 = "07/24 - 07/25", # Add a label for the second time interval
  Weight_Diff_22 = Lysimetry_Super_Update$Weight_diff_07_24_07_25,
  Time_Diff_22 = as.numeric(Lysimetry_Super_Update$Time_diff_07_24_07_25, units = "hours"),
  Transpiration_Rate_22 = Lysimetry_Super_Update$Transpiration_rate_07_24_07_25,
  
  Interval_23 = "07/25 - 07/26", # Add a label for the second time interval
  Weight_Diff_23 = Lysimetry_Super_Update$Weight_diff_07_25_07_26,
  Time_Diff_23 = as.numeric(Lysimetry_Super_Update$Time_diff_07_25_07_26, units = "hours"),
  Transpiration_Rate_23 = Lysimetry_Super_Update$Transpiration_rate_07_25_07_26,
  
  Interval_24 = "07/24 - 07/27", # Add a label for the second time interval
  Weight_Diff_24 = Lysimetry_Super_Update$Weight_diff_07_24_07_27,
  Time_Diff_24 = as.numeric(Lysimetry_Super_Update$Time_diff_07_24_07_27, units = "hours"),
  Transpiration_Rate_24 = Lysimetry_Super_Update$Transpiration_rate_07_24_07_27,
  
  Interval_25 = "07/27 - 07/31", # Add a label for the second time interval
  Weight_Diff_25 = Lysimetry_Super_Update$Weight_diff_07_27_07_31,
  Time_Diff_25 = as.numeric(Lysimetry_Super_Update$Time_diff_07_27_07_31, units = "hours"),
  Transpiration_Rate_25 = Lysimetry_Super_Update$Transpiration_rate_07_27_07_31,
  
  Interval_26 = "07/31 - 08/01", # Add a label for the second time interval
  Weight_Diff_26 = Lysimetry_Super_Update$Weight_diff_07_31_08_01,
  Time_Diff_26 = as.numeric(Lysimetry_Super_Update$Time_diff_07_31_08_01, units = "hours"),
  Transpiration_Rate_26 = Lysimetry_Super_Update$Transpiration_rate_07_31_08_01,
  
  Interval_27 = "08/01 - 08/02", # Add a label for the second time interval
  Weight_Diff_27 = Lysimetry_Super_Update$Weight_diff_08_01_08_02,
  Time_Diff_27 = as.numeric(Lysimetry_Super_Update$Time_diff_08_01_08_02, units = "hours"),
  Transpiration_Rate_27 = Lysimetry_Super_Update$Transpiration_rate_08_01_08_02,
  
  Interval_28 = "08/01 - 08/03", # Add a label for the second time interval
  Weight_Diff_28 = Lysimetry_Super_Update$Weight_diff_08_01_08_03,
  Time_Diff_28 = as.numeric(Lysimetry_Super_Update$Time_diff_08_01_08_03, units = "hours"),
  Transpiration_Rate_28 = Lysimetry_Super_Update$Transpiration_rate_08_01_08_03,
  
  Interval_29 = "08/03 - 08/07", # Add a label for the second time interval
  Weight_Diff_29 = Lysimetry_Super_Update$Weight_diff_08_03_08_07,
  Time_Diff_29 = as.numeric(Lysimetry_Super_Update$Time_diff_08_03_08_07, units = "hours"),
  Transpiration_Rate_29 = Lysimetry_Super_Update$Transpiration_rate_08_03_08_07,
  
  Interval_30 = "08/07 - 08/08", # Add a label for the second time interval
  Weight_Diff_30 = Lysimetry_Super_Update$Weight_diff_08_07_08_08,
  Time_Diff_30 = as.numeric(Lysimetry_Super_Update$Time_diff_08_07_08_08, units = "hours"),
  Transpiration_Rate_30 = Lysimetry_Super_Update$Transpiration_rate_08_07_08_08,
  
  Interval_31 = "08/08 - 08/09", # Add a label for the second time interval
  Weight_Diff_31 = Lysimetry_Super_Update$Weight_diff_08_08_08_09,
  Time_Diff_31 = as.numeric(Lysimetry_Super_Update$Time_diff_08_08_08_09, units = "hours"),
  Transpiration_Rate_31 = Lysimetry_Super_Update$Transpiration_rate_08_08_08_09,
  
  Interval_32 = "08/07 - 08/10", # Add a label for the second time interval
  Weight_Diff_32 = Lysimetry_Super_Update$Weight_diff_08_07_08_10,
  Time_Diff_32 = as.numeric(Lysimetry_Super_Update$Time_diff_08_07_08_10, units = "hours"),
  Transpiration_Rate_32 = Lysimetry_Super_Update$Transpiration_rate_08_07_08_10,
  
  Interval_33 = "08/10 - 08/14", # Add a label for the second time interval
  Weight_Diff_33 = Lysimetry_Super_Update$Weight_diff_08_10_08_14,
  Time_Diff_33 = as.numeric(Lysimetry_Super_Update$Time_diff_08_10_08_14, units = "hours"),
  Transpiration_Rate_33 = Lysimetry_Super_Update$Transpiration_rate_08_10_08_14,
  
  Interval_34 = "08/14 - 08/15", # Add a label for the second time interval
  Weight_Diff_34 = Lysimetry_Super_Update$Weight_diff_08_14_08_15,
  Time_Diff_34 = as.numeric(Lysimetry_Super_Update$Time_diff_08_14_08_15, units = "hours"),
  Transpiration_Rate_34 = Lysimetry_Super_Update$Transpiration_rate_08_14_08_15,
  
  Interval_35 = "08/15 - 08/16", # Add a label for the second time interval
  Weight_Diff_35 = Lysimetry_Super_Update$Weight_diff_08_15_08_16,
  Time_Diff_35 = as.numeric(Lysimetry_Super_Update$Time_diff_08_15_08_16, units = "hours"),
  Transpiration_Rate_35 = Lysimetry_Super_Update$Transpiration_rate_08_15_08_16
)

# Manually saved the transpiration rate columns on a new Excel sheet for ease 
transpiration_rates <- read_xlsx("Lysimetry+Info.xlsx", sheet = "Transpiration Rates")




