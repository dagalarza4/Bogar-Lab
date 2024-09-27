require(tidyverse)
require(readxl)
require(cowplot)

# Laura's effort####

# Reshape the data

lysimetry_data = read_csv("Lysimetry_data.csv")
lysimetry_data = lysimetry_data[1:89,] #for some reason we're getting a ton of blank lines at the end, cutting them here.
treatments = read_csv("Treatment_key.csv") %>%
  mutate(ID = Plant_ID)

# Reshape the data (thanks Chat GPT for the regex and tidyverse wizardry!)
reshaped_data <- lysimetry_data %>%
  pivot_longer(cols = -ID,
               names_to = c(".value", "date"),
               names_pattern = "(sample|Time)(.*)") %>%
  drop_na()

reshaped_data = mutate(reshaped_data,
                       newtime = parse_date_time(reshaped_data$Time, "mdy_HM"))
# A couple of the times are hours minutes AND seconds, dealing
# with this below. (A savvier coder would use tidyverse for this
# but LB is faster with base R.)
for (i in 1:nrow(reshaped_data)) {
  if (is.na(reshaped_data$newtime[i])) {
    reshaped_data$newtime[i] = parse_date_time(reshaped_data$Time[i], "mdy_HMS")
  }
}

reshaped_data <- reshaped_data %>%
  select(ID, sample, newtime) %>%
  arrange(ID, desc(newtime))

last_three <- reshaped_data %>%
  group_by(ID) %>%
  slice_head(n = 3) %>%
  ungroup()

last_and_third_to_last <- reshaped_data %>%
  group_by(ID) %>%
  slice(c(1,3)) %>% # keep just last and third to last measurement
  ungroup()

last_and_second_to_last <- reshaped_data %>%
  group_by(ID) %>%
  slice(c(3,2)) %>% # keep just last and second to last measurement
  ungroup()

# now working to calculate hourly loss
hourly_loss <- last_and_third_to_last %>%
  arrange(ID, desc(newtime)) %>%
  group_by(ID) %>%
  summarize(massdiff_g = first(sample) - last(sample), # generates some confusing negative values, what's up?
            timediff = first(newtime) - last(newtime)) %>%
  mutate(mass_per_day = massdiff_g/as.numeric(timediff)) %>%
  mutate(mass_per_hr = mass_per_day/24) #24 hrs per day

# So, it looks like the last and third to last measurements do
# NOT consistently capture the final 48 hrs. 
# will need to fix this.
# for now, what does it look like?

# Adding in treatment info for plotting

alltogether = left_join(hourly_loss, treatments)

# Test boxplot

testplot = ggplot(data = alltogether) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  geom_boxplot(outlier.shape = NA, aes(x = Species, y = mass_per_hr, color = Colonized)) +
  geom_jitter(aes(x = Species, y = mass_per_hr, color = Colonized)) +
  xlab("Fungi") + 
  facet_grid(. ~ Treatment)
  ylab("Transpiration rate (water loss g/hr)") +
  scale_x_discrete(breaks=c("NM","SP", "TC", "RP", "R+S", "R+T"),
                   labels=c("No fungi",
                            "Suillus ponderosus", 
                            "Truncocolumella citrina",
                            "Rhizopogon evadens",
                            "R. evadens and S. ponderosus",
                            "R. evadens and T. citrina"))

testplot
# Preliminary thoughts:
# I think I calculated these rates backwards?
# all the mass losses for drought are negative.
# scale_x_discrete not working with the faceted plot
# would need to redo axis labels for publication


# DAG's effort 7/19/24
library(ggplot2)
library(dplyr)

# Boxplot
alltogether$Species <- factor(alltogether$Species, levels = c("NM", "RP", "SP", "TC", "R+S", "S+T"))


testplot <- ggplot(data = alltogether) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  geom_boxplot(aes(x = Species, y = mass_per_hr, color = Colonized), outlier.shape = NA) +
  geom_jitter(aes(x = Species, y = mass_per_hr, color = Colonized)) +
  xlab("Fungal Species") + 
  ylab("Transpiration Rate (water loss g/hr)") +
  scale_x_discrete(labels = c("NM",
                              "RP",
                              "SP", 
                              "TC",
                              "R+S",
                              "S+T")) +
  facet_grid(. ~ Treatment)

print(testplot)

#New boxplot changes 07/20/23
alltogether <- alltogether %>%
  mutate(Species = case_when(
    grepl("NM", Species_Treatment) ~ "NM",
    grepl("RP", Species_Treatment) ~ "RP",
    grepl("SP", Species_Treatment) ~ "SP",
    grepl("TC", Species_Treatment) ~ "TC",
    grepl("R\\+S", Species_Treatment) ~ "R+S",
    grepl("S\\+T", Species_Treatment) ~ "S+T"
  ))

alltogether$Species <- factor(alltogether$Species,
                              levels = c("NM", "RP", "SP", "TC", "R+S", "S+T"))

testplot <- ggplot(data = alltogether) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  geom_boxplot(aes(x = Species, y = mass_per_hr, color = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x = Species, y = mass_per_hr, color = Treatment)) +
  xlab("Fungal Species") + 
  ylab("Transpiration Rate (water loss g/hr)") +
  scale_color_manual(name = "Treatment", values = c("control" = "deepskyblue", "drought" = "red")) +
  labs(color = "Treatment")

print(testplot)



# DAG's boxplot ####

##Attempt at finding water loss/hr in positive values 
#07/21/23

hourly_loss <- last_and_third_to_last %>%
  arrange(ID, desc(newtime)) %>%
  group_by(ID) %>%
  summarize(massdiff_g = last(sample) - first(sample), # Corrected calculation
            timediff = as.numeric(difftime(first(newtime), last(newtime), units = "hours"))) %>%
  mutate(mass_per_hr = massdiff_g / timediff) # Directly calculate mass per hour

# Calculate water loss for the last two days before harvest
last_two_days <- reshaped_data %>%
  group_by(ID) %>%
  filter(difftime(max(newtime), newtime, units = "days") <= 2) %>%
  arrange(ID, desc(newtime))

# Ensure only two measurements are considered per plant
last_two_measurements <- last_two_days %>%
  group_by(ID) %>%
  slice_head(n = 2) %>%
  ungroup()

hourly_loss <- last_two_measurements %>%
  arrange(ID, desc(newtime)) %>%
  group_by(ID) %>%
  summarize(massdiff_g = last(sample) - first(sample), # Corrected calculation
            timediff = as.numeric(difftime(first(newtime), last(newtime), units = "hours"))) %>%
  mutate(mass_per_hr = massdiff_g / timediff)

alltogether <- left_join(hourly_loss, treatments)

alltogether$Species <- factor(alltogether$Species,
                              levels = c("NM", "RP", "SP", "TC", "R+S", "S+T"))

testplot <- ggplot(data = alltogether) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  geom_boxplot(aes(x = Species, y = mass_per_hr, color = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x = Species, y = mass_per_hr, color = Treatment)) +
  xlab("Fungal Species") + 
  ylab("Transpiration Rate (water loss g/hr)") +
  scale_color_manual(name = "Treatment", values = c("control" = "deepskyblue", "drought" = "tomato1")) +
  labs(color = "Treatment") +
  geom_hline(yintercept = c(0.05, 0.1, 0.15, 0.2, 0.25), color = "grey", linetype = "solid") +
  ylim(0, 0.25)

print(testplot)
### Above graph is best

#8/9 Update ####

require(tidyverse)
require(readxl)
require(cowplot)

# Read in the lysimetry data
lysimetry_data <- read_csv("Lysimetry_data.csv")
lysimetry_data <- lysimetry_data[1:89,] # Removing blank lines

# Read in the treatment key and harvest days
treatments <- read_csv("Treatment_key.csv") %>%
  mutate(ID = Plant_ID)

harvest_days <- read_csv("Harvest_Days.csv")

# Reshape the lysimetry data
reshaped_data <- lysimetry_data %>%
  pivot_longer(cols = -ID,
               names_to = c(".value", "date"),
               names_pattern = "(sample|Time)(.*)") %>%
  drop_na() %>%
  mutate(newtime = parse_date_time(Time, "mdy_HM"))

# Handling time format discrepancies
for (i in 1:nrow(reshaped_data)) {
  if (is.na(reshaped_data$newtime[i])) {
    reshaped_data$newtime[i] = parse_date_time(reshaped_data$Time[i], "mdy_HMS")
  }
}

# Merge reshaped data with harvest days
reshaped_data <- left_join(reshaped_data, harvest_days, by = "ID")

# Convert LastDay1 and LastDay2 to datetime
reshaped_data <- reshaped_data %>%
  mutate(
    LastDay1 = as.POSIXct(LastDay1, format = "%m/%d/%Y %H:%M"),
    LastDay2 = as.POSIXct(LastDay2, format = "%m/%d/%Y %H:%M")
  )

# Filter the data to only include measurements taken on the last two days before harvest
last_two_days_before_harvest <- reshaped_data %>%
  group_by(ID) %>%
  filter(newtime %in% c(LastDay1, LastDay2)) %>%
  arrange(ID, desc(newtime)) %>%
  ungroup()

# Calculate hourly water loss
hourly_loss <- last_two_days_before_harvest %>%
  arrange(ID, desc(newtime)) %>%
  group_by(ID) %>%
  summarize(
    massdiff_g = last(sample) - first(sample), # Corrected calculation
    timediff = as.numeric(difftime(first(newtime), last(newtime), units = "hours"))
  ) %>%
  mutate(mass_per_hr = massdiff_g / timediff) %>%
  ungroup()

# Combine with treatment data
alltogether <- left_join(hourly_loss, treatments)

# Update species factor levels
alltogether$Species <- factor(alltogether$Species,
                              levels = c("NM", "RP", "SP", "TC", "R+S", "S+T"))

# Create the boxplot
testplot <- ggplot(data = alltogether) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  geom_boxplot(aes(x = Species, y = mass_per_hr, color = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x = Species, y = mass_per_hr, color = Treatment)) +
  xlab("Fungal Species") + 
  ylab("Transpiration Rate (water loss g/hr)") +
  scale_color_manual(name = "Treatment", values = c("control" = "deepskyblue", "drought" = "red")) +
  labs(color = "Treatment") +
  geom_hline(yintercept = c(0.05, 0.1, 0.15, 0.2, 0.25), color = "grey", linetype = "solid") +
  ylim(0, 0.25)

print(testplot)



#New attempt with Lysimetry+Info.csv- did not work
library(readxl)

lys_info <- "Lysimetry+Info.xlsx"

#Convert each sheet into a data frame
harvest_days <- read_excel(lys_info, sheet = "Harvest Days")
days_watered <- read_excel(lys_info, sheet = "Days Watered")

write.csv(harvest_days, "Harvest_Days.csv", row.names = FALSE)
write.csv(days_watered, "Days_Watered.csv", row.names = FALSE)

require(tidyverse)
require(readxl)

lysimetry_data <- read_csv("Lysimetry_data.csv")
treatments <- read_csv("Treatment_key.csv") %>%
  mutate(ID = Plant_ID)
harvest_days <- read_excel("Lysimetry+Info.xlsx", sheet = "Harvest Days") %>%
  rename(ID = Sample, HarvestDate = HarvestDate)

# Reshape the lysimetry data
reshaped_data <- lysimetry_data %>%
  pivot_longer(cols = -ID,
               names_to = c(".value", "date"),
               names_pattern = "(sample|Time)(.*)") %>%
  drop_na() %>%
  mutate(newtime = parse_date_time(Time, "mdy_HM"))

for (i in 1:nrow(reshaped_data)) {
  if (is.na(reshaped_data$newtime[i])) {
    reshaped_data$newtime[i] = parse_date_time(reshaped_data$Time[i], "mdy_HMS")
  }
}

# Merge with harvest days
reshaped_data <- left_join(reshaped_data, harvest_days, by = "ID")

# Filter data for the last 2 days before harvest
last_two_days_before_harvest <- reshaped_data %>%
  group_by(ID) %>%
  filter(difftime(HarvestDate, newtime, units = "days") <= 2) %>%
  arrange(ID, desc(newtime)) %>%
  slice_head(n = 2) %>%
  ungroup()

# Calculate hourly loss
hourly_loss <- last_two_days_before_harvest %>%
  group_by(ID) %>%
  summarize(massdiff_g = last(sample) - first(sample), # Corrected calculation
            timediff = as.numeric(difftime(first(newtime), last(newtime), units = "hours"))) %>%
  mutate(mass_per_hr = massdiff_g / timediff) # Directly calculate mass per hour

# Combine with treatments data
alltogether <- left_join(hourly_loss, treatments)

# Update species factor levels
alltogether$Species <- factor(alltogether$Species,
                              levels = c("NM", "RP", "SP", "TC", "R+S", "S+T"))

# Plotting
testplot <- ggplot(data = alltogether) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  geom_boxplot(aes(x = Species, y = mass_per_hr, color = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x = Species, y = mass_per_hr, color = Treatment)) +
  xlab("Fungal Species") + 
  ylab("Transpiration Rate (water loss g/hr)") +
  scale_color_manual(name = "Treatment", values = c("control" = "deepskyblue", "drought" = "red")) +
  labs(color = "Treatment") +
  geom_hline(yintercept = c(0.05, 0.1, 0.15, 0.2, 0.25), color = "grey", linetype = "solid") +
  ylim(0, 0.25)

print(testplot)





# Tutor's effort ####

LysimetryID <- read_excel("Lysimetry+Data+for+R.xlsx", range = "A1:A90")

LysimetryTime <- read_excel("Lysimetry+Data+for+R.xlsx")%>%
  select(c(contains("Time")))%>%
  mutate(`Time07/11/23`=`Time07/11/23`+4017)


# Create a new dataframe to store the differences
TimeDiff <- data.frame(matrix(NA, nrow = nrow(LysimetryTime), ncol = ncol(LysimetryTime) - 1))

for (i in 1:(ncol(LysimetryTime) - 1)) {
  TimeDiff[, i] <- LysimetryTime[, i + 1] - LysimetryTime[, i]
}
# Rename columns of differences dataframe
column_names <- colnames(LysimetryTime)[-1]

colnames(TimeDiff) <- column_names

TimeDiff <- TimeDiff*24
# Tests
sum(TimeDiff < 0, na.rm = TRUE)

# Diff in Amount
LysimetryMeasure <- read_excel("Lysimetry+Data+for+R.xlsx")%>%
  select(-c( ID,contains("Time")))
MeasureDiff <- data.frame(matrix(NA, nrow = nrow(LysimetryMeasure), ncol = ncol(LysimetryMeasure) - 1))

for (i in 1:(ncol(LysimetryMeasure) - 1)) {
  MeasureDiff[, i] <- LysimetryMeasure[, i + 1] - LysimetryMeasure[, i]
}
# Rename columns
column_names <- colnames(LysimetryMeasure)[-1]

colnames(MeasureDiff) <- column_names

RateChange <- MeasureDiff/TimeDiff

RateChange <- cbind(LysimetryID,RateChange)

# Rename Columns
colnames_df <- colnames(RateChange)
colnames_df <- gsub("sample", "", colnames_df)
colnames(RateChange) <- colnames_df

write.csv(RateChange, "RateChange_modified.csv", row.names = FALSE)



Lysimetry_Info <- read_excel("Lysimetry+Info.xlsx", sheet = "Days Watered")%>%
  filter(ID=='NM04')%>%
  pivot_longer(cols = -ID, names_to = "Date", values_to = "Rate")

# Reshape the data
RateChange_long <- RateChange %>%
  pivot_longer(cols = -ID, names_to = "Date", values_to = "Rate")

# Plotting
ggplot(RateChange_long, aes(x = Date, y = Rate, group = ID, color = factor(ID))) +
  geom_line() +
  geom_vline(data = Lysimetry_Info %>% filter(Rate == 10), aes(xintercept = Date), color = "blue", linetype = "dashed") +
  geom_vline(data = Lysimetry_Info %>% filter(Rate == 20), aes(xintercept = Date), color = "blue", linetype = "solid", size = 1) +
  labs(title = "Weight Change Over Time (g)",
       x = "Date",
       y = "Rate") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot

Harvest_Data <- read_excel("Harvest Data.xlsx")%>%
  select(Plant_ID,Species,Treatment)


BoxDS <- merge(Harvest_Data,RateChange_long,by.x='Plant_ID', by.y = 'ID')%>%
  na.omit()
write.csv(BoxDS, "BoxDS.csv", row.names = FALSE)

BoxDS$Date <- as.Date(BoxDS$Date, format='%m/%d/%y')
BoxDS <- BoxDS%>%
  group_by(Plant_ID)%>%
  arrange(desc(Date))%>%
  slice(1:2)%>%
  ungroup()%>%
  select(-Plant_ID,-Date)%>%
  mutate(Rate=Rate*-1)%>%
  mutate(Species = factor(Species, levels = c('NM','RP','SP','TC','R+S','S+T')))



ggplot(BoxDS, aes(x=Species,y=Rate,fill=Treatment))+
  geom_boxplot()+
  ylim(0,.25)+
  theme_minimal()

#Trying to commit, will this work?


#Timeline of water usage####

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

# Load the data
days_watered <- read_csv("Days_Watered.csv")
harvest_days <- read_csv("Harvest_Days.csv")

# Convert HarvestDate to date format
harvest_days$HarvestDate <- as.Date(harvest_days$HarvestDate, format="%m/%d/%Y")

# Reshape Days_Watered data to long format
days_watered_long <- days_watered %>%
  pivot_longer(cols = -ID, names_to = "date", values_to = "water_amount") %>%
  mutate(date = as.Date(date, format="%m/%d/%Y"))

# Merge with harvest days to filter out data after harvest
days_watered_filtered <- days_watered_long %>%
  left_join(harvest_days, by = c("ID" = "Sample")) %>%
  filter(date <= HarvestDate)

# Load the treatments data
treatments <- read_csv("Treatment_key.csv") %>%
  mutate(ID = Plant_ID) # Ensure the column name matches for merging

# Assuming treatments are already loaded and merged into the data
alltogether <- left_join(days_watered_filtered, treatments, by = "ID")

# Calculate cumulative water usage
alltogether <- alltogether %>%
  group_by(ID, Treatment, Species, date) %>%
  summarize(cumulative_water = sum(water_amount, na.rm = TRUE)) %>%
  ungroup()

# Summarize data by date and treatment
cumulative_usage <- alltogether %>%
  group_by(date, Treatment, Species) %>%
  summarize(total_water = sum(cumulative_water, na.rm = TRUE)) %>%
  ungroup()

# Create timeline plot
timeline_plot <- ggplot(cumulative_usage, aes(x = date, y = total_water, color = Treatment)) +
  geom_line() +
  facet_wrap(~Species, scales = "free_y") +
  theme_classic() +
  labs(title = "Water Usage Over Time by Treatment and Species",
       x = "Date",
       y = "Total Water Usage (g)",
       color = "Treatment") +
  scale_color_manual(values = c("control" = "deepskyblue", "drought" = "tomato1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(timeline_plot)

#Change to a per-plant water usage metric ####
library(dplyr)

# Ensure 'date' is included in the merge and calculations
alltogether <- days_watered_filtered %>%
  left_join(treatments, by = "ID") %>%
  group_by(ID, Treatment, Species, date) %>%
  summarize(cumulative_water = sum(water_amount, na.rm = TRUE), .groups = 'drop')

# Count the number of plants for each treatment on each date
plant_counts <- days_watered_filtered %>%
  left_join(treatments, by = "ID") %>%
  group_by(date, Treatment) %>%
  summarize(num_plants = n_distinct(ID), .groups = 'drop')

# Ensure 'date' and 'Treatment' are included in the plant_counts data
alltogether <- alltogether %>%
  left_join(plant_counts, by = c("date", "Treatment"))

# Calculate average water usage per plant
average_usage <- alltogether %>%
  group_by(date, Treatment, Species) %>%
  summarize(average_water = sum(cumulative_water, na.rm = TRUE) / sum(num_plants, na.rm = TRUE), .groups = 'drop')

print(average_usage)

# Create timeline plot with updated average water usage
timeline_plot <- ggplot(average_usage, aes(x = date, y = average_water, color = Treatment)) +
  geom_line() +
  facet_wrap(~Species, scales = "free_y") +
  scale_color_manual(values = c("control" = "deepskyblue", "drought" = "red")) +
  theme_classic() +
  labs(title = "Average Water Usage Over Time by Treatment and Species",
       x = "Date",
       y = "Average Water Usage per Plant (g)",
       color = "Treatment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(timeline_plot)



head(days_watered)





# 8/9 Update on Timeline Graph

require(tidyverse)
require(readxl)
require(cowplot)

# Read in the lysimetry data
lys_data <- read_csv("Lysimetry_data.csv")
lys_data <- lys_data[1:89,] # Removing blank lines

# Read in the treatment key and harvest days
treatments <- read_csv("Treatment_key.csv") %>%
  mutate(ID = Plant_ID)

harvest_days <- read_csv("Harvest_Days.csv")

# Reshape the lysimetry data
reshaped_data <- lys_data %>%
  pivot_longer(cols = -ID,
               names_to = c(".value", "date"),
               names_pattern = "(sample|Time)(.*)") %>%
  drop_na() %>%
  mutate(newtime = parse_date_time(Time, "mdy_HM"))

# Handling time format discrepancies
for (i in 1:nrow(reshaped_data)) {
  if (is.na(reshaped_data$newtime[i])) {
    reshaped_data$newtime[i] = parse_date_time(reshaped_data$Time[i], "mdy_HMS")
  }
}

# Merge reshaped data with harvest days (assuming 'Sample' is the ID in harvest_days)
reshaped_data <- left_join(reshaped_data, harvest_days, by = c("ID" = "Sample"))

# Convert LastDay1 and LastDay2 to datetime
reshaped_data <- reshaped_data %>%
  mutate(
    LastDay1 = as.POSIXct(LastDay1, format = "%m/%d/%Y %H:%M"),
    LastDay2 = as.POSIXct(LastDay2, format = "%m/%d/%Y %H:%M")
  )

# Filter out measurements after the plant's harvest date
reshaped_data <- reshaped_data %>%
  filter(newtime <= LastDay1 | is.na(LastDay1))

# Calculate transpiration rates over time for each ID
transpiration_data <- reshaped_data %>%
  arrange(ID, newtime) %>%
  group_by(ID) %>%
  mutate(
    time_diff = as.numeric(difftime(newtime, lag(newtime), units = "hours")),
    mass_diff = lag(sample) - sample,
    transpiration_rate = mass_diff / time_diff
  ) %>%
  ungroup()

# Combine with treatment data
alltogether <- left_join(transpiration_data, treatments)

# Update species factor levels
alltogether$Species <- factor(alltogether$Species,
                              levels = c("NM", "RP", "SP", "TC", "R+S", "S+T"))

# Create the timeline graph
timeline_plot <- ggplot(data = alltogether) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  geom_line(aes(x = newtime, y = transpiration_rate, color = Treatment, group = ID)) +
  facet_wrap(~ Species) +
  xlab("Date") + 
  ylab("Transpiration Rate (water loss g/hr)") +
  scale_color_manual(name = "Treatment", values = c("control" = "deepskyblue", "drought" = "red")) +
  labs(color = "Treatment") +
  geom_hline(yintercept = c(0.05, 0.1, 0.15, 0.2, 0.25), color = "grey", linetype = "solid") +
  ylim(0, 0.25)

print(timeline_plot)






### DAG's new efforts ####

#Use Harvest Days dates

require(tidyverse)
require(readxl)
require(cowplot)
library(lubridate)

# Read in the lysimetry data and harvest days
lysimetry_data <- read_csv("Lysimetry_data.csv")
treatments <- read_csv("Treatment_key.csv") %>%
  mutate(ID = Plant_ID)
harvest_days <- read_csv("Harvest_Days.csv")

# Correcting the date format for LastDay1 and LastDay2
reshaped_data$LastDay1 <- as_date(ymd_hms(reshaped_data$LastDay1, tz = "UTC", quiet = TRUE))
reshaped_data$LastDay2 <- as_date(ymd_hms(reshaped_data$LastDay2, tz = "UTC", quiet = TRUE))

# Replace the inconsistent time formats with correct ones
reshaped_data$Time <- gsub("(\\d{1,2}/\\d{1,2}/)\\d{4}", "\\1\\d{2}", reshaped_data$Time)

# Now parse the dates again
reshaped_data$newtime <- parse_date_time(reshaped_data$Time, orders = c("mdy_HM", "mdy_HMS"))


# Filtering the last two days before harvest
last_two_days_before_harvest <- reshaped_data %>%
  filter(newtime >= LastDay1 & newtime <= LastDay2)

# Reshape the lysimetry data
reshaped_data <- lysimetry_data %>%
  pivot_longer(cols = -ID,
               names_to = c(".value", "date"),
               names_pattern = "(sample|Time)(.*)") %>%
  drop_na() %>%
  mutate(newtime = parse_date_time(Time, "mdy_HM"))  # Parse time format

# Handling time format discrepancies (add 'HMS' parsing if necessary)
for (i in 1:nrow(reshaped_data)) {
  if (is.na(reshaped_data$newtime[i])) {
    reshaped_data$newtime[i] = parse_date_time(reshaped_data$Time[i], "mdy_HMS")
  }
}

# Merge reshaped data with harvest days (adjust the join to match column names)
reshaped_data <- left_join(reshaped_data, harvest_days, by = c("ID" = "Sample"))

# Convert LastDay1 and LastDay2 to datetime
reshaped_data <- reshaped_data %>%
  mutate(
    LastDay1 = as.POSIXct(LastDay1, format = "%m/%d/%Y"),
    LastDay2 = as.POSIXct(LastDay2, format = "%m/%d/%Y")
  )

# Filter the data to only include measurements taken on the last two days before harvest
last_two_days_before_harvest <- reshaped_data %>%
  group_by(ID) %>%
  filter(newtime %in% c(LastDay1, LastDay2)) %>%
  arrange(ID, desc(newtime)) %>%
  ungroup()

# Calculate hourly water loss
hourly_loss <- last_two_days_before_harvest %>%
  arrange(ID, desc(newtime)) %>%
  group_by(ID) %>%
  summarize(
    massdiff_g = last(sample) - first(sample),  # Calculate water loss
    timediff = as.numeric(difftime(first(newtime), last(newtime), units = "hours"))  # Time difference in hours
  ) %>%
  mutate(mass_per_hr = massdiff_g / timediff) %>%
  ungroup()

# Combine hourly water loss with treatment data
alltogether <- left_join(hourly_loss, treatments, by = "ID")

# Update species factor levels
alltogether$Species <- factor(alltogether$Species,
                              levels = c("NM", "RP", "SP", "TC", "R+S", "S+T"))

print(head(alltogether))


#Debugging
# Debug the parsing of 'Time' column
unique(reshaped_data$Time)  # Check the format of times

# Ensure that the date parsing matches the data
reshaped_data$newtime <- parse_date_time(reshaped_data$Time, orders = c("mdy_HM", "mdy_HMS"))

# Adjust filtering to allow for range matching
last_two_days_before_harvest <- reshaped_data %>%
  group_by(ID) %>%
  filter(newtime >= LastDay1 & newtime <= LastDay2) %>%
  arrange(ID, desc(newtime)) %>%
  ungroup()

print(head(last_two_days_before_harvest))  # Check filtered data


#Checking
# Print out some relevant details before the merge
print(head(harvest_days_data))  # Check the harvest data
print(unique(reshaped_data$ID))  # Make sure the IDs match

# Verify the filtering condition for last two days before harvest
print(reshaped_data$newtime)  # Ensure this column is correctly parsed and contains values




# Create the boxplot
testplot <- ggplot(data = alltogether) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  geom_boxplot(aes(x = Species, y = mass_per_hr, color = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x = Species, y = mass_per_hr, color = Treatment)) +
  xlab("Fungal Species") + 
  ylab("Transpiration Rate (water loss g/hr)") +
  scale_color_manual(name = "Treatment", values = c("control" = "deepskyblue", "drought" = "red")) +
  labs(color = "Treatment") +
  geom_hline(yintercept = c(0.05, 0.1, 0.15, 0.2, 0.25), color = "grey", linetype = "solid") +
  ylim(0, 0.25)

print(testplot)



### New code

# Load necessary libraries
library(tidyverse)
library(lubridate)

# Load the data
lys_data <- read_csv("Lysimetry_data.csv")
harvest_days <- read_csv("Harvest_Days.csv")

# View the structure of lys_data to identify date columns
str(lys_data)

# Reshape lysimetry data to long format
lys_data_long <- lys_data %>%
  pivot_longer(
    cols = starts_with("Time"),  # Adjust if necessary to match your date columns
    names_to = "Date",
    values_to = "Water_Usage"
  ) %>%
  mutate(Date = as.Date(str_extract(Date, "\\d{2}/\\d{2}/\\d{2}"), format = "%m/%d/%y"))

# Convert date columns in harvest_days to Date type
harvest_days <- harvest_days %>%
  mutate(LastDay1 = as.Date(LastDay1, format = "%m/%d/%Y"),
         LastDay2 = as.Date(LastDay2, format = "%m/%d/%Y"))

# Join harvest days with lysimetry data to exclude harvested plants
data_with_harvest <- lys_data_long %>%
  left_join(harvest_days, by = "Sample") %>%  # Ensure the join column is correct
  filter(Date <= LastDay2) %>%
  group_by(Sample) %>%
  filter(Date >= min(Date))  # Ensure we're including data up to LastDay2

# Identify days when plants were watered (assuming there's a column "Watered" indicating watering)
# You may need to adjust this if "Watered" column is named differently or doesn't exist
watered_days <- data_with_harvest %>%
  filter(Watered == 1)

# Create the timeline graph
mygraph <- ggplot(data_with_harvest, aes(x = Date, y = Water_Usage, color = Treatment, group = Sample)) +
  geom_line() +
  geom_point(data = watered_days, aes(x = Date, y = Water_Usage), color = "blue", shape = 17) +
  labs(title = "Water Usage Timeline by Treatment",
       x = "Date",
       y = "Water Usage (units)",
       color = "Treatment") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Treatment"))

# Save the plot
ggsave("Water_Usage_Timeline.png", plot = mygraph)




library(tidyverse)
library(lubridate)

# Reshape lysimetry data to long format
lys_data_long <- lys_data %>%
  # Gather sample columns into key-value pairs
  pivot_longer(
    cols = starts_with("sample"),
    names_to = "date",
    values_to = "sample_weight"
  ) %>%
  # Clean and process the date column
  mutate(
    # Extract the actual date from the sample name
    date = str_remove(date, "sample"),
    # Convert date to Date format
    date = mdy(date)
  ) %>%
  # Ensure that the time information is handled correctly
  # Extract time information from 'Water_Usage' column if it exists
  mutate(
    time = mdy_hm(Water_Usage),
    datetime = as_datetime(paste(date, format(time, "%H:%M")))
  ) %>%
  select(ID, datetime, sample_weight)

# Check the structure of the reshaped data
str(lys_data_long)

# View the first few rows to ensure it's reshaped correctly
head(lys_data_long)







#9/27/24 Timeline graph attempt ####

library(readxl) 
library(readr)  
library(tidyr)  
library(dplyr)    
library(ggplot2)  

# Step 1: Import data
lys_up927 <- read_excel("Lysimetry_updated.xlsx")
treatment_data <- read_csv("Treatment_key.csv")

# Step 3: Reshape the sample data
lys_samples_long <- lys_up927 %>%
  pivot_longer(
    cols = starts_with("sample"),
    names_to = c("Sample", "Date"), # Create separate columns
    names_sep = "(?<=sample)(?=\\d)", # Split names at "sample" and number
    values_to = "Transpiration_Rate" # Assign values to this column
  ) %>%
  select(ID, Date, Transpiration_Rate)  # Select relevant columns

# Step 4: Reshape the time data
lys_times_long <- lys_up927 %>%
  pivot_longer(
    cols = starts_with("Time"),
    names_to = c("TimeSample", "Date"), # Create separate columns
    names_sep = "(?<=Time)(?=\\d)", # Split names at "Time" and number
    values_to = "DateTime" # Assign values to this column
  ) %>%
  select(ID, Date, DateTime)  # Select relevant columns

# Step 5: Join the reshaped samples and times based on ID and Date
lys_long_up <- lys_samples_long %>%
  inner_join(lys_times_long, by = c("ID", "Date")) %>%
  select(ID, DateTime, Transpiration_Rate) %>%  # Select relevant columns
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))  # Ensure DateTime is in the correct format

# Step 6: Merge with treatment data
lys_long_up <- lys_long_up %>%
  left_join(treatment_data, by = c("ID" = "Plant_ID"))  # Adjust the join if 'ID' is named differently in treatment_data

# Step 7: Create the plot
ggplot(lys_long_up, aes(x = DateTime, y = Transpiration_Rate, color = Treatment)) +
  geom_line() +  # Line for each plant
  geom_point() +  # Points for each measurement
  labs(title = "Transpiration Rates Over Time by Treatment and Fungal Species",
       x = "Date and Time",
       y = "Transpiration Rate (g/day)",
       color = "Treatment") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +  # Format x-axis to show months and years
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  facet_wrap(~ Species)  # Create separate plots for each species
