

#8/9 Update

library(tidyverse)
library(readxl)
library(cowplot)

excel_data <- read_excel("Lysimetry Data for R.xlsx", sheet = 1)
write.csv(excel_data, "Lysimetry_data_updated.csv", row.names = FALSE)


# Read in the lysimetry data
lysimetry_data_updated <- read_csv("Lysimetry_data.csv")
lysimetry_data_updated <- lysimetry_data_updated[1:89,] # Removing blank lines

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
