#### Difference in Transpiration During Drought ####

#Data conversion
difference_data$Day0 <- as.Date(difference_data$Day0, format = "%m/%d/%y")
difference_data$HarvestDate <- as.Date(difference_data$HarvestDate, format = "%m/%d/%y")

#Filtering
difference_data <- difference_data %>%
  filter(!is.na(Day0) & !is.na(HarvestDate))

#Group-by Operations
difference_data <- difference_data %>%
  group_by(Plant_ID) %>%
  mutate(
    Transpiration_at_Day0 = Transpiration_rate_value[Date == Day0],
    Transpiration_at_Harvest = Transpiration_rate_value[Date == HarvestDate],
    Difference_g_hr = Transpiration_at_Harvest - Transpiration_at_Day0
  ) %>%
  ungroup()

#Graph
library(ggplot2)
library(forcats)

species_colors <- c(
  "NM" = "moccasin",
  "SP" = "tan3",
  "RP" = "salmon4",
  "TC" = "gold2",
  "S+T" = "darkgoldenrod2",
  "R+S" = "orangered4"
)

# Reorder the 'Species' factor levels
difference_data$Species <- fct_relevel(difference_data$Species, "NM", "SP", "RP", "TC", "S+T", "R+S")

# Create the plot
ggplot(difference_data, aes(x = Species, y = Difference_g_hr, fill = Species)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = species_colors) +
  scale_y_continuous(
    limits = c(-0.2, 0),
    name = "Difference in Transpiration Rate (g/hr)"
  ) +
  labs(
    title = "Transpiration Rate Difference from Drought Day 0 to Day 6",
    x = "Species"
  ) +
  theme_minimal() +
  guides(fill = "none")



#Jitter points
ggplot(difference_data, aes(x = Species, y = Difference_g_hr, fill = Species)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, shape = 16, color = "black") + # Add jittered points
  scale_fill_manual(values = species_colors) +
  scale_y_continuous(limits = c(-0.2, 0), name = "Difference in Transpiration Rate (g/hr)") +
  labs(
    title = "Transpiration Rate Difference from Drought Day 0 to Day 6",
    x = "Species"
  ) +
  theme_minimal() +
  guides(fill = "none")



#### Statistics ####
# Fit the ANOVA model (Significant)
anova_model <- aov(Mean_Difference_g_hr ~ Species, data = difference_data_summary)
summary(anova_model)

# Perform Tukey's Honest Significant Difference test (No one species is significantly different)
tukey_results <- TukeyHSD(anova_model)
print(tukey_results)

