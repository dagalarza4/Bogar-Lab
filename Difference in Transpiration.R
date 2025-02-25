#### Difference in Transpiration During Drought ####
library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)

#### Difference in Transpiration During Drought ####
# Load the data from the 'Lysimetry+Info.xlsx' file
day0_6_diff <- read_xlsx("Lysimetry+Info.xlsx", sheet = "Harvest Days")
transpiration_rates <- read_xlsx("Lysimetry+Info.xlsx", sheet = "Transpiration Rates")

# Data conversion for Day0 and HarvestDate
day0_6_diff$Day0 <- as.Date(day0_6_diff$Day0, format = "%m/%d/%y")
day0_6_diff$HarvestDate <- as.Date(day0_6_diff$HarvestDate, format = "%m/%d/%y")

# Merging the data
difference_data <- day0_6_diff %>%
  left_join(transpiration_rates, by = c("ID" = "Plant_ID"))

# Filtering to remove rows with NA for Day0 or HarvestDate
difference_data <- difference_data %>%
  filter(!is.na(Day0) & !is.na(HarvestDate))

# Group-by Operations to calculate the transpiration differences
difference_data <- difference_data %>%
  group_by(ID) %>%
  mutate(
    Transpiration_at_Day0 = Day0_Transpiration,
    Transpiration_at_Harvest = Harvest_Transpiration,
    Difference_g_hr = Transpiration_at_Harvest - Transpiration_at_Day0
  ) %>%
  ungroup()

# Create a new factor variable combining Species and Treatment for the x-axis
difference_data$Species_Treatment <- factor(paste(difference_data$Species, difference_data$Treatment, sep = "_"),
                                            levels = c("NM_control", "NM_drought",
                                                       "SP_control", "SP_drought",
                                                       "RP_control", "RP_drought",
                                                       "TC_control", "TC_drought",
                                                       "S+T_control", "S+T_drought",
                                                       "R+S_control", "R+S_drought"))

# Function to lighten a color
adjust_color <- function(col, factor = 0.2, lighten = TRUE) {
  col_rgb <- col2rgb(col)
  if (lighten) {
    # Blend the color with white to lighten it
    white_rgb <- c(255, 255, 255)
    blended_rgb <- (1 - factor) * col_rgb + factor * white_rgb
  } else {
    # Return the original color if not lightening
    blended_rgb <- col_rgb
  }
  rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue = 255)
}

# Apply lightened colors for drought plants only
lightened_colors <- sapply(species_colors, function(col) adjust_color(col, factor = 0.4, lighten = TRUE))

# Keep control colors the same as original
control_colors <- species_colors

# Combine control and lightened drought colors
adjusted_colors <- c(
  setNames(control_colors, paste(names(species_colors), "control", sep = "_")),
  setNames(lightened_colors, paste(names(species_colors), "drought", sep = "_"))
)


#Graph
plot_day6_2 <- ggplot(difference_data %>% filter(!is.na(Harvest_Transpiration)), aes(x = Species, y = Difference_g_hr, linetype = Treatment)) +
  geom_boxplot(
    aes(fill = Species_Treatment, linetype = Treatment),  # Fill by combined Species and Treatment
    outlier.shape = NA  # Boxplot without outlier points
  ) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7) +  # Add jittered points for individual data
  scale_fill_manual(
    values = adjusted_colors,  # Use the adjusted species colors for fills
    guide = "none"  # Remove the species color legend
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed"),  # Solid for control, dashed for drought
    labels = c("Control", "Drought"),  # Custom linetype labels
    name = "Treatment"  # Title for the linetype legend
  ) +
  scale_x_discrete(
    limits = c("NM", "SP", "RP", "TC", "S+T", "R+S")  # Species order
  ) +  # Set the order of species on the x-axis
  scale_y_continuous(limits = c(-0.25, 0.25)) +  # Set y-axis limits from 0 to 0.2
  labs(
    title = "Difference in Transpiration Rates Throughout Drought",
    x = "Species",
    y = "Difference in Transpiration Rate (g/hr)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "bottom"  # Move the legend to the top
  )
print(plot_day6_2)






#Only Drought Graph
# Graph
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

# Jitter points
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








# Step 1: Add 'Treatment' column for control and drought plants
difference_data <- difference_data %>%
  mutate(Treatment = ifelse(grepl("control", ID), "Control", "Drought"))

# Step 2: Create a new factor variable combining Species and Treatment for the x-axis
difference_data$Species_Treatment <- factor(paste(difference_data$Species, difference_data$Treatment, sep = "_"),
                                            levels = c("NM_Control", "NM_Drought", "SP_Control", "SP_Drought", 
                                                       "RP_Control", "RP_Drought", "TC_Control", "TC_Drought", 
                                                       "S+T_Control", "S+T_Drought", "R+S_Control", "R+S_Drought"))

# Step 3: Apply colors for species (control plants in solid colors, drought plants in lighter/dashed colors)
species_colors <- c("NM" = "moccasin", "SP" = "tan3", "RP" = "salmon4", "TC" = "gold2", 
                    "S+T" = "darkgoldenrod2", "R+S" = "orangered4")

# Adjust control and drought colors
lightened_colors <- sapply(species_colors, function(col) adjust_color(col, factor = 0.4, lighten = TRUE))
control_colors <- species_colors
adjusted_colors <- c(setNames(control_colors, paste(names(species_colors), "Control", sep = "_")),
                     setNames(lightened_colors, paste(names(species_colors), "Drought", sep = "_")))

# Step 4: Create the plot combining both control and drought plants with solid and dashed lines
ggplot(difference_data, aes(x = Species_Treatment, y = Difference_g_hr, fill = Species_Treatment, linetype = Treatment)) +
  geom_boxplot(outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(width = 0.2, size = 2, shape = 16, color = "black") +  # Jittered points
  scale_fill_manual(values = adjusted_colors) +  # Adjusted colors for control and drought
  scale_linetype_manual(values = c("solid", "dashed"),  # Solid for control, dashed for drought
                        labels = c("Control", "Drought"),
                        name = "Treatment") +
  scale_y_continuous(limits = c(-0.2, 0), name = "Difference in Transpiration Rate (g/hr)") +
  scale_x_discrete(labels = c("NM_Control", "NM_Drought", "SP_Control", "SP_Drought", 
                              "RP_Control", "RP_Drought", "TC_Control", "TC_Drought", 
                              "S+T_Control", "S+T_Drought", "R+S_Control", "R+S_Drought")) +  # X-axis labels
  labs(title = "Transpiration Rate Difference from Drought Day 0 to Day 6", x = "Species and Treatment") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to the bottom











#### New Difference in Transpiration Rate Graph- Possibly to Replace Figure 1 ####
day0_6_diff <- read_xlsx("Lysimetry+Info.xlsx", sheet = "Harvest Days")
transpiration_rates <- read_xlsx("Lysimetry+Info.xlsx", sheet = "Transpiration Rates")

# Data conversion for Day0 and HarvestDate
day0_6_diff$Day0 <- as.Date(day0_6_diff$Day0, format = "%m/%d/%y")
day0_6_diff$HarvestDate <- as.Date(day0_6_diff$HarvestDate, format = "%m/%d/%y")

# Merging the data
difference_data <- day0_6_diff %>%
  left_join(transpiration_rates, by = c("ID" = "Plant_ID"))

# Filtering to remove rows with NA for Day0 or HarvestDate
difference_data <- difference_data %>%
  filter(!is.na(Day0) & !is.na(HarvestDate))

# Group-by Operations to calculate the transpiration differences
difference_data <- difference_data %>%
  group_by(ID) %>%
  mutate(
    Transpiration_at_Day0 = Day0_Transpiration,
    Transpiration_at_Harvest = Harvest_Transpiration,
    Difference_g_hr = Transpiration_at_Harvest - Transpiration_at_Day0
  ) %>%
  ungroup()

# Create a new factor variable combining Species and Treatment for the x-axis
difference_data$Species_Treatment <- factor(paste(difference_data$Species, difference_data$Treatment, sep = "_"),
                                            levels = c("NM_control", "NM_drought",
                                                       "SP_control", "SP_drought",
                                                       "RP_control", "RP_drought",
                                                       "TC_control", "TC_drought",
                                                       "S+T_control", "S+T_drought",
                                                       "R+S_control", "R+S_drought"))

# Function to lighten a color
adjust_color <- function(col, factor = 0.2, lighten = TRUE) {
  col_rgb <- col2rgb(col)
  if (lighten) {
    # Blend the color with white to lighten it
    white_rgb <- c(255, 255, 255)
    blended_rgb <- (1 - factor) * col_rgb + factor * white_rgb
  } else {
    # Return the original color if not lightening
    blended_rgb <- col_rgb
  }
  rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue = 255)
}

# Apply lightened colors for drought plants only
lightened_colors <- sapply(species_colors, function(col) adjust_color(col, factor = 0.4, lighten = TRUE))

# Keep control colors the same as original
control_colors <- species_colors

# Combine control and lightened drought colors
adjusted_colors <- c(
  setNames(control_colors, paste(names(species_colors), "control", sep = "_")),
  setNames(lightened_colors, paste(names(species_colors), "drought", sep = "_"))
)


#Graph
plot_day6_2 <- ggplot(difference_data %>% filter(!is.na(Harvest_Transpiration)), aes(x = Species, y = Difference_g_hr, linetype = Treatment)) +
  geom_boxplot(
    aes(fill = Species_Treatment, linetype = Treatment),  # Fill by combined Species and Treatment
    outlier.shape = NA  # Boxplot without outlier points
  ) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7) +  # Add jittered points for individual data
  scale_fill_manual(
    values = adjusted_colors,  # Use the adjusted species colors for fills
    guide = "none"  # Remove the species color legend
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed"),  # Solid for control, dashed for drought
    labels = c("Control", "Drought"),  # Custom linetype labels
    name = "Treatment"  # Title for the linetype legend
  ) +
  scale_x_discrete(
    limits = c("NM", "SP", "RP", "TC", "S+T", "R+S")  # Species order
  ) +  # Set the order of species on the x-axis
  scale_y_continuous(limits = c(-0.25, 0.25)) +  # Set y-axis limits from 0 to 0.2
  labs(
    title = "b) Transpiration Rate on Day 6 of Drought",
    x = "Species",
    y = "Transpiration Rate (g/hr)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "bottom"  # Move the legend to the top
  )
print(plot_day6_2)
