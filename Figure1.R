#### Fungi Impact Plant Water Use, Measured by Transpiration Rate Graph ####
# Panel A: X axis species, Y axis transpiration rate day 0 of drydown (final measurement before dry down, right after water is applied)

# Most of the needed data structures will be in DFD Data.R
require(tidyverse)

# Define species colors
species_colors <- c(
  "NM" = "moccasin",
  "SP" = "tan3",
  "RP" = "salmon4",
  "TC" = "gold2",
  "S+T" = "darkgoldenrod2",
  "R+S" = "orangered4"
)

# Prepare the data for drought plants for Day 0
plot_day0_data <- filtered_data %>%
  filter(Treatment == "drought") %>%  # Filter for drought plants only
  left_join(harvest_days, by = "Plant_ID") %>%  # Join harvest days
  filter(!is.na(Day0))  # Exclude rows where Day0 is NA

# Set the order of the species factor levels
plot_day0_data$Species <- factor(plot_day0_data$Species, levels = c("NM", "SP", "RP", "TC", "S+T", "R+S"))

# Create the boxplot for Day 0 with jitter points
plot_day0 <- ggplot(plot_day0_data, aes(x = Species, y = Transpiration_rate_value, fill = Species)) +
  geom_boxplot(outlier.shape = NA) +  # Create the boxplot without outlier points
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7) +  # Add jittered points for individual data
  scale_fill_manual(values = species_colors) +  # Apply custom colors
  scale_y_continuous(limits = c(0, 0.2)) +  # Set y-axis limits from 0 to 0.2
  labs(
    title = "a) Transpiration Rate Day 0 of Drought",
    x = "Species",
    y = "Transpiration Rate (g/hr)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "none"  # Remove legend
  )
print(plot_day0)

# Create the boxplot for Day 0 without jitter points
plot_day0 <- ggplot(plot_day0_data, aes(x = Species, y = Transpiration_rate_value, fill = Species)) +
  geom_boxplot(outlier.shape = NA) +  # Create the boxplot without outlier points
  scale_fill_manual(values = species_colors) +  # Apply custom colors
  scale_y_continuous(limits = c(0, 0.2)) +  # Set y-axis limits from 0 to 0.2
  labs(
    title = "a) Transpiration Rate Day 0 of Drought",
    x = "Species",
    y = "Transpiration Rate (g/hr)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "none"  # Remove legend
  )
print(plot_day0)


library(patchwork)

# Panel B: Transpiration Rate on Harvest Day (Day 6 of Drought)
plot_day6_2 <- ggplot(plot_day6_data2, aes(x = Species, y = Transpiration_rate_value)) +
  geom_boxplot(
    aes(fill = Species_T        reatment, linetype = Treatment),  # Fill by combined Species and Treatment
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
  scale_y_continuous(limits = c(0, 0.2)) +  # Set y-axis limits from 0 to 0.2
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

# Combine plots
combined_plot <- plot_day0 / plot_day6_2  # Arrange Day 0 on top and Day 6 below

# Display the combined plot
print(combined_plot)





#### ANOVA for Transpiration Rates ####

#ANOVA for Transpiration Rate on Day 0 by Species (Significant)
transpiration_day0_anova <- aov(Transpiration_rate_value ~ Species, data = plot_day0_data)
summary(transpiration_day0_anova)

# Perform Tukey's HSD test (No one species is significantly different)
tukey_result <- TukeyHSD(transpiration_day0_anova)
summary(tukey_result)
tukey_result 


# ANOVA for Transpiration Rate on Day 6 by Species (Not significant)
transpiration_day6_anova <- aov(Transpiration_rate_value ~ Species, data = plot_day6_data)
summary(transpiration_day6_anova)


