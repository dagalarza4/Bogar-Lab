#### Percent Colonization and Biomass 2 Panel graph ####

library(ggplot2)
library(patchwork)

# Define species colors
species_colors <- c(
  "NM" = "moccasin",
  "SP" = "tan3",
  "RP" = "salmon4",
  "TC" = "gold2",
  "S+T" = "darkgoldenrod2",
  "R+S" = "orangered4"
)

# Panel A: Percent Colonization by Species (Boxplot with filled boxes)
panel_a <- ggplot(subset(Harvest_Data_Clean), 
                  aes(x = factor(Species, levels = species_order), y = perccol, fill = Species)) +
  geom_boxplot(outlier.color = "black", outlier.size = 1.5) +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "a) Percent Colonization by Species", 
    x = "Species", 
    y = "Colonization (%)"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Panel B: Total Dry Biomass by Species (Boxplot with filled boxes, excluding NM)
panel_b <- ggplot(subset(Harvest_Data_Clean), 
                  aes(x = factor(Species, levels = species_order), y = total_dry_mass, fill = Species)) +
  geom_boxplot(outlier.color = "black", outlier.size = 1.5) +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "b) Total Dry Biomass by Species", 
    x = "Species", 
    y = "Total Dry Biomass (g)"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Combine the two panels vertically
combined_plot1 <- panel_a / panel_b

# Display the combined plot
print(combined_plot1)



#### Statistics ####

#Percent Colonization by Species (Significant)
Percent_Col_by_Species <- aov(`%_colonization` ~ Species, data = Harvest_Data_Clean)
summary(Percent_Col_by_Species)

# Perform Tukey's HSD test for Percent Colonization by Species
tukey_colonization <- TukeyHSD(Percent_Col_by_Species)
print(tukey_colonization)



#Total Dry Biomass by Treatment (not significant)
totalmassanovabytreatment = aov(total_dry_mass ~ Treatment, data = Harvest_Data_Clean)
summary(totalmassanovabytreatment)

# Perform Tukey's HSD test for Total Dry Biomass by Treatment
tukey_biomass_treatment <- TukeyHSD(totalmassanovabytreatment)
print(tukey_biomass_treatment)



#Total Dry Biomass by Species and Treatment (not significant)
totalmassanovabyST = aov(total_dry_mass ~ Species * Treatment, data = Harvest_Data_Clean)
summary(totalmassanovabyST)

# Perform Tukey's HSD test for Total Dry Biomass by Species and Treatment
tukey_biomass_ST <- TukeyHSD(totalmassanovabyST)
print(tukey_biomass_ST)


#### Tukey Graph ####
# Define species colors
species_colors <- c(
  "NM" = "moccasin",
  "SP" = "tan3",
  "RP" = "salmon4",
  "TC" = "gold2",
  "S+T" = "darkgoldenrod2",
  "R+S" = "orangered4"
)

# Add Tukey HSD letters
tukey_letters <- data.frame(
  Species = c("NM", "SP", "RP", "TC", "S+T", "R+S"),
  Label = c("b", "b", "a", "b", "b", "a") # Assign letters based on Tukey results
)

# Panel A: Percent Colonization by Species (Boxplot with filled boxes and letters)
panel_a_2 <- ggplot(subset(Harvest_Data_Clean), 
                  aes(x = factor(Species, levels = species_order), y = perccol, fill = Species)) +
  geom_boxplot(outlier.color = "black", outlier.size = 1.5) +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "a) Percent Colonization by Species", 
    x = "Species", 
    y = "Colonization (%)"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 80)) +
  # Add letters above the boxes
  geom_text(data = tukey_letters, 
            aes(x = Species, y = 75, label = Label), # Adjust 'y' to position the letters above the boxes
            inherit.aes = FALSE, size = 5)
print(panel_a_2)

# Panel B: Total Dry Biomass by Species (Boxplot with filled boxes, excluding NM)
panel_b <- ggplot(subset(Harvest_Data_Clean), 
                  aes(x = factor(Species, levels = species_order), y = total_dry_mass, fill = Species)) +
  geom_boxplot(outlier.color = "black", outlier.size = 1.5) +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "b) Total Dry Biomass by Species", 
    x = "Species", 
    y = "Total Dry Biomass (g)"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Combine the two panels vertically
combined_plot1_2 <- panel_a_2 / panel_b

# Display the combined plot
print(combined_plot1_2)
