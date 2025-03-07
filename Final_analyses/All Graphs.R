library(ggplot2)
library(patchwork)
library(readxl)
library(readr)
library(dplyr)
library(forcats)
library(cowplot)

#### Open Datasets and Extra ####

#NOTE: Make sure to load all the DFD Data Tidying script FIRST- data preprocessing required

load("Harvest_Data_LB.RData")

lysimetry_data <- read_csv("lysimetry_data_for_analysis.csv")

treatments = read_csv("Treatment_key.csv") %>%
  mutate(ID = Plant_ID)

# Define species colors
species_colors <- c(
  "NM" = "moccasin",
  "SP" = "tan3",
  "RP" = "salmon4",
  "TC" = "gold2",
  "S+T" = "darkgoldenrod2",
  "R+S" = "orangered4")

# Define the desired order of species
species_order <- c("NM", "SP", "RP", "TC", "S+T", "R+S")

#### Percent Colonization and Biomass 2-Panel Graph - Figure 1 ####

# Add Tukey HSD letters- already calculated on 'Stats.R'
tukey_letters <- data.frame(
  Species = c("NM", "SP", "RP", "TC", "S+T", "R+S"),
  Label = c("b", "b", "a", "b", "b", "a")
)

# Panel A: Percent Colonization by Species
perccol_graph <- ggplot(subset(Harvest_Data_LB), 
                        aes(x = factor(Species, levels = species_order), y = perccol, fill = Species)) + 
  geom_boxplot(outlier.color = "black", outlier.size = 1.5,
               position = position_dodge(0.9)) +  # Adjust position of boxplots
  scale_fill_manual(values = species_colors) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.15), 
             size = 1.5, alpha = 0.7) +  # Add jitter points with alignment
  labs(
    title = "a) Percent Colonization by Species", 
    x = "Species", 
    y = "Colonization (%)"
  ) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  coord_cartesian(ylim = c(0, 80)) + 
  geom_text(data = tukey_letters, 
            aes(x = Species, y = 75, label = Label),
            inherit.aes = FALSE, size = 5)

# Panel B: Total Dry Biomass by Species
total_dry_biomass_graph <- ggplot(subset(Harvest_Data_LB), 
                                  aes(x = factor(Species, levels = species_order), y = total_dry_mass, fill = Species)) + 
  geom_boxplot(outlier.color = "black", outlier.size = 1.5,
               position = position_dodge(0.9)) +  # Adjust position of boxplots
  scale_fill_manual(values = species_colors) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.15), 
             size = 1.5, alpha = 0.7) +  # Add jitter points with alignment
  labs(
    title = "b) Total Dry Biomass by Species", 
    x = "Species", 
    y = "Total Dry Biomass (g)"
  ) + 
  theme_bw() + 
  theme(legend.position = "none")

# Combine the two panels vertically
combined_perccol_biomass_graph <- perccol_graph / total_dry_biomass_graph

# Display the combined plot
print(combined_perccol_biomass_graph)

save_plot("Colonization_biomass_figure.pdf", combined_perccol_biomass_graph, base_height = 6, base_aspect_ratio = 1)



# ###### SKIP THIS #####
# #### Difference in Transpiration During Drought ####
# 
# # Data conversion for Day0 and HarvestDate
# lysimetry_data$Day0 <- as.Date(lysimetry_data$Day0, format = "%m/%d/%y")
# lysimetry_data$HarvestDate <- as.Date(lysimetry_data$HarvestDate, format = "%m/%d/%y")
# 
# # Merging the data (we assume 'lysimetry_data' already contains all the necessary columns, so no need for a separate merge)
# difference_data <- lysimetry_data %>%
#   filter(!is.na(Day0) & !is.na(HarvestDate))  # Remove rows with NA for Day0 or HarvestDate
# 
# # Group-by Operations to calculate the transpiration differences
# difference_data <- difference_data %>%
#   group_by(ID) %>%
#   mutate(
#     Transpiration_at_Day0 = Transpiration_Rate_Value[Dates == Day0],
#     Transpiration_at_Harvest = Transpiration_Rate_Value[Dates == HarvestDate],
#     Difference_g_hr = Transpiration_at_Harvest - Transpiration_at_Day0
#   ) %>%
#   ungroup()
# 
# # Create a new factor variable combining Species and Treatment for the x-axis
# difference_data$Species_Treatment <- factor(paste(difference_data$Species, difference_data$Treatment, sep = "_"),
#                                             levels = c("NM_control", "NM_drought",
#                                                        "SP_control", "SP_drought",
#                                                        "RP_control", "RP_drought",
#                                                        "TC_control", "TC_drought",
#                                                        "S+T_control", "S+T_drought",
#                                                        "R+S_control", "R+S_drought"))
# 
# # Function to lighten a color
# adjust_color <- function(col, factor = 0.2, lighten = TRUE) {
#   col_rgb <- col2rgb(col)
#   if (lighten) {
#     # Blend the color with white to lighten it
#     white_rgb <- c(255, 255, 255)
#     blended_rgb <- (1 - factor) * col_rgb + factor * white_rgb
#   } else {
#     # Return the original color if not lightening
#     blended_rgb <- col_rgb
#   }
#   rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue = 255)
# }
# 
# # Apply lightened colors for drought plants only
# lightened_colors <- sapply(species_colors, function(col) adjust_color(col, factor = 0.4, lighten = TRUE))
# 
# # Keep control colors the same as original
# control_colors <- species_colors
# 
# # Combine control and lightened drought colors
# adjusted_colors <- c(
#   setNames(control_colors, paste(names(species_colors), "control", sep = "_")),
#   setNames(lightened_colors, paste(names(species_colors), "drought", sep = "_"))
# )
# 
# # Graph
# plot_day6_2 <- ggplot(difference_data %>% filter(!is.na(Transpiration_at_Harvest)), aes(x = Species_Treatment, y = Difference_g_hr)) +
#   # Create boxplots without outliers
#   geom_boxplot(
#     aes(fill = Species_Treatment, linetype = Treatment),  # Fill by combined Species and Treatment
#     outlier.shape = NA,  # Boxplot without outlier points
#     position = position_dodge(0.8)  # Adjust boxplots position for dodging
#   ) + 
#   # Add jittered points to align them with the boxplot (dodging them within the species_treatment)
#   geom_point(
#     aes(color = Species_Treatment),  # Align points with the Species_Treatment
#     position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),  # Jitter and dodge to align points with boxplots
#     size = 2,  # Adjust point size as needed
#     alpha = 0.7  # Adjust point transparency
#   ) +
#   # Custom colors for species and treatments
#   scale_fill_manual(
#     values = adjusted_colors,  # Use the adjusted species colors for fills
#     guide = "none"  # Remove the species color legend
#   ) + 
#   scale_color_manual(
#     values = adjusted_colors  # Ensure the dots follow the same color scheme
#   ) + 
#   scale_linetype_manual(
#     values = c("solid", "dashed"),  # Solid for control, dashed for drought
#     labels = c("Control", "Drought"),  # Custom linetype labels
#     name = "Treatment"  # Title for the linetype legend
#   ) + 
#   # Set x-axis limits to control the species order
#   scale_x_discrete(
#     limits = c("NM_control", "NM_drought", "SP_control", "SP_drought", 
#                "RP_control", "RP_drought", "TC_control", "TC_drought", 
#                "S+T_control", "S+T_drought", "R+S_control", "R+S_drought")  # Species_Treatment order
#   ) +  
#   scale_y_continuous(limits = c(-0.25, 0.25)) +  # Set y-axis limits from -0.25 to 0.25
#   labs(
#     title = "Difference in Transpiration Rates Throughout Drought",
#     x = "Species_Treatment",
#     y = "Difference in Transpiration Rate (g/hr)"
#   ) + 
#   theme_minimal() + 
#   theme(
#     text = element_text(size = 14),
#     legend.position = "bottom"  # Move the legend to the bottom
#   )
# 
# # Display the plot
# print(plot_day6_2)
# ###### END SKIP THIS #####



#### Supplemental Graphs ####

# Root:Shoot by Species and Treatment
Harvest_Data_LB$Root_to_Shoot_Ratio <- Harvest_Data_LB$Root_DW / Harvest_Data_LB$Shoot_DW

Root_Shoot_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Root_to_Shoot_Ratio, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Root to Shoot Ratio By Species and Treatment", x = "Species", y = "Root to Shoot Ratio") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Root_Shoot_ST_graph)

# Root DW by Species and Treatment
Root_DW_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Root_DW, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Root Dry Weight By Species and Treatment", x = "Species", y = "Root Dry Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Root_DW_ST_graph)

# Root FW by Species and Treatment
Root_FW_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Root_FW_before, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Root Fresh Weight By Species and Treatment", x = "Species", y = "Root Fresh Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Root_FW_ST_graph)

# Shoot DW by Species and Treatment
Shoot_DW_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Shoot_DW, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Dry Weight By Species and Treatment", x = "Species", y = "Shoot Dry Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Shoot_DW_ST_graph)

# Shoot FW by Species and Treatment
Shoot_FW_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Shoot_FW_before, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Fresh Weight By Species and Treatment", x = "Species", y = "Shoot Fresh Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Shoot_FW_ST_graph)

# Root % MC by Species and Treatment
Root_MC_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Root_%_MC`, fill = Treatment)) + 
  geom_boxplot() + 
  ylim(65, NA) +
  labs(title = "Root Moisture Content By Species and Treatment", x = "Species", y = "Root Moisture Content (%)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Root_MC_ST_graph)

# Shoot % MC by Species and Treatment
Shoot_MC_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Shoot_%_MC`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Moisture Content By Species and Treatment", x = "Species", y = "Shoot Moisture Content (%)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Shoot_MC_ST_graph)

# Stem Diameter by Species and Treatment
Stem_diameter_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Stem_diameter`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Stem Diameter By Species and Treatment", x = "Species", y = "Stem Diameter (cm)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Stem_diameter_ST_graph)

# Final Weight by Species and Treatment
Harvest_Data_LB$Final_weight <- as.numeric(Harvest_Data_LB$Final_weight)

Harvest_Data_LB <- Harvest_Data_LB[!is.na(Harvest_Data_LB$Final_weight), ]

Final_weight_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Final_weight`, fill = Treatment)) + 
  geom_boxplot() + 
  ylim(NA, 90) +
  labs(title = "Final Total Weight By Species and Treatment", x = "Species", y = "Final Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Final_weight_ST_graph)

# Leaf SA by Species and Treatment
Leaf_SA_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Av_Needle_SA`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Leaf Surface Area By Species and Treatment", x = "Species", y = "Leaf Surface Area (cm)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Leaf_SA_ST_graph)

# % Colonization by Total Dry Biomass (without values = 0)
Perccol_TDB_graph <- ggplot(
  subset(Harvest_Data_LB, Species != "NM" & perccol != 0), 
  aes(x = perccol, y = total_dry_mass, color = factor(Species, levels = species_order))
) +   
  geom_point() +   
  geom_smooth(method = "lm", se = FALSE) +   
  scale_color_manual(values = species_colors) +  # Apply custom colors
  labs(
    title = "Percent Colonization by Total Dry Biomass",
    x = "Colonization (%)",
    y = "Total Dry Biomass (g)",
    color = "Species"  # Rename the legend title
  ) + 
  theme_bw()  
print(Perccol_TDB_graph)

# Fungi Impact Plant Water Use, Measured by Transpiration Rate Graph

# Panel A: X axis species, Y axis transpiration rate on Day 0 of drydown (final measurement before drydown, right after water is applied)

# Convert Species and Treatment to a factor with the specified order
lysimetry_data$Species <- factor(lysimetry_data$Species, levels = species_order)
lysimetry_data$Treatment <- factor(lysimetry_data$Treatment, levels = c("control", "drought"))

# Create a combined Species_Treatment variable for distinct coloring
lysimetry_data <- lysimetry_data %>%
  mutate(Species_Treatment = paste(Species, Treatment, sep = "_"))

# Day 0 Graph (Replace "Day0_Transpiration" with corresponding column in new data)
plot_day0 <- ggplot(lysimetry_data %>% filter(Treatment == "drought", !is.na(Transpiration_Rate_Value)), 
                    aes(x = Species, y = Transpiration_Rate_Value, fill = Species)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7) + 
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  labs(
    title = "a) Transpiration Rate Day 0 of Drought",
    x = "Species",
    y = "Transpiration Rate (g/hr)"
  ) + 
  theme_minimal() + 
  theme(
    text = element_text(size = 14),
    legend.position = "none"
  )
print(plot_day0)


# Panel B: Transpiration Rate on Harvest Day (Day 6 of Drought)

# Create a new factor variable combining Species and Treatment for the x-axis
lysimetry_data$Species_Treatment <- factor(paste(lysimetry_data$Species, lysimetry_data$Treatment, sep = "_"),
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

# Panel B: Transpiration Rate on Harvest Day (Day 6 of Drought)
plot_day6_2 <- ggplot(lysimetry_data %>% filter(!is.na(Transpiration_Rate_Value)), 
                      aes(x = Species, y = Transpiration_Rate_Value, linetype = Treatment)) +
  geom_boxplot(
    aes(fill = Species_Treatment, linetype = Treatment),  # Fill by combined Species and Treatment
    outlier.shape = NA  # Boxplot without outlier points
  ) + 
  geom_point(position = position_jitterdodge(jitter.width = 1),
                 aes(x = Species, 
                     y = Transpiration_Rate_Value,
                     fill = Species_Treatment,
                     alpha = 0.7)) +
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
combined_plot <- plot_day0 / plot_day6_2

# Display the combined plot
print(combined_plot)

save_plot("Transpiration_rate_figure.pdf", combined_plot, base_height = 6, base_aspect_ratio = 1)




