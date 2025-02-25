library(ggplot2)
library(patchwork)
library(readxl)
library(dplyr)
library(forcats)

#### Open Datasets and Extra ####

#NOTE: Make sure to load Harvest_Data_2 on DFD Data Tidying FIRST- data preprocessing required

load("Harvest_Data_LB.RData")
transpiration_rates <- read_xlsx("Lysimetry+Info.xlsx", sheet = "Transpiration Rates")

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

#### Percent Colonization and Biomass 2-Panel Graph - Figure 2 ####

# Add Tukey HSD letters- already calculated on 'Stats.R'
tukey_letters <- data.frame(
  Species = c("NM", "SP", "RP", "TC", "S+T", "R+S"),
  Label = c("b", "b", "a", "b", "b", "a")
)

# Panel A: Percent Colonization by Species
perccol_graph <- ggplot(subset(Harvest_Data_LB), 
                        aes(x = factor(Species, levels = species_order), y = perccol, fill = Species)) +
  geom_boxplot(outlier.color = "black", outlier.size = 1.5) +
  scale_fill_manual(values = species_colors) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7) +  # Add jitter points
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
  geom_boxplot(outlier.color = "black", outlier.size = 1.5) +
  scale_fill_manual(values = species_colors) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7) +  # Add jitter points
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



#### Fungi Impact Plant Water Use, Measured by Transpiration Rate Graph - Figure 1 ####

# Panel A: X axis species, Y axis transpiration rate day 0 of drydown (final measurement before dry down, right after water is applied)

# Convert Species and Treatment to a factor with the specified order
transpiration_rates$Species <- factor(transpiration_rates$Species, levels = species_order)
transpiration_rates$Treatment <- factor(transpiration_rates$Treatment, levels = c("control", "drought"))

# Create a combined Species_Treatment variable for distinct coloring
transpiration_rates <- transpiration_rates %>%
  mutate(Species_Treatment = paste(Species, Treatment, sep = "_"))

# Day 0 Graph
plot_day0 <- ggplot(transpiration_rates %>% filter(Treatment == "drought", !is.na(Day0_Transpiration)), 
                    aes(x = Species, y = Day0_Transpiration, fill = Species)) +
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
transpiration_rates$Species_Treatment <- factor(paste(transpiration_rates$Species, transpiration_rates$Treatment, sep = "_"),
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
plot_day6_2 <- ggplot(transpiration_rates %>% filter(!is.na(Harvest_Transpiration)), aes(x = Species, y = Harvest_Transpiration, linetype = Treatment)) +
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


#### Supplemental Graphs ####

#Root:Shoot by Species and Treatment
Harvest_Data_LB$Root_to_Shoot_Ratio <- Harvest_Data_LB$Root_DW / Harvest_Data_LB$Shoot_DW

Root_Shoot_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Root_to_Shoot_Ratio, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Root to Shoot Ratio By Species and Treatment", x = "Species", y = "Root to Shoot Ratio") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Root_Shoot_ST_graph)

#Root DW by Species and Treatment
Root_DW_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Root_DW, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Root Dry Weight By Species and Treatment", x = "Species", y = "Root Dry Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Root_DW_ST_graph)

#Root FW by Species and Treatment
Root_FW_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Root_FW_before, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Root Fresh Weight By Species and Treatment", x = "Species", y = "Root Fresh Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Root_FW_ST_graph)

#Shoot DW by Species and Treatment
Shoot_DW_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Shoot_DW, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Dry Weight By Species and Treatment", x = "Species", y = "Shoot Dry Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Shoot_DW_ST_graph)

#Shoot FW by Species and Treatment
Shoot_FW_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = Shoot_FW_before, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Fresh Weight By Species and Treatment", x = "Species", y = "Shoot Fresh Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Shoot_FW_ST_graph)

#Root % MC by Species and Treatment
Root_MC_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Root_%_MC`, fill = Treatment)) + 
  geom_boxplot() + 
  ylim(65, NA) +
  labs(title = "Root Moisture Content By Species and Treatment", x = "Species", y = "Root Moisture Content (%)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Root_MC_ST_graph)

#Shoot % MC by Species and Treatment
Shoot_MC_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Shoot_%_MC`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Moisture Content By Species and Treatment", x = "Species", y = "Shoot Moisture Content (%)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Shoot_MC_ST_graph)

#Stem Diameter by Species and Treatment
Stem_diameter_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Stem_diameter`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Stem Diameter By Species and Treatment", x = "Species", y = "Stem Diameter (cm)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Stem_diameter_ST_graph)

#Final Weight by Species and Treatment
Harvest_Data_LB$Final_weight <- as.numeric(Harvest_Data_LB$Final_weight)

Harvest_Data_LB <- Harvest_Data_LB[!is.na(Harvest_Data_LB$Final_weight), ]

Final_weight_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Final_weight`, fill = Treatment)) + 
  geom_boxplot() + 
  ylim(NA, 90) +
  labs(title = "Final Total Weight By Species and Treatment", x = "Species", y = "Final Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Final_weight_ST_graph)

#Leaf SA by Species and Treatment
Leaf_SA_ST_graph <- ggplot(Harvest_Data_LB, aes(x = factor(Species, levels = species_order), y = `Av_Needle_SA`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Leaf Surface Area By Species and Treatment", x = "Species", y = "Leaf Surface Area (cm)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))
print(Leaf_SA_ST_graph)

#% Colonization by Total Dry Biomass (without values = 0)
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






