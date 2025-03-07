#### Statistics ####

#### Open Datasets and Extra ####

#NOTE: Make sure to load all the DFD Data Tidying and All Graphs script FIRST- data preprocessing required

#### Main Figures ####

# Figure 1, Panel A:Percent Colonization by Species (Significant)
Perccol_by_Species <- aov(`perccol` ~ Species, data = Harvest_Data_LB)
summary(Perccol_by_Species)

TukeyHSD(Perccol_by_Species)

# Figure 1, Panel B: Total Dry Biomass by Species (not significant, but RP plants were a bit smaller than R+S and other)
totalmassanova = aov(total_dry_mass ~ Species, data = Harvest_Data_LB)
summary(totalmassanova)
TukeyHSD(totalmassanova)

TukeyHSD(totalmassanova)

#Total Dry Biomass by Treatment (not significant)
totalmassanovabytreatment = aov(total_dry_mass ~ Treatment, data = Harvest_Data_LB)
summary(totalmassanovabytreatment)

TukeyHSD(totalmassanovabytreatment)

#Total Dry Biomass by Species and Treatment (not significant)
totalmassanovabyST = aov(total_dry_mass ~ Species * Treatment, data = Harvest_Data_LB)
summary(totalmassanovabyST)

TukeyHSD(totalmassanovabyST)

#% Colonization by Total Dry Biomass (not significant)
Colonization_by_biomass <- aov(total_dry_mass ~ Species * perccol, data = subset(Harvest_Data_LB, Species != "NM"))
summary(Colonization_by_biomass)

Colonization_by_biomass_simplified <- aov(total_dry_mass ~ perccol, data = subset(Harvest_Data_LB, Species != "NM"))
summary(Colonization_by_biomass_simplified)

TukeyHSD(Colonization_by_biomass)
TukeyHSD(Colonization_by_biomass_simplified)

# Figure 2: Difference in Transpiration During Drought

# Merge datasets to align transpiration rates by ID
combined_transpiration <- day0_transpiration %>%
  inner_join(harvest_transpiration, by = c("ID", "Species", "Treatment"))

# Reshape to long format for paired comparison
long_data_2 <- combined_transpiration %>%
  pivot_longer(cols = c(Day0Transpiration, HarvestDateTranspiration),
               names_to = "Timepoint",
               values_to = "Transpiration_Rate") %>%
  mutate(Timepoint = factor(Timepoint, levels = c("Day0Transpiration", "HarvestDateTranspiration")))

# Difference between Day0 and HarvestDate transpiration rates
anova_day0_vs_harvest_simple <- aov(Transpiration_Rate ~ Timepoint, data = long_data_2)
summary(anova_day0_vs_harvest_simple)

# Day0 transpiration rates by species
anova_day0_species <- aov(Day0Transpiration ~ Species, data = day0_transpiration)
summary(anova_day0_species)

# HarvestDate transpiration rates by species
anova_harvest_species <- aov(HarvestDateTranspiration ~ Species, data = harvest_transpiration)
summary(anova_harvest_species)


#### Supplemental ####

lysimetry_data <- read_csv("lysimetry_data_for_analysis.csv")

# Transpiration Rate by Species
Lysimetry_by_Species <- aov(`Transpiration_Rate_Value` ~ Species, data = lysimetry_data)
summary(Lysimetry_by_Species)

TukeyHSD(Lysimetry_by_Species)

# Root:Shoot Ratio by Species (not significant)
Harvest_Data_LB$Root_to_Shoot_Ratio <- Harvest_Data_LB$Root_DW / Harvest_Data_LB$Shoot_DW

Root_to_Shoot_Ratio_by_Species <- aov(`Root_to_Shoot_Ratio` ~ Species, data = Harvest_Data_LB)
summary(Root_to_Shoot_Ratio_by_Species)

TukeyHSD(Root_to_Shoot_Ratio_by_Species)

# Root:Shoot Ratio by Treatment (not significant)
Root_to_Shoot_Ratio_by_Treatment <- aov(`Root_to_Shoot_Ratio` ~ Treatment, data = Harvest_Data_LB)
summary(Root_to_Shoot_Ratio_by_Treatment)

TukeyHSD(Root_to_Shoot_Ratio_by_Treatment)

# Root:Shoot Ratio by Treatment and Species (not sig)
Root_to_Shoot_Ratio_by_T_S <- aov(`Root_to_Shoot_Ratio` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Root_to_Shoot_Ratio_by_T_S)

TukeyHSD(Root_to_Shoot_Ratio_by_T_S)

# Root DW by Species (not significant)
Root_DW_by_Species <- aov(`Root_DW` ~ Species, data = Harvest_Data_LB)
summary(Root_DW_by_Species)

TukeyHSD(Root_DW_by_Species)

# Root DW by Treatment (not significant)
Root_DW_by_Treatment <- aov(`Root_DW` ~ Treatment, data = Harvest_Data_LB)
summary(Root_DW_by_Treatment)

TukeyHSD(Root_DW_by_Treatment)

# Root DW by Species and Treatment (not significant)
Root_DW_by_ST <- aov(`Root_DW` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Root_DW_by_ST)

TukeyHSD(Root_DW_by_ST)

# Root FW by Species (not significant)
Root_FW_by_Species <- aov(`Root_FW_before` ~ Species, data = Harvest_Data_LB)
summary(Root_FW_by_Species)

TukeyHSD(Root_FW_by_Species)

# Root FW by Treatment (not significant)
Root_FW_by_Treatment <- aov(`Root_FW_before` ~ Treatment, data = Harvest_Data_LB)
summary(Root_FW_by_Treatment)

TukeyHSD(Root_FW_by_Treatment)

# Root FW by Species and Treatment (not significant)
Root_FW_by_ST <- aov(`Root_FW_before` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Root_FW_by_ST)

TukeyHSD(Root_FW_by_ST)

# Shoot DW by Species (Significant, RP-R+S, SP-R+S)
Shoot_DW_by_Species <- aov(`Shoot_DW` ~ Species, data = Harvest_Data_LB)
summary(Shoot_DW_by_Species)
TukeyHSD(Shoot_DW_by_Species)

TukeyHSD(Shoot_DW_by_Species)

# Shoot DW by Treatment (not significant)
Shoot_DW_by_Treatment <- aov(`Shoot_DW` ~ Treatment, data = Harvest_Data_LB)
summary(Shoot_DW_by_Treatment)

TukeyHSD(Shoot_DW_by_Treatment)

# Shoot DW by Species and Treatment (not significant)
Shoot_DW_by_ST <- aov(`Shoot_DW` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Shoot_DW_by_ST)

TukeyHSD(Shoot_DW_by_ST)

# Shoot FW by Species (Significant)
Shoot_FW_by_Species <- aov(`Shoot_FW_before` ~ Species, data = Harvest_Data_LB)
summary(Shoot_FW_by_Species)

TukeyHSD(Shoot_FW_by_Species)

# Shoot FW by Treatment (not significant)
Shoot_FW_by_Treatment <- aov(`Shoot_FW_before` ~ Treatment, data = Harvest_Data_LB)
summary(Shoot_FW_by_Treatment)

TukeyHSD(Shoot_FW_by_Treatment)

# Shoot FW by Species and Treatment (not significant)
Shoot_FW_by_ST <- aov(`Shoot_FW_before` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Shoot_FW_by_ST)

TukeyHSD(Shoot_FW_by_ST)

# Stem Diameter by Species (not significant)
Stem_diameter_by_Species <- aov(`Stem_diameter` ~ Species, data = Harvest_Data_LB)
summary(Stem_diameter_by_Species)

TukeyHSD(Stem_diameter_by_Species)

# Stem Diameter by Treatment (not significant)
Stem_diameter_by_Treatment <- aov(`Stem_diameter` ~ Treatment, data = Harvest_Data_LB)
summary(Stem_diameter_by_Treatment)

TukeyHSD(Stem_diameter_by_Treatment)

# Stem Diameter by Species and Treatment (not significant)
Stem_diameter_by_ST <- aov(`Stem_diameter` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Stem_diameter_by_ST)

TukeyHSD(Stem_diameter_by_ST)

# Root MC by Species (not significant)
Root_MC_by_Species <- aov(`Root_%_MC` ~ Species, data = Harvest_Data_LB)
summary(Root_MC_by_Species)

TukeyHSD(Root_MC_by_Species)

# Root MC by Treatment (not significant)
Root_MC_by_Treatment <- aov(`Root_%_MC` ~ Treatment, data = Harvest_Data_LB)
summary(Root_MC_by_Treatment)

TukeyHSD(Root_MC_by_Treatment)

# Root MC by Species and Treatment (not significant)
Root_MC_by_ST <- aov(`Root_%_MC` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Root_MC_by_ST)

TukeyHSD(Root_MC_by_ST)

# Shoot MC by Species (not significant)
Shoot_MC_by_Species <- aov(`Shoot_%_MC` ~ Species, data = Harvest_Data_LB)
summary(Shoot_MC_by_Species)

TukeyHSD(Shoot_MC_by_Species)

# Shoot MC by Treatment (Significant)
Shoot_MC_by_Treatment <- aov(`Shoot_%_MC` ~ Treatment, data = Harvest_Data_LB)
summary(Shoot_MC_by_Treatment)

TukeyHSD(Shoot_MC_by_Treatment)

# Shoot MC by Species and Treatment (Significant)
Shoot_MC_by_ST <- aov(`Shoot_%_MC` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Shoot_MC_by_ST)

TukeyHSD(Shoot_MC_by_ST)

# Final Weight by Species (not significant)
Final_Weight_by_Species <- aov(`Final_weight` ~ Species, data = Harvest_Data_LB)
summary(Final_Weight_by_Species)

TukeyHSD(Final_Weight_by_Species)

# Final Weight by Treatment (significant)
Final_Weight_by_Treatment <- aov(`Final_weight` ~ Treatment, data = Harvest_Data_LB)
summary(Final_Weight_by_Treatment)

TukeyHSD(Final_Weight_by_Treatment)

# Final Weight by Species and Treatment (significant)
Final_Weight_by_ST <- aov(`Final_weight` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Final_Weight_by_ST)

TukeyHSD(Final_Weight_by_ST)

# Leaf SA by Species (not significant)
Av_Needle_SA_Species <- aov(`Av_Needle_SA` ~ Species, data = Harvest_Data_LB)
summary(Av_Needle_SA_Species)

TukeyHSD(Av_Needle_SA_Species)

# Leaf SA by Treatment (not significant)
Av_Needle_SA_Treatment <- aov(`Av_Needle_SA` ~ Treatment, data = Harvest_Data_LB)
summary(Av_Needle_SA_Treatment)

TukeyHSD(Av_Needle_SA_Treatment)

# Leaf SA by Species and Treatment (not significant)
Av_Needle_SA_ST <- aov(`Av_Needle_SA` ~ Species * Treatment, data = Harvest_Data_LB)
summary(Av_Needle_SA_ST)

TukeyHSD(Av_Needle_SA_ST)