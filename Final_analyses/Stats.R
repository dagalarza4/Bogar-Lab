#### Statistics ####

#### Open Datasets and Extra ####

#NOTE: Make sure to load all the DFD Data Tidying and All Graphs script FIRST- data preprocessing required

#### Main Figures ####

# Figure 1, Panel A:Percent Colonization by Species (Significant)
Perccol_by_Species <- aov(`perccol` ~ Species, data = Harvest_Data_LB)
summary(Perccol_by_Species)

TukeyHSD(Perccol_by_Species)

# Figure 1, Panel A:Percent Colonization by Species Type (Significant)
Perccol_by_Species_T <- aov(perccol ~ Species_Type, data = Harvest_Data_LB %>% 
                              filter(Species_Type %in% c("Single", "Double")))
summary(Perccol_by_Species_T)

# Figure 1, Panel B: Total Dry Biomass by Species (not significant, but RP plants were a bit smaller than R+S and other)
totalmassanova = aov(total_dry_mass ~ Species, data = Harvest_Data_LB)
summary(totalmassanova)

TukeyHSD(totalmassanova)

# Figure 1, Panel B: Total Dry Biomass by Species Type (Significant)
totalmassanova_SpT <- aov(total_dry_mass ~ Species_Type, data = Harvest_Data_LB %>% 
                              filter(Species_Type %in% c("Single", "Double")))
summary(totalmassanova_SpT)

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



# Figure 2: Difference in Transpiration During Drought by Species (not significant)
Trans_Difference_by_Species <- aov(`Difference_g_hr` ~ Species, data = TranspirationRates) 
summary(Trans_Difference_by_Species)

TukeyHSD(Trans_Difference_by_Species)

# Figure 2: Difference in Transpiration During Drought by Species Type (not significant)
Trans_Difference_by_Species_T <- aov(`Difference_g_hr` ~ Species_Type, data = TranspirationRates %>% 
  filter(Species_Type %in% c("Single", "Double")))
summary(Trans_Difference_by_Species_T)

#Figure 2: Difference in Transpiration During Drought by Treatment (Significant)
Trans_Difference_by_Treatment <- aov(`Difference_g_hr` ~ Treatment, data = TranspirationRates) 
summary(Trans_Difference_by_Treatment)

TukeyHSD(Trans_Difference_by_Treatment)

#Figure 2: Difference in Transpiration During Drought by Species and Treatment (not significant)
Trans_Difference_by_ST <- aov(`Difference_g_hr` ~ Species * Treatment, data = TranspirationRates) 
summary(Trans_Difference_by_ST)

TukeyHSD(Trans_Difference_by_ST)

# Figure 2: Day 0 Transpiration During Drought by Species (not significant)
Day0_by_Species <- aov(`Day0Transpiration` ~ Species, data = TranspirationRates) 
summary(Day0_by_Species)

TukeyHSD(Day0_by_Species)

# Figure 2: Day 0 Transpiration During Drought by Species Type (not significant)
Day0_by_Species_T <- aov(`Day0Transpiration` ~ Species, data = TranspirationRates %>% 
                           filter(Species_Type %in% c("Single", "Double")))
summary(Day0_by_Species_T)

#Figure 2: Day0 Transpiration During Drought by Treatment (Significant)
Day0_by_Treatment <- aov(`Day0Transpiration` ~ Treatment, data = TranspirationRates) 
summary(Day0_by_Treatment)

TukeyHSD(Day0_by_Treatment)

#Figure 2: Day 0 Transpiration During Drought by Species and Treatment (not significant)
Day0_by_ST <- aov(`Day0Transpiration` ~ Species * Treatment, data = TranspirationRates) 
summary(Day0_by_ST)

# Figure 2: Day 6 Transpiration During Drought by Species (not significant)
Day6_by_Species <- aov(`HarvestDateTranspiration` ~ Species, data = TranspirationRates) 
summary(Day6_by_Species)

TukeyHSD(Day6_by_Species)

# Figure 2: Day 6 Transpiration During Drought by Species Type (not significant)
Day6_by_Species_T <- aov(`HarvestDateTranspiration` ~ Species, data = TranspirationRates %>% 
                           filter(Species_Type %in% c("Single", "Double")))
summary(Day6_by_Species_T)

#Figure 2: Day 6 Transpiration During Drought by Treatment (Significant)
Day6_by_Treatment <- aov(`HarvestDateTranspiration` ~ Treatment, data = TranspirationRates) 
summary(Day6_by_Treatment)

TukeyHSD(Day6_by_Treatment)

#Figure 2: Day 6 Transpiration During Drought by Species and Treatment (not significant)
Day6_by_ST <- aov(`HarvestDateTranspiration` ~ Species * Treatment, data = TranspirationRates) 
summary(Day6_by_ST)




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