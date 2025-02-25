#### Statistics ####

#### Main Figures ####

# Figure 1, Panel A: Transpiration Rate Day 0 of Drought by Species (not significant)
anova_day0 <- aov(Day0_Transpiration ~ factor(Species), data = transpiration_rates %>% filter(Treatment == "drought", !is.na(Day0_Transpiration)))
summary(anova_day0)

tukey_day0 <- TukeyHSD(anova_day0)
summary(tukey_day0)

# Tukey HSD for pairwise comparisons for Day 0 Transpiration
tukey_day0 <- TukeyHSD(anova_day0)
summary(tukey_day0)

# Transpiration Rate of all Plants at Harvest (Day 6 of Drought) (not significant)
anova_harvest <- aov(Harvest_Transpiration ~ Species * Treatment, data = transpiration_rates)
summary(anova_harvest)

tukey_harvest <- TukeyHSD(anova_harvest)
summary(tukey_harvest)

# Differences in transpiration rate by species (not significant)
anova_model_differences <- aov(Difference_g_hr ~ Species, data = difference_data)
summary(anova_model_differences)

tukey_results <- TukeyHSD(anova_model_differences)
print(tukey_results)



# Figure 2, Panel A:Percent Colonization by Species (Significant)
Perccol_by_Species <- aov(`perccol` ~ Species, data = Harvest_Data_LB)
summary(Perccol_by_Species)

TukeyHSD(Perccol_by_Species)

# Figure 2, Panel B: Total Dry Biomass by Species (not significant, but RP plants were a bit smaller than R+S and other)
totalmassanova = aov(total_dry_mass ~ Species, data = Harvest_Data_LB)
summary(totalmassanova)
TukeyHSD(totalmassanova)

TukeyHSD(totalmassanova)

#Total Dry Biomass by Treatment (not significant)
totalmassanovabytreatment = aov(total_dry_mass ~ Treatment, data = Harvest_Data_Clean)
summary(totalmassanovabytreatment)

TukeyHSD(totalmassanovabytreatment)

#Total Dry Biomass by Species and Treatment (not significant)
totalmassanovabyST = aov(total_dry_mass ~ Species * Treatment, data = Harvest_Data_Clean)
summary(totalmassanovabyST)

TukeyHSD(totalmassanovabyST)

#% Colonization by Total Dry Biomass (not significant)
Colonization_by_biomass <- aov(total_dry_mass ~ Species * perccol, data = subset(Harvest_Data_Clean, Species != "NM"))
summary(Colonization_by_biomass)

Colonization_by_biomass_simplified <- aov(total_dry_mass ~ perccol, data = subset(Harvest_Data_Clean, Species != "NM"))
summary(Colonization_by_biomass_simplified)

TukeyHSD(Colonization_by_biomass)
TukeyHSD(Colonization_by_biomass_simplified)




#### Supplemental ####

transpiration_rates <- read_xlsx("Lysimetry+Info.xlsx", sheet = "Transpiration Rates")

#Transpiration Rate by Species
Trans_by_Species <- aov(`Transpiration_rate_value` ~ Species, data = transpiration_data)
summary(Trans_by_Species)

TukeyHSD(Trans_by_Species)

#Transpiration Rate by Percent Colonization
Trans_by_Species <- aov(`Transpiration_rate_value` ~ `%_colonization`, data = transpiration_data)
summary(Trans_by_Species)

TukeyHSD(Trans_by_Perccol)

#Root:Shoot Ratio by Species (not significant)
Harvest_Data_Clean$Root_to_Shoot_Ratio <- Harvest_Data_Clean$Root_DW / Harvest_Data_Clean$Shoot_DW

Root_to_Shoot_Ratio_by_Species <- aov(`Root_to_Shoot_Ratio` ~ Species, data = Harvest_Data_Clean)
summary(Root_to_Shoot_Ratio_by_Species)

TukeyHSD(Root_to_Shoot_Ratio_by_Species)

#Root:Shoot Ratio by Treatment (not significant)
Root_to_Shoot_Ratio_by_Treatment <- aov(`Root_to_Shoot_Ratio` ~ Treatment, data = Harvest_Data_Clean)
summary(Root_to_Shoot_Ratio_by_Treatment)

TukeyHSD(Root_to_Shoot_Ratio_by_Treatment)

#Root:Shoot Ratio by Treatment and Species (not sig)
Root_to_Shoot_Ratio_by_T_S <- aov(`Root_to_Shoot_Ratio` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Root_to_Shoot_Ratio_by_T_S)

TukeyHSD(Root_to_Shoot_Ratio_by_T_S)

#Root DW by Species (not significant)
Root_DW_by_Species <- aov(`Root_DW` ~ Species, data = Harvest_Data_Clean)
summary(Root_DW_by_Species)

TukeyHSD(Root_DW_by_Species)

#Root DW by Treatment (not significant)
Root_DW_by_Treatment <- aov(`Root_DW` ~ Treatment, data = Harvest_Data_Clean)
summary(Root_DW_by_Treatment)

TukeyHSD(Root_DW_by_Treatment)

#Root DW by Species and Treatment (not significant)
Root_DW_by_ST <- aov(`Root_DW` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Root_DW_by_ST)

TukeyHSD(Root_DW_by_ST)

#Root FW by Species (not significant)
Root_FW_by_Species <- aov(`Root_FW_before` ~ Species, data = Harvest_Data_Clean)
summary(Root_FW_by_Species)

TukeyHSD(Root_FW_by_Species)

#Root FW by Treatment (not significant)
Root_FW_by_Treatment <- aov(`Root_FW_before` ~ Treatment, data = Harvest_Data_Clean)
summary(Root_FW_by_Treatment)

TukeyHSD(Root_FW_by_Treatment)

#Root FW by Species and Treatment (not significant)
Root_FW_by_ST <- aov(`Root_FW_before` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Root_FW_by_ST)

TukeyHSD(Root_FW_by_ST)

#Shoot DW by Species (Significant, RP-R+S, SP-R+S)
Shoot_DW_by_Species <- aov(`Shoot_DW` ~ Species, data = Harvest_Data_Clean)
summary(Shoot_DW_by_Species)
TukeyHSD(Shoot_DW_by_Species)

TukeyHSD(Shoot_DW_by_Species)

#Shoot DW by Treatment (not significant)
Shoot_DW_by_Treatment <- aov(`Shoot_DW` ~ Treatment, data = Harvest_Data_Clean)
summary(Shoot_DW_by_Treatment)

TukeyHSD(Shoot_DW_by_Treatment)

#Shoot DW by Species and Treatment (not significant)
Shoot_DW_by_ST <- aov(`Shoot_DW` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Shoot_DW_by_ST)

TukeyHSD(Shoot_DW_by_ST)

#Shoot FW by Species (Significant)
Shoot_FW_by_Species <- aov(`Shoot_FW_before` ~ Species, data = Harvest_Data_Clean)
summary(Shoot_FW_by_Species)

TukeyHSD(Shoot_FW_by_Species)

#Shoot FW by Treatment (not significant)
Shoot_FW_by_Treatment <- aov(`Shoot_FW_before` ~ Treatment, data = Harvest_Data_Clean)
summary(Shoot_FW_by_Treatment)

TukeyHSD(Shoot_FW_by_Treatment)

#Shoot FW by Species and Treatment (not significant)
Shoot_FW_by_ST <- aov(`Shoot_FW_before` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Shoot_FW_by_ST)

TukeyHSD(Shoot_FW_by_ST)

#Stem Diameter by Species (not significant)
Stem_diameter_by_Species <- aov(`Stem_diameter` ~ Species, data = Harvest_Data_Clean)
summary(Stem_diameter_by_Species)

TukeyHSD(Stem_diameter_by_Species)

#Stem Diameter by Treatment (not significant)
Stem_diameter_by_Treatment <- aov(`Stem_diameter` ~ Treatment, data = Harvest_Data_Clean)
summary(Stem_diameter_by_Treatment)

TukeyHSD(Stem_diameter_by_Treatment)

#Stem Diameter by Species and Treatment (not significant)
Stem_diameter_by_ST <- aov(`Stem_diameter` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Stem_diameter_by_ST)

TukeyHSD(Stem_diameter_by_ST)

#Root MC by Species (not significant)
Root_MC_by_Species <- aov(`Root_%_MC` ~ Species, data = Harvest_Data_Clean)
summary(Root_MC_by_Species)

TukeyHSD(Root_MC_by_Species)

#Root MC by Treatment (not significant)
Root_MC_by_Treatment <- aov(`Root_%_MC` ~ Treatment, data = Harvest_Data_Clean)
summary(Root_MC_by_Treatment)

TukeyHSD(Root_MC_by_Treatment)

#Root MC by Species and Treatment (not significant)
Root_MC_by_ST <- aov(`Root_%_MC` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Root_MC_by_ST)

TukeyHSD(Root_MC_by_ST)

#Shoot MC by Species (not significant)
Shoot_MC_by_Species <- aov(`Shoot_%_MC` ~ Species, data = Harvest_Data_Clean)
summary(Shoot_MC_by_Species)

TukeyHSD(Shoot_MC_by_Species)

#Shoot MC by Treatment (Significant)
Shoot_MC_by_Treatment <- aov(`Shoot_%_MC` ~ Treatment, data = Harvest_Data_Clean)
summary(Shoot_MC_by_Treatment)

TukeyHSD(Shoot_MC_by_Treatment)

#Shoot MC by Species and Treatment (Significant)
Shoot_MC_by_ST <- aov(`Shoot_%_MC` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Shoot_MC_by_ST)

TukeyHSD(Shoot_MC_by_ST)

#Final Weight by Species (not significant)
Final_Weight_by_Species <- aov(`Final_weight` ~ Species, data = Harvest_Data_Clean)
summary(Final_Weight_by_Species)

TukeyHSD(Final_Weight_by_Species)

#Final Weight by Treatment (significant)
Final_Weight_by_Treatment <- aov(`Final_weight` ~ Treatment, data = Harvest_Data_Clean)
summary(Final_Weight_by_Treatment)

TukeyHSD(Final_Weight_by_Treatment)

#Final Weight by Species and Treatment (significant)
Final_Weight_by_ST <- aov(`Final_weight` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Final_Weight_by_ST)

TukeyHSD(Final_Weight_by_ST)

#Leaf SA by Species (not significant)
Av_Needle_SA_Species <- aov(`Av_Needle_SA` ~ Species, data = Harvest_Data_Clean)
summary(Av_Needle_SA_Species)

TukeyHSD(Av_Needle_SA_Species)

#Leaf SA by Treatment (not significant)
Av_Needle_SA_Treatment <- aov(`Av_Needle_SA` ~ Treatment, data = Harvest_Data_Clean)
summary(Av_Needle_SA_Treatment)

TukeyHSD(Av_Needle_SA_Treatment)

#Leaf SA by Species and Treatment (not significant)
Av_Needle_SA_ST <- aov(`Av_Needle_SA` ~ Species * Treatment, data = Harvest_Data_Clean)
summary(Av_Needle_SA_ST)

TukeyHSD(Av_Needle_SA_ST)

#Control transpiration rate ANOVA statistics
panel_a_anova_data <- panel_a_data %>% 
  filter(!is.na(Average_Transpiration))

panel_a_anova <- aov(Average_Transpiration ~ Species, data = panel_a_anova_data)
summary(panel_a_anova)

TukeyHSD(panel_a_anova)

#Control transpiration rate by % colonization ANOVA statistics
trans_by_perccol_data <- trans_by_perccol_data %>%
  filter(!is.na(Average_Transpiration), !is.na(perccol))

trans_by_perccol_anova <- aov(Average_Transpiration ~ Species * perccol, data = trans_by_perccol_data)
summary(trans_by_perccol_anova)

TukeyHSD(trans_by_perccol_anova)

#Drought transpiration rate 2 Days B4 harvest ANOVA test
plot_b_anova_data <- plot_b_data %>%
  mutate(
    Day_Label = factor(Day_Label, levels = c("2 Days Before Harvest", "1 Day Before Harvest")),
    Species = as.factor(Species))

plot_b_anova <- aov(Average_Transpiration ~ Day_Label * Species, data = plot_b_anova_data)

summary(plot_b_anova)

#Drought transpiration rate by % colonization ANOVA test
trans_by_perccol_drought_anova_data <- trans_by_perccol_drought %>%
  mutate(
    perccol = as.numeric(perccol), 
    Species = as.factor(Species))

library(lme4)

lmm_trans_perccol_D <- lmer(Average_Transpiration ~ perccol * Species + (1 | Plant_ID), data = trans_by_perccol_drought_anova_data)

summary(lmm_trans_perccol_D)

anova(lmm_trans_perccol_D)

#Combined transpiration rate 2 days b4 harvest ANOVA test
combined_trans_2_days_b4_anova <- aov(Average_Transpiration ~ Treatment * Day_Label * Species,
                                      data = combined_trans_2_days_b4)

summary(combined_trans_2_days_b4)

#Combined transpiration rate 2 days b4 by % colonization ANOVA test
combined_data_colonization <- combined_data_colonization %>%
  mutate(
    Treatment = as.factor(Treatment),
    Species = as.factor(Species),
    `perccol` = as.factor(`perccol`))

combined_colonization_anova <- aov(Average_Transpiration ~ Treatment * `perccol` * Species, data = combined_data_colonization)

summary(combined_colonization_anova)