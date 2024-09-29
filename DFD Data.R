library(tidyverse)
library(ggplot2)
library(broom)
library(AICcmodavg)
library(ggpubr)
library(devtools)
library(usethis)
library(reshape2)
library(dplyr)

head(Harvest_Data)
names(Harvest_Data)

#### Percent Colonization ####
dev.off()
ggplot(Harvest_Data, aes(Species, `%_colonization`)) + geom_point()

ggplot(Harvest_Data, aes(Species, `%_colonization`)) + geom_point(colour = 5) + 
  labs(title = "Percent Colonization by Species", x = "Species", y = "Percent Colonization")

ggplot(Harvest_Data, aes(Species, `%_colonization`)) + geom_boxplot() +
  labs(title = "Percent Colonization by Species", x = "Species", y = "Percentage")

ggplot(Harvest_Data, aes(Species, `%_colonization`, fill = Species)) + geom_boxplot() + 
  labs(title = "Percent Colonization by Fungal Species", x = "Species", y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

melt(Harvest_Data, treatment.vars = 'id')
measure.vars = c('drought', 'control')
ggplot(Harvest_Data) + geom_boxplot(aes(Species, `%_colonization`))

ggplot(Harvest_Data, aes(Species, `%_colonization`, fill = Species)) + geom_boxplot() + 
  labs(title = "Percent Colonization by Fungal Species", x = "Species", y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

# Laura's suggested split by color for Percent Colonization:

ggplot(Harvest_Data, aes(Species, `%_colonization`, 
                         fill = Species,
                         color = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Percent Colonization by Fungal Species", 
       x = "Species", 
       y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

Harvest_Data$LBperccol = 
  (Harvest_Data$`%_colonization`/100)/((Harvest_Data$`%_colonization`/100)+1)*100

ggplot(Harvest_Data, aes(Species, LBperccol, 
                         fill = Species,
                         color = Treatment)) + 
  geom_boxplot(outlier.alpha = 0) + 
  labs(title = "Percent Colonization by Fungal Species", 
       x = "Species", 
       y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(Harvest_Data, aes(Species, LBperccol, 
                         fill = Species)) + 
  geom_boxplot(outlier.alpha = 0) + 
  labs( 
       x = "Species", 
       y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

#Add a title to the above graph
library(ggplot2)

library(ggplot2)

library(ggplot2)

p <- ggplot(Harvest_Data, aes(x = Species, y = LBperccol, fill = Species)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  labs(title = "Percentage Colonization by Fungal Species",
       x = "Species",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Adjust size as needed
        axis.title.x = element_text(size = 10),  # Adjust size as needed
        axis.title.y = element_text(size = 10, face = "plain"))  # Use "plain" instead of "normal" for face

print(p)

# Checking statistics 
summary_stats %>%
  mutate(
    Species = as.character(Species)
  ) %>%
  group_by(Species) %>%
  summarise(
    Mean_of_LBperccol = mean(mean_value),
    Median_of_LBperccol = median(median_value),
    SD_of_LBperccol = sd(sd_value)
  ) %>%
  mutate(
    summary = paste("Species:", Species, "\n",
                    "Mean of LBperccol:", Mean_of_LBperccol, "\n",
                    "Median of LBperccol:", Median_of_LBperccol, "\n",
                    "Standard deviation of LBperccol:", SD_of_LBperccol, "\n")
  ) %>%
  select(summary)

#Create total dry biomass
Harvest_Data_Perc = Harvest_Data %>% mutate(total_dry_biomass = Shoot_DW + Root_DW)
Harvest_Data_Perc$perccol = 
  (Harvest_Data_Perc$`%_colonization`/100)/((Harvest_Data_Perc$`%_colonization`/100)+1)*100



#### LB sandbox area: ####
# LB attempt:
# boxplot(Harvest_Data$Shoot_DW~Harvest_Data$Species)
ggplot(Harvest_Data, aes(x=Species, y=Shoot_DW))+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter()+
  labs(title = "Shoot Dry Weight By Fungal Species", x = "Species", y = "Shoot Dry Weight (g)")

Harvest_Data_LB = Harvest_Data %>% mutate(total_dry_mass = Shoot_DW + Root_DW)
Harvest_Data_LB$perccol = 
  (Harvest_Data_LB$`%_colonization`/100)/((Harvest_Data_LB$`%_colonization`/100)+1)*100

ggplot(Harvest_Data_LB, aes(x=Species, y=total_dry_mass))+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter()+
  labs(title = "Total Dry Biomass By Fungal Species", x = "Species", y = "Shoot Dry Weight (g)")

totalmassanova = aov(total_dry_mass ~ Species, data = Harvest_Data_LB)
summary(totalmassanova)
TukeyHSD(totalmassanova)
# No significant diffs, but indeed RP plants were a bit smaller than R+S and others.
shootmassanova = aov(Shoot_DW ~ Species, data = Harvest_Data_LB)
summary(shootmassanova)
TukeyHSD(shootmassanova)

ggplot(subset(Harvest_Data_LB, Species != "NM"), aes(x = perccol, y = total_dry_mass)) +
  geom_point() +
  geom_smooth(method = "lm")

#### End of LB sandbox area ####



#### Statistics #### 

#Percent Colonization by Species (Significant)
Percent_Col_by_Species <- aov(`%_colonization` ~ Species, data = Harvest_Data)
summary(Percent_Col_by_Species)

#Root:Shoot Ratio by Species (not significant)
Harvest_Data$Root_to_Shoot_Ratio <- Harvest_Data$Root_DW / Harvest_Data$Shoot_DW
Root_to_Shoot_Ratio_by_Species <- aov(`Root_to_Shoot_Ratio` ~ Species, data = Harvest_Data)
summary(Root_to_Shoot_Ratio_by_Species)

#Root:Shoot Ratio by Treatment (not significant)
Root_to_Shoot_Ratio_by_Treatment <- aov(`Root_to_Shoot_Ratio` ~ Treatment, data = Harvest_Data)
summary(Root_to_Shoot_Ratio_by_Treatment)

#Root DW by Species (not significant)
Root_DW_by_Species <- aov(`Root_DW` ~ Species, data = Harvest_Data)
summary(Root_DW_by_Species)

#Root DW by Treatment (not significant)
Root_DW_by_Treatment <- aov(`Root_DW` ~ Treatment, data = Harvest_Data)
summary(Root_DW_by_Treatment)

#Root FW by Species (not significant)
Root_FW_by_Species <- aov(`Root_FW_before` ~ Species, data = Harvest_Data)
summary(Root_FW_by_Species)

#Root FW by Treatment (not significant)
Root_FW_by_Treatment <- aov(`Root_FW_before` ~ Treatment, data = Harvest_Data)
summary(Root_FW_by_Treatment)

#Shoot DW by Species (Significant, RP-R+S, SP-R+S)
Shoot_DW_by_Species <- aov(`Shoot_DW` ~ Species, data = Harvest_Data)
summary(Shoot_DW_by_Species)
TukeyHSD(Shoot_DW_by_Species)

#Shoot DW by Treatment (not significant)
Shoot_DW_by_Treatment <- aov(`Shoot_DW` ~ Treatment, data = Harvest_Data)
summary(Shoot_DW_by_Treatment)

#Shoot FW by Species (Significant)
Shoot_FW_by_Species <- aov(`Shoot_FW_before` ~ Species, data = Harvest_Data)
summary(Shoot_FW_by_Species)

#Shoot FW by Treatment (not significant)
Shoot_FW_by_Treatment <- aov(`Shoot_FW_before` ~ Treatment, data = Harvest_Data)
summary(Shoot_FW_by_Treatment)

#Stem Diameter by Species (not significant)
Stem_diameter_by_Species <- aov(`Stem_diameter` ~ Species, data = Harvest_Data)
summary(Stem_diameter_by_Species)

#Stem Diameter by Treatment (not significant)
Stem_diameter_by_Treatment <- aov(`Stem_diameter` ~ Treatment, data = Harvest_Data)
summary(Stem_diameter_by_Treatment)

#Root MC by Species (not significant)
Root_MC_by_Species <- aov(`Root_%_MC` ~ Species, data = Harvest_Data)
summary(Root_MC_by_Species)

#Root MC by Treatment (not significant)
Root_MC_by_Treatment <- aov(`Root_%_MC` ~ Treatment, data = Harvest_Data)
summary(Root_MC_by_Treatment)

#Shoot MC by Species (not significant)
Shoot_MC_by_Species <- aov(`Shoot_%_MC` ~ Species, data = Harvest_Data)
summary(Shoot_MC_by_Species)

#Shoot MC by Treatment (Significant)
Shoot_MC_by_Treatment <- aov(`Shoot_%_MC` ~ Treatment, data = Harvest_Data)
summary(Shoot_MC_by_Treatment)

#Total Dry Biomass by Species (not significant, but indeed RP plants were a bit smaller than R+S and other)
totalmassanova = aov(total_dry_mass ~ Species, data = Harvest_Data_LB)
summary(totalmassanova)
TukeyHSD(totalmassanova)

#Total Dry Biomass by Treatment (not significant)
totalmassanovabytreatment = aov(total_dry_mass ~ Treatment, data = Harvest_Data_LB)
summary(totalmassanovabytreatment)





#### Boxplot Graphs ####

#Root:Shoot by Species
boxplot(Harvest_Data$Root_to_Shoot_Ratio ~ Harvest_Data$Species)
ggplot(Harvest_Data, aes(Species, Root_to_Shoot_Ratio)) +geom_boxplot() +
  labs(title = "Root to Shoot Ratio by Species", y = "Root to Shoot Ratio")

#Root:Shoot by Treatment (provides no meaningful info)
ggplot(Harvest_Data, aes(Treatment, Root_to_Shoot_Ratio)) +geom_boxplot() +
  labs(title = "Root to Shoot Ratio by Treatment", y = "Root to Shoot Ratio")

#Shoot DW by Species and Treatment
boxplot(Harvest_Data$Shoot_DW~Harvest_Data$Species+Harvest_Data$Treatment)
ggplot(Harvest_Data, aes(x=Species, y=Shoot_DW, fill=Treatment))+
  geom_boxplot()+
  labs(title = "Shoot Dry Weight By Species and Treatment", x = "Species", y = "Shoot Dry Weight (g)")

#Shoot FW by Species and Treatment
boxplot(Harvest_Data$Shoot_FW_before~Harvest_Data$Species+Harvest_Data$Treatment)
ggplot(Harvest_Data, aes(x=Species, y=Shoot_FW_before, fill=Treatment))+
  geom_boxplot()+
  labs(title = "Shoot Fresh Weight By Species and Treatment", x = "Species", y = "Shoot Fresh Weight (g)")

#Root % MC by Species 
ggplot(Harvest_Data, aes(Species, `Root_%_MC`)) + geom_boxplot() +
  ylim(65, NA) +
  labs(y = "Root Percent Moisture Content")

#Root % MC by Treatment 
ggplot(Harvest_Data, aes(Treatment, `Root_%_MC`)) + geom_boxplot() +
  ylim(65, NA) +
  labs(title= "Root Moisture Content by Treatment", y = "Root Percent Moisture Content (%)")

#Shoot % MC by Species
ggplot(Harvest_Data, aes(Species, `Shoot_%_MC`)) + geom_boxplot() +
  ylim(55, NA) +
  labs(y = "Shoot Percent Moisture Content")

#Shoot % MC by Treatment
ggplot(Harvest_Data, aes(Treatment, `Shoot_%_MC`)) + geom_boxplot() + 
  labs(title = "Shoot Moisture Content by Treatment", y = "Shoot Moisture Content (%)")

#Stem Diameter by Species
ggplot(Harvest_Data, aes (Species, `Stem_diameter`)) + geom_boxplot() +
  labs(y = "Stem Diameter (cm)")

#Shoot FW by Species
ggplot(Harvest_Data, aes (Species, `Shoot_FW_after`)) + geom_boxplot() +
  labs(y = "Shoot Fresh Weight (g)")

#Root FW by Species
ggplot(Harvest_Data, aes (Species, `Root_FW_after`)) + geom_boxplot() +
  labs(y = "Root Fresh Weight (g)")

#Attempt at Final Weight
ggplot(Harvest_Data, aes (factor (Species), `Final_weight`)) + geom_boxplot() +
  ylim(0, NA) +
  labs(y = "Final Weight (g)")

ggplot(Harvest_Data, aes(factor(Species), `Final_weight`)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, NA)) +  # This zooms the y-axis without removing data
  labs(y = "Final Weight (g)")

#Leaf SA by Species
Harvest_Data$Species <- factor(Harvest_Data$Species, 
                               levels = c("NM", "RP", "SP", "TC", "R+S", "T+C"))
ggplot(Harvest_Data, aes(Species, `Av_Needle_SA`)) + 
  geom_boxplot() + 
  labs(y = "Needle Surface Area (mm)")

#% Colonization by Total Dry Biomass
ggplot(subset(Harvest_Data_LB, Species != "NM"), aes(x = perccol, y = total_dry_mass, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Percent Colonization by Total Dry Biomass", 
       x = "Percent Colonization", 
       y = "Total Dry Biomass (g)")

#% Colonization by Total Dry Biomass (without values = 0)
Harvest_Data_LB$Species <- factor(Harvest_Data$Species, 
                               levels = c("NM", "RP", "SP", "TC", "R+S", "S+T"))
ggplot(subset(Harvest_Data_LB, Species != "NM" & perccol != 0), aes(x = perccol, y = total_dry_mass, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Percent Colonization by Total Dry Biomass", 
       x = "Percent Colonization", 
       y = "Total Dry Biomass (g)")


