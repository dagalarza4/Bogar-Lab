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

dev.off()
ggplot(Harvest_Data, aes(Species, `%_colonization`)) + geom_point()

ggplot(Harvest_Data, aes(Species, `%_colonization`)) + geom_point(colour = 5) + 
  labs(title = "Percent Colonization by Species", x = "Species", y = "Percent Colonization")

ggplot(Harvest_Data, aes(Species, `%_colonization`)) + geom_boxplot() +
  labs(title = "Percent Colonization by Species", x = "Species", y = "Percentage")

ggplot(Harvest_Data, aes(Species, `%_colonization`, fill = Species)) + geom_boxplot() + 
  labs(title = "Percent Colonization by Fungal Species", x = "Species", y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(Harvest_Data, aes(Species, `Root_%_MC`)) + geom_boxplot()
ggplot(Harvest_Data, aes(Species, `Shoot_%_MC`)) + geom_boxplot()

Root_MC_by_Species <- aov(`Root_%_MC` ~ Species, data = Harvest_Data)
summary(Root_MC_by_Species)

Shoot_MC_by_Species <- aov(`Shoot_%_MC` ~ Species, data = Harvest_Data)
summary(Shoot_MC_by_Species)

Percent_Col_by_Species <- aov(`%_colonization` ~ Species, data = Harvest_Data)
summary(Percent_Col_by_Species)

melt(Harvest_Data, treatment.vars = 'id')
measure.vars = c('drought', 'control')
ggplot(Harvest_Data) + geom_boxplot(aes(Species, `%_colonization`))

ggplot(Harvest_Data, aes(Species, `%_colonization`, fill = Species)) + geom_boxplot() + 
  labs(title = "Percent Colonization by Fungal Species", x = "Species", y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

# Laura's suggested split by color:

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

# Assuming LBperccol refers to the percent colonization data
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

# Print the plot
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

#Me
Harvest_Data$Root_to_Shoot_Ratio <- Harvest_Data$Root_DW / Harvest_Data$Shoot_DW
Root_to_Shoot_Ratio_by_Species <- aov(`Root_to_Shoot_Ratio` ~ Species, data = Harvest_Data)
summary(Root_to_Shoot_Ratio_by_Species)
Root_DW_by_Species <- aov(`Root_DW` ~ Species, data = Harvest_Data)
summary(Root_DW_by_Species)
Shoot_DW_by_Species <- aov(`Shoot_DW` ~ Species, data = Harvest_Data)
summary(Shoot_DW_by_Species)
Root_FW_by_Species <- aov(`Root_FW_before` ~ Species, data = Harvest_Data)
summary(Root_FW_by_Species)
Shoot_FW_by_Species <- aov(`Shoot_FW_before` ~ Species, data = Harvest_Data)
summary(Shoot_FW_by_Species)
Stem_diameter_by_Species <- aov(`Stem_diameter` ~ Species, data = Harvest_Data)
summary(Stem_diameter_by_Species)
Stem_diameter_by_Treatment <- aov(`Stem_diameter` ~ Treatment, data = Harvest_Data)
summary(Stem_diameter_by_Treatment)
Root_to_Shoot_Ratio_by_Treatment <- aov(`Root_to_Shoot_Ratio` ~ Treatment, data = Harvest_Data)
summary(Root_to_Shoot_Ratio_by_Treatment)

boxplot(Harvest_Data$Shoot_DW~Harvest_Data$Species+Harvest_Data$Treatment)
ggplot(Harvest_Data, aes(x=Species, y=Shoot_DW, fill=Treatment))+
  geom_boxplot()+
  labs(title = "Shoot Dry Weight By Species and Treatment", x = "Species", y = "Shoot Dry Weight (g)")

boxplot(Harvest_Data$Shoot_FW_before~Harvest_Data$Species+Harvest_Data$Treatment)
ggplot(Harvest_Data, aes(x=Species, y=Shoot_FW_before, fill=Treatment))+
  geom_boxplot()+
  labs(title = "Shoot Fresh Weight By Species and Treatment", x = "Species", y = "Shoot Fresh Weight (g)")



#### 9/22/24 update ####
ggplot(Harvest_Data, aes(Species, `Root_%_MC`)) + geom_boxplot() +
  ylim(65, NA) +
  labs(y = "Root Percent Moisture Content")

ggplot(Harvest_Data, aes(Species, `Shoot_%_MC`)) + geom_boxplot() +
  ylim(55, NA) +
  labs(y = "Shoot Percent Moisture Content")

ggplot(Harvest_Data, aes (Species, `Stem_diameter`)) + geom_boxplot() +
  labs(y = "Stem Diameter (cm)")

ggplot(Harvest_Data, aes (Species, `Shoot_FW_after`)) + geom_boxplot() +
  labs(y = "Shoot Fresh Weight (g)")

ggplot(Harvest_Data, aes (Species, `Root_FW_after`)) + geom_boxplot() +
  labs(y = "Root Fresh Weight (g)")

ggplot(Harvest_Data, aes (Species, `Average_SA`)) + geom_boxplot() +
  ylim(25, NA) +
  labs(y = "Leaf Surface Area (mm)")



ggplot(Harvest_Data, aes (factor (Species), `Final_weight`)) + geom_boxplot() +
  ylim(0, NA) +
  labs(y = "Final Weight (g)")

ggplot(Harvest_Data, aes(factor(Species), `Final_weight`)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, NA)) +  # This zooms the y-axis without removing data
  labs(y = "Final Weight (g)")



