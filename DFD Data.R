library(tidyverse)
library(ggplot2)
library(broom)
library(AICcmodavg)
library(ggpubr)
library(devtools)
library(usethis)
library(reshape2)
library(dplyr)
library(readxl)

Harvest_Data <- read_excel("Harvest Data.xlsx")
head(Harvest_Data)
names(Harvest_Data)

treatments = read_csv("Treatment_key.csv") %>%
  mutate(ID = Plant_ID)


#11/9/24 update to fix S+T species category- NA displayed, not S+T
# Check for any remaining NA values in the 'Species' column
sum(is.na(Harvest_Data_LB$Species))
# Replace any remaining NA values in the 'Species' column with "S+T"
Harvest_Data_LB$Species[is.na(Harvest_Data_LB$Species)] <- "S+T"
# Add "S+T" as a level to the 'Species' factor
levels(Harvest_Data_LB$Species) <- c(levels(Harvest_Data_LB$Species), "S+T")
# Replace NA values in 'Species' with "S+T"
Harvest_Data_LB$Species[is.na(Harvest_Data_LB$Species)] <- "S+T"
# Check the unique values in the 'Species' column
unique(Harvest_Data_LB$Species)
# Check the factor levels of 'Species'
levels(Harvest_Data_LB$Species)
# Check the count of each species
table(Harvest_Data_LB$Species)
# Remove the 'T+C' level from the Species factor
Harvest_Data_LB$Species <- factor(Harvest_Data_LB$Species, levels = levels(Harvest_Data_LB$Species)[levels(Harvest_Data_LB$Species) != "T+C"])
# Check the unique values in the 'Species' column
unique(Harvest_Data_LB$Species)
# Check the factor levels of 'Species'
levels(Harvest_Data_LB$Species)
# Check the count of each species
table(Harvest_Data_LB$Species)
# Create a backup of the cleaned data
Harvest_Data_Clean <- Harvest_Data_LB


#### Percent Colonization ####
dev.off()
ggplot(Harvest_Data_Clean, aes(Species, `%_colonization`)) + geom_point()

ggplot(Harvest_Data_Clean, aes(Species, `%_colonization`)) + geom_point(colour = 5) + 
  labs(title = "Percent Colonization by Species", x = "Species", y = "Percent Colonization")

ggplot(Harvest_Data_Clean, aes(Species, `%_colonization`)) + geom_boxplot() +
  labs(title = "Percent Colonization by Species", x = "Species", y = "Percentage")

ggplot(Harvest_Data_Clean, aes(Species, `%_colonization`, fill = Species)) + geom_boxplot() + 
  labs(title = "Percent Colonization by Fungal Species", x = "Species", y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

melt(Harvest_Data_Clean, treatment.vars = 'id')
measure.vars = c('drought', 'control')
ggplot(Harvest_Data) + geom_boxplot(aes(Species, `%_colonization`))

ggplot(Harvest_Data_Clean, aes(Species, `%_colonization`, fill = Species)) + geom_boxplot() + 
  labs(title = "Percent Colonization by Fungal Species", x = "Species", y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

# Laura's suggested split by color for Percent Colonization:

ggplot(Harvest_Data_Clean, aes(Species, `%_colonization`, 
                         fill = Species,
                         color = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Percent Colonization by Fungal Species", 
       x = "Species", 
       y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

Harvest_Data_Clean$LBperccol = 
  (Harvest_Data$`%_colonization`/100)/((Harvest_Data$`%_colonization`/100)+1)*100

ggplot(Harvest_Data_Clean, aes(Species, LBperccol, 
                         fill = Species,
                         color = Treatment)) + 
  geom_boxplot(outlier.alpha = 0) + 
  labs(title = "Percent Colonization by Fungal Species", 
       x = "Species", 
       y = "Percentage") + 
  geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(Harvest_Data_Clean, aes(Species, LBperccol, 
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

p <- ggplot(Harvest_Data_Clean, aes(x = Species, y = LBperccol, fill = Species)) +
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
Harvest_Data_Perc = Harvest_Data_Clean %>% mutate(total_dry_biomass = Shoot_DW + Root_DW)
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
Percent_Col_by_Species <- aov(`%_colonization` ~ Species, data = Harvest_Data_Clean)
summary(Percent_Col_by_Species)

#Root:Shoot Ratio by Species (not significant)
Harvest_Data_Clean$Root_to_Shoot_Ratio <- Harvest_Data_Clean$Root_DW / Harvest_Data_Clean$Shoot_DW
Root_to_Shoot_Ratio_by_Species <- aov(`Root_to_Shoot_Ratio` ~ Species, data = Harvest_Data_Clean)
summary(Root_to_Shoot_Ratio_by_Species)

#Root:Shoot Ratio by Treatment (not significant)
Root_to_Shoot_Ratio_by_Treatment <- aov(`Root_to_Shoot_Ratio` ~ Treatment, data = Harvest_Data_Clean)
summary(Root_to_Shoot_Ratio_by_Treatment)

#Root DW by Species (not significant)
Root_DW_by_Species <- aov(`Root_DW` ~ Species, data = Harvest_Data_Clean)
summary(Root_DW_by_Species)

#Root DW by Treatment (not significant)
Root_DW_by_Treatment <- aov(`Root_DW` ~ Treatment, data = Harvest_Data_Clean)
summary(Root_DW_by_Treatment)

#Root FW by Species (not significant)
Root_FW_by_Species <- aov(`Root_FW_before` ~ Species, data = Harvest_Data_Clean)
summary(Root_FW_by_Species)

#Root FW by Treatment (not significant)
Root_FW_by_Treatment <- aov(`Root_FW_before` ~ Treatment, data = Harvest_Data_Clean)
summary(Root_FW_by_Treatment)

#Shoot DW by Species (Significant, RP-R+S, SP-R+S)
Shoot_DW_by_Species <- aov(`Shoot_DW` ~ Species, data = Harvest_Data_Clean)
summary(Shoot_DW_by_Species)
TukeyHSD(Shoot_DW_by_Species)

#Shoot DW by Treatment (not significant)
Shoot_DW_by_Treatment <- aov(`Shoot_DW` ~ Treatment, data = Harvest_Data_Clean)
summary(Shoot_DW_by_Treatment)

#Shoot FW by Species (Significant)
Shoot_FW_by_Species <- aov(`Shoot_FW_before` ~ Species, data = Harvest_Data_Clean)
summary(Shoot_FW_by_Species)

#Shoot FW by Treatment (not significant)
Shoot_FW_by_Treatment <- aov(`Shoot_FW_before` ~ Treatment, data = Harvest_Data_Clean)
summary(Shoot_FW_by_Treatment)

#Stem Diameter by Species (not significant)
Stem_diameter_by_Species <- aov(`Stem_diameter` ~ Species, data = Harvest_Data_Clean)
summary(Stem_diameter_by_Species)

#Stem Diameter by Treatment (not significant)
Stem_diameter_by_Treatment <- aov(`Stem_diameter` ~ Treatment, data = Harvest_Data_Clean)
summary(Stem_diameter_by_Treatment)

#Root MC by Species (not significant)
Root_MC_by_Species <- aov(`Root_%_MC` ~ Species, data = Harvest_Data_Clean)
summary(Root_MC_by_Species)

#Root MC by Treatment (not significant)
Root_MC_by_Treatment <- aov(`Root_%_MC` ~ Treatment, data = Harvest_Data_Clean)
summary(Root_MC_by_Treatment)

#Shoot MC by Species (not significant)
Shoot_MC_by_Species <- aov(`Shoot_%_MC` ~ Species, data = Harvest_Data_Clean)
summary(Shoot_MC_by_Species)

#Shoot MC by Treatment (Significant)
Shoot_MC_by_Treatment <- aov(`Shoot_%_MC` ~ Treatment, data = Harvest_Data_Clean)
summary(Shoot_MC_by_Treatment)

#Total Dry Biomass by Species (not significant, but indeed RP plants were a bit smaller than R+S and other)
totalmassanova = aov(total_dry_mass ~ Species, data = Harvest_Data_Clean)
summary(totalmassanova)
TukeyHSD(totalmassanova)

#Total Dry Biomass by Treatment (not significant)
totalmassanovabytreatment = aov(total_dry_mass ~ Treatment, data = Harvest_Data_Clean)
summary(totalmassanovabytreatment)

#Final Weight by Species (not significant)
Final_Weight_by_Species <- aov(`Final_weight` ~ Species, data = Harvest_Data_Clean)
summary(Final_Weight_by_Species)

#Final Weight by Treatment (significant)
Final_Weight_by_Treatment <- aov(`Final_weight` ~ Treatment, data = Harvest_Data_Clean)
summary(Final_Weight_by_Treatment)




#### Boxplot Graphs ####

#Root:Shoot by Species and Treatment
ggplot(Harvest_Data_Clean, aes(x = Species, y = Root_to_Shoot_Ratio, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Root to Shoot Ratio By Species and Treatment", x = "Species", y = "Root to Shoot Ratio") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Root:Shoot by Species
boxplot(Harvest_Data_Clean$Root_to_Shoot_Ratio ~ Harvest_Data$Species)
ggplot(Harvest_Data_Clean, aes(Species, Root_to_Shoot_Ratio)) +geom_boxplot() +
  labs(title = "Root to Shoot Ratio by Species", y = "Root to Shoot Ratio")

#Root:Shoot by Treatment (provides no meaningful info)
ggplot(Harvest_Data_Clean, aes(Treatment, Root_to_Shoot_Ratio)) +geom_boxplot() +
  labs(title = "Root to Shoot Ratio by Treatment", y = "Root to Shoot Ratio")

#Shoot DW by Species and Treatment
boxplot(Harvest_Data_Clean$Shoot_DW~Harvest_Data$Species+Harvest_Data_Clean$Treatment)
ggplot(Harvest_Data_Clean, aes(x = Species, y = Shoot_DW, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Dry Weight By Species and Treatment", x = "Species", y = "Shoot Dry Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Shoot FW by Species and Treatment
boxplot(Harvest_Data_Clean$Shoot_FW_before~Harvest_Data_Clean$Species+Harvest_Data_Clean$Treatment)
ggplot(Harvest_Data_Clean, aes(x = Species, y = Shoot_FW_before, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Fresh Weight By Species and Treatment", x = "Species", y = "Shoot Fresh Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Root % MC by Species and Treatment
ggplot(Harvest_Data_Clean, aes(x = Species, y = `Root_%_MC`, fill = Treatment)) + 
  geom_boxplot() + 
  ylim(65, NA) +
  labs(title = "Root Moisture Content By Species and Treatment", x = "Species", y = "Root Moisture Content (%)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Root % MC by Species 
ggplot(Harvest_Data_Clean, aes(Species, `Root_%_MC`)) + geom_boxplot() +
  ylim(65, NA) +
  labs(y = "Root Moisture Content (%)")

#Root % MC by Treatment 
ggplot(Harvest_Data_Clean, aes(Treatment, `Root_%_MC`)) + geom_boxplot() +
  ylim(65, NA) +
  labs(title= "Root Moisture Content by Treatment", y = "Root Moisture Content (%)")

#Shoot % MC by Species and Treatment
ggplot(Harvest_Data_Clean, aes(x = Species, y = `Shoot_%_MC`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Moisture Content By Species and Treatment", x = "Species", y = "Shoot Moisture Content (%)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Shoot % MC by Species
ggplot(Harvest_Data_Clean, aes(Species, `Shoot_%_MC`)) + geom_boxplot() +
  ylim(55, NA) +
  labs(title = "Shoot Moisture Content by Species", y = "Shoot Moisture Content (%)")

#Shoot % MC by Treatment
ggplot(Harvest_Data_Clean, aes(Treatment, `Shoot_%_MC`)) + geom_boxplot() + 
  labs(title = "Shoot Moisture Content by Treatment", y = "Shoot Moisture Content (%)")

#Stem Diameter by Species and Treatment
ggplot(Harvest_Data_Clean, aes(x = Species, y = `Stem_diameter`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Stem Diameter By Species and Treatment", x = "Species", y = "Stem Diameter (cm)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Stem Diameter by Species
ggplot(Harvest_Data_Clean, aes (Species, `Stem_diameter`)) + geom_boxplot() +
  labs(y = "Stem Diameter (cm)")

#Stem Diameter by Treatment
ggplot(Harvest_Data_Clean, aes (Treatment, `Stem_diameter`)) + geom_boxplot() +
  labs(y = "Stem Diameter (cm)")

#Shoot FW by Species and Treatment
ggplot(Harvest_Data_Clean, aes(x = Species, y = `Shoot_FW_after`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Shoot Fresh Weight By Species and Treatment", x = "Species", y = "Shoot Fresh Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Shoot FW by Species
ggplot(Harvest_Data_Clean, aes (Species, `Shoot_FW_after`)) + geom_boxplot() +
  labs(y = "Shoot Fresh Weight (g)")

#Root FW by Species and Treatment
ggplot(Harvest_Data_Clean, aes(x = Species, y = `Root_FW_after`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Root Fresh Weight By Species and Treatment", x = "Species", y = "Root Fresh Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Root FW by Species
ggplot(Harvest_Data_Clean, aes (Species, `Root_FW_after`)) + geom_boxplot() +
  labs(y = "Root Fresh Weight (g)")

#Final Weight by Species and Treatment
Harvest_Data_Clean$Final_weight <- as.numeric(Harvest_Data_Clean$Final_weight)
sum(is.na(Harvest_Data_Clean$Final_weight))
Harvest_Data_Clean[is.na(Harvest_Data_Clean$Final_weight), "Final_weight"]
Harvest_Data_Clean <- Harvest_Data_Clean[!is.na(Harvest_Data_Clean$Final_weight), ]

ggplot(Harvest_Data_Clean, aes(x = Species, y = `Final_weight`, fill = Treatment)) + 
  geom_boxplot() + 
  ylim(NA, 90) +
  labs(title = "Final Total Weight By Species and Treatment", x = "Species", y = "Final Weight (g)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Leaf SA by Species and Treatment
ggplot(Harvest_Data_Clean, aes(x = Species, y = `Av_Needle_SA`, fill = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Leaf Surface Area By Species and Treatment", x = "Species", y = "Leaf Surface Area (cm)") + 
  scale_fill_manual(values = c("drought" = "red", "control" = "deepskyblue"))

#Leaf SA by Species
ggplot(Harvest_Data_Clean, aes (Species, `Av_Needle_SA`)) + geom_boxplot() +
  labs(y = "Leaf Surface Area (cm)")

#Leaf SA by Treatment
ggplot(Harvest_Data_Clean, aes (Treatment, `Av_Needle_SA`)) + geom_boxplot() +
  labs(y = "Leaf Surface Area (cm)")

#% Colonization by Total Dry Biomass
ggplot(subset(Harvest_Data_Clean, Species != "NM"), aes(x = perccol, y = total_dry_mass, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Percent Colonization by Total Dry Biomass", 
       x = "Colonization (%)", 
       y = "Total Dry Biomass (g)")

#% Colonization by Total Dry Biomass (without values = 0)
ggplot(subset(Harvest_Data_Clean, Species != "NM" & perccol != 0), aes(x = perccol, y = total_dry_mass, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Percent Colonization by Total Dry Biomass", 
       x = "Colonization (%)", 
       y = "Total Dry Biomass (g)")






#### Attempt at Time Differences- Lysimetry stuff ####
library(readxl)
Lysimetry_Updated <- read_excel("Lysimetry_Updated.xlsx")
Lysimetry_Super_Update <- Lysimetry_Updated

# Convert the date/time columns to POSIXct format
Lysimetry_Super_Update$Time06_19_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/19/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_21_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/21/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_22_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/22/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_23_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/23/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_26_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/26/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_27_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/27/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_28_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/28/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time06_29_23 <- as.POSIXct(Lysimetry_Super_Update$`Time06/29/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_03_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/03/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_05_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/05/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_06_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/06/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_07_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/07/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_10_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/10/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_11_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/11/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_12_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/12/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_13_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/13/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_17_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/17/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_18_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/18/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_19_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/19/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_20_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/20/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_24_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/24/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_25_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/25/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_26_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/26/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_27_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/27/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time07_31_23 <- as.POSIXct(Lysimetry_Super_Update$`Time07/31/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_01_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/01/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_02_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/02/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_03_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/03/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_07_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/07/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_08_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/08/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_09_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/09/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_10_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/10/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_14_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/14/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_15_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/15/23`, format="%m/%d/%y %H:%M")

Lysimetry_Super_Update$Time08_16_23 <- as.POSIXct(Lysimetry_Super_Update$`Time08/16/23`, format="%m/%d/%y %H:%M")

# List of time columns in order
time_columns <- c("Time06_19_23", "Time06_21_23", "Time06_22_23", "Time06_23_23", "Time06_26_23", 
                  "Time06_27_23", "Time06_28_23", "Time06_29_23", "Time07_03_23", "Time07_05_23", 
                  "Time07_06_23", "Time07_07_23", "Time07_10_23", "Time07_11_23", "Time07_12_23", 
                  "Time07_13_23", "Time07_17_23", "Time07_18_23", "Time07_19_23", "Time07_20_23", 
                  "Time07_24_23", "Time07_25_23", "Time07_26_23", "Time07_27_23", "Time07_31_23", 
                  "Time08_01_23", "Time08_02_23", "Time08_03_23", "Time08_07_23", "Time08_08_23", 
                  "Time08_09_23", "Time08_10_23", "Time08_14_23", "Time08_15_23", "Time08_16_23")

# List of date columns in the format "TimeMM/DD/YY"
time_columns <- grep("Time", colnames(Lysimetry_Super_Update), value = TRUE)

#Create columns for time difference
Lysimetry_Super_Update$Time_diff_06_19_06_21 <- 
  Lysimetry_Super_Update$Time06_21_23 - Lysimetry_Super_Update$Time06_19_23

Lysimetry_Super_Update$Time_diff_06_21_06_22 <- 
  Lysimetry_Super_Update$Time06_22_23 - Lysimetry_Super_Update$Time06_21_23

Lysimetry_Super_Update$Time_diff_06_22_06_23 <- 
  Lysimetry_Super_Update$Time06_23_23 - Lysimetry_Super_Update$Time06_22_23

Lysimetry_Super_Update$Time_diff_06_23_06_26 <- 
  Lysimetry_Super_Update$Time06_26_23 - Lysimetry_Super_Update$Time06_23_23

Lysimetry_Super_Update$Time_diff_06_26_06_27 <- 
  Lysimetry_Super_Update$Time06_27_23 - Lysimetry_Super_Update$Time06_26_23

Lysimetry_Super_Update$Time_diff_06_27_06_28 <- 
  Lysimetry_Super_Update$Time06_28_23 - Lysimetry_Super_Update$Time06_27_23

Lysimetry_Super_Update$Time_diff_06_28_06_29 <- 
  Lysimetry_Super_Update$Time06_29_23 - Lysimetry_Super_Update$Time06_28_23

Lysimetry_Super_Update$Time_diff_06_29_07_03 <- 
  Lysimetry_Super_Update$Time07_03_23 - Lysimetry_Super_Update$Time06_29_23

Lysimetry_Super_Update$Time_diff_07_03_07_05 <- 
  Lysimetry_Super_Update$Time07_05_23 - Lysimetry_Super_Update$Time07_03_23

Lysimetry_Super_Update$Time_diff_07_05_07_06 <- 
  Lysimetry_Super_Update$Time07_06_23 - Lysimetry_Super_Update$Time07_05_23

Lysimetry_Super_Update$Time_diff_07_06_07_07 <- 
  Lysimetry_Super_Update$Time07_07_23 - Lysimetry_Super_Update$Time07_06_23

Lysimetry_Super_Update$Time_diff_07_07_07_10 <- 
  Lysimetry_Super_Update$Time07_10_23 - Lysimetry_Super_Update$Time07_07_23

Lysimetry_Super_Update$Time_diff_07_10_07_11 <- 
  Lysimetry_Super_Update$Time07_11_23 - Lysimetry_Super_Update$Time07_10_23

Lysimetry_Super_Update$Time_diff_07_11_07_12 <- 
  Lysimetry_Super_Update$Time07_12_23 - Lysimetry_Super_Update$Time07_11_23

Lysimetry_Super_Update$Time_diff_07_11_07_13 <- 
  Lysimetry_Super_Update$Time07_13_23 - Lysimetry_Super_Update$Time07_11_23

Lysimetry_Super_Update$Time_diff_07_13_07_17 <- 
  Lysimetry_Super_Update$Time07_17_23 - Lysimetry_Super_Update$Time07_13_23

Lysimetry_Super_Update$Time_diff_07_17_07_18 <- 
  Lysimetry_Super_Update$Time07_18_23 - Lysimetry_Super_Update$Time07_17_23

Lysimetry_Super_Update$Time_diff_07_18_07_19 <- 
  Lysimetry_Super_Update$Time07_19_23 - Lysimetry_Super_Update$Time07_18_23

Lysimetry_Super_Update$Time_diff_07_18_07_20 <- 
  Lysimetry_Super_Update$Time07_20_23 - Lysimetry_Super_Update$Time07_18_23

Lysimetry_Super_Update$Time_diff_07_20_07_24 <- 
  Lysimetry_Super_Update$Time07_24_23 - Lysimetry_Super_Update$Time07_20_23

Lysimetry_Super_Update$Time_diff_07_24_07_25 <- 
  Lysimetry_Super_Update$Time07_25_23 - Lysimetry_Super_Update$Time07_24_23

Lysimetry_Super_Update$Time_diff_07_25_07_26 <- 
  Lysimetry_Super_Update$Time07_26_23 - Lysimetry_Super_Update$Time07_25_23

Lysimetry_Super_Update$Time_diff_07_24_07_27 <- 
  Lysimetry_Super_Update$Time07_27_23 - Lysimetry_Super_Update$Time07_24_23

Lysimetry_Super_Update$Time_diff_07_27_07_31 <- 
  Lysimetry_Super_Update$Time07_31_23 - Lysimetry_Super_Update$Time07_27_23

Lysimetry_Super_Update$Time_diff_07_31_08_01 <- 
  Lysimetry_Super_Update$Time08_01_23 - Lysimetry_Super_Update$Time07_31_23

Lysimetry_Super_Update$Time_diff_08_01_08_02 <- 
  Lysimetry_Super_Update$Time08_02_23 - Lysimetry_Super_Update$Time08_01_23

Lysimetry_Super_Update$Time_diff_08_01_08_03 <- 
  Lysimetry_Super_Update$Time08_03_23 - Lysimetry_Super_Update$Time08_01_23

Lysimetry_Super_Update$Time_diff_08_03_08_07 <- 
  Lysimetry_Super_Update$Time08_07_23 - Lysimetry_Super_Update$Time08_03_23

Lysimetry_Super_Update$Time_diff_08_07_08_08 <- 
  Lysimetry_Super_Update$Time08_08_23 - Lysimetry_Super_Update$Time08_07_23

Lysimetry_Super_Update$Time_diff_08_08_08_09 <- 
  Lysimetry_Super_Update$Time08_09_23 - Lysimetry_Super_Update$Time08_08_23

Lysimetry_Super_Update$Time_diff_08_07_08_10 <- 
  Lysimetry_Super_Update$Time08_10_23 - Lysimetry_Super_Update$Time08_07_23

Lysimetry_Super_Update$Time_diff_08_10_08_14 <- 
  Lysimetry_Super_Update$Time08_14_23 - Lysimetry_Super_Update$Time08_10_23

Lysimetry_Super_Update$Time_diff_08_14_08_15 <- 
  Lysimetry_Super_Update$Time08_15_23 - Lysimetry_Super_Update$Time08_14_23

Lysimetry_Super_Update$Time_diff_08_15_08_16 <- 
  Lysimetry_Super_Update$Time08_16_23 - Lysimetry_Super_Update$Time08_15_23

# Convert difftime to numeric in hours
Lysimetry_Super_Update$Time_diff_06_19_06_21 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_19_06_21, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_21_06_22 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_21_06_22, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_22_06_23 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_22_06_23, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_23_06_26 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_23_06_26, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_26_06_27 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_26_06_27, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_27_06_28 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_27_06_28, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_28_06_29 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_28_06_29, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_06_29_07_03 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_06_29_07_03, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_03_07_05 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_03_07_05, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_05_07_06 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_05_07_06, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_06_07_07 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_06_07_07, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_07_07_10 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_07_07_10, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_10_07_11 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_10_07_11, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_11_07_12 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_11_07_12, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_11_07_13 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_11_07_13, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_13_07_17 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_13_07_17, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_17_07_18 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_17_07_18, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_18_07_19 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_18_07_19, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_18_07_20 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_18_07_20, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_20_07_24 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_20_07_24, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_24_07_25 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_24_07_25, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_25_07_26 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_25_07_26, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_24_07_27 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_24_07_27, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_27_07_31 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_27_07_31, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_07_31_08_01 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_07_31_08_01, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_01_08_02 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_01_08_02, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_01_08_03 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_01_08_03, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_03_08_07 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_03_08_07, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_07_08_08 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_07_08_08, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_08_08_09 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_08_08_09, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_07_08_10 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_07_08_10, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_10_08_14 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_10_08_14, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_14_08_15 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_14_08_15, 
  units = "hours"
)
Lysimetry_Super_Update$Time_diff_08_15_08_16 <- as.numeric(
  Lysimetry_Super_Update$Time_diff_08_15_08_16, 
  units = "hours"
)

#Calculate weight differences
Lysimetry_Super_Update$Weight_diff_06_19_06_21 <- 
  Lysimetry_Super_Update$`sample06/19/23` - Lysimetry_Super_Update$`sample06/21/23`

Lysimetry_Super_Update$Weight_diff_06_21_06_22 <- 
  Lysimetry_Super_Update$`sample06/21/23` - Lysimetry_Super_Update$`sample06/22/23`

Lysimetry_Super_Update$Weight_diff_06_22_06_23 <- 
  Lysimetry_Super_Update$`sample06/22/23` - Lysimetry_Super_Update$`sample06/23/23`

Lysimetry_Super_Update$Weight_diff_06_23_06_26 <- 
  Lysimetry_Super_Update$`water06/23/23` - Lysimetry_Super_Update$`sample06/26/23`

Lysimetry_Super_Update$Weight_diff_06_26_06_27 <- 
  Lysimetry_Super_Update$`water06/26/23` - Lysimetry_Super_Update$`sample06/27/23`

Lysimetry_Super_Update$Weight_diff_06_27_06_28 <- 
  Lysimetry_Super_Update$`sample06/27/23` - Lysimetry_Super_Update$`sample06/28/23`

Lysimetry_Super_Update$Weight_diff_06_28_06_29 <- 
  Lysimetry_Super_Update$`sample06/28/23` - Lysimetry_Super_Update$`sample06/29/23`

Lysimetry_Super_Update$Weight_diff_06_29_07_03 <- 
  Lysimetry_Super_Update$`water06/29/23` - Lysimetry_Super_Update$`sample07/03/23`

Lysimetry_Super_Update$Weight_diff_07_03_07_05 <- 
  Lysimetry_Super_Update$`water07/03/23` - Lysimetry_Super_Update$`sample07/05/23`

Lysimetry_Super_Update$Weight_diff_07_05_07_06 <- 
  Lysimetry_Super_Update$`sample07/05/23` - Lysimetry_Super_Update$`sample07/06/23`

Lysimetry_Super_Update$Weight_diff_07_06_07_07 <- 
  Lysimetry_Super_Update$`water07/06/23` - Lysimetry_Super_Update$`sample07/07/23`

Lysimetry_Super_Update$Weight_diff_07_07_07_10 <- 
  Lysimetry_Super_Update$`sample07/07/23` - Lysimetry_Super_Update$`sample07/10/23`

Lysimetry_Super_Update$Weight_diff_07_10_07_11 <- 
  Lysimetry_Super_Update$`water07/10/23` - Lysimetry_Super_Update$`sample07/11/23`

Lysimetry_Super_Update$Weight_diff_07_11_07_12 <- 
  Lysimetry_Super_Update$`sample07/11/23` - Lysimetry_Super_Update$`sample07/12/23`

Lysimetry_Super_Update$Weight_diff_07_11_07_13 <- 
  Lysimetry_Super_Update$`sample07/11/23` - Lysimetry_Super_Update$`sample07/13/23`

Lysimetry_Super_Update$Weight_diff_07_13_07_17 <- 
  Lysimetry_Super_Update$`water07/13/23` - Lysimetry_Super_Update$`sample07/17/23`

Lysimetry_Super_Update$Weight_diff_07_17_07_18 <- 
  Lysimetry_Super_Update$`water07/17/23` - Lysimetry_Super_Update$`sample07/18/23`

Lysimetry_Super_Update$Weight_diff_07_18_07_19 <- 
  Lysimetry_Super_Update$`sample07/18/23` - Lysimetry_Super_Update$`sample07/19/23`

Lysimetry_Super_Update$Weight_diff_07_18_07_20 <- 
  Lysimetry_Super_Update$`sample07/18/23` - Lysimetry_Super_Update$`sample07/20/23`

Lysimetry_Super_Update$Weight_diff_07_20_07_24 <- 
  Lysimetry_Super_Update$`water07/20/23` - Lysimetry_Super_Update$`sample07/24/23`

Lysimetry_Super_Update$Weight_diff_07_24_07_25 <- 
  Lysimetry_Super_Update$`water07/24/23` - Lysimetry_Super_Update$`sample07/25/23`

Lysimetry_Super_Update$Weight_diff_07_25_07_26 <- 
  Lysimetry_Super_Update$`sample07/25/23` - Lysimetry_Super_Update$`sample07/26/23`

Lysimetry_Super_Update$Weight_diff_07_24_07_27 <- 
  Lysimetry_Super_Update$`water07/24/23` - Lysimetry_Super_Update$`sample07/27/23`

Lysimetry_Super_Update$Weight_diff_07_27_07_31 <- 
  Lysimetry_Super_Update$`water07/27/23` - Lysimetry_Super_Update$`sample07/31/23`

Lysimetry_Super_Update$Weight_diff_07_31_08_01 <- 
  Lysimetry_Super_Update$`water07/31/23` - Lysimetry_Super_Update$`sample08/01/23`

Lysimetry_Super_Update$Weight_diff_08_01_08_02 <- 
  Lysimetry_Super_Update$`sample08/01/23` - Lysimetry_Super_Update$`sample08/02/23`

Lysimetry_Super_Update$Weight_diff_08_01_08_03 <- 
  Lysimetry_Super_Update$`sample08/01/23` - Lysimetry_Super_Update$`sample08/03/23`

Lysimetry_Super_Update$Weight_diff_08_03_08_07 <- 
  Lysimetry_Super_Update$`water08/03/23` - Lysimetry_Super_Update$`sample08/07/23`

Lysimetry_Super_Update$Weight_diff_08_07_08_08 <- 
  Lysimetry_Super_Update$`water08/07/23` - Lysimetry_Super_Update$`sample08/08/23`

Lysimetry_Super_Update$Weight_diff_08_08_08_09 <- 
  Lysimetry_Super_Update$`sample08/08/23` - Lysimetry_Super_Update$`sample08/09/23`

Lysimetry_Super_Update$Weight_diff_08_07_08_10 <- 
  Lysimetry_Super_Update$`water08/07/23` - Lysimetry_Super_Update$`sample08/10/23`

Lysimetry_Super_Update$Weight_diff_08_10_08_14 <- 
  Lysimetry_Super_Update$`water08/10/23` - Lysimetry_Super_Update$`sample08/14/23`

Lysimetry_Super_Update$Weight_diff_08_14_08_15 <- 
  Lysimetry_Super_Update$`sample08/14/23` - Lysimetry_Super_Update$`sample08/15/23`

Lysimetry_Super_Update$Weight_diff_08_15_08_16 <- 
  Lysimetry_Super_Update$`sample08/15/23` - Lysimetry_Super_Update$`sample08/16/23`

#Divide time difference by transpiration rate 
Lysimetry_Super_Update$Transpiration_rate_06_19_06_21 <- 
  Lysimetry_Super_Update$Weight_diff_06_19_06_21 / as.numeric(Lysimetry_Super_Update$Time_diff_06_19_06_21, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_21_06_22 <- 
  Lysimetry_Super_Update$Weight_diff_06_21_06_22 / as.numeric(Lysimetry_Super_Update$Time_diff_06_21_06_22, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_22_06_23 <- 
  Lysimetry_Super_Update$Weight_diff_06_22_06_23 / as.numeric(Lysimetry_Super_Update$Time_diff_06_22_06_23, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_23_06_26 <- 
  Lysimetry_Super_Update$Weight_diff_06_23_06_26 / as.numeric(Lysimetry_Super_Update$Time_diff_06_23_06_26, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_26_06_27 <- 
  Lysimetry_Super_Update$Weight_diff_06_26_06_27 / as.numeric(Lysimetry_Super_Update$Time_diff_06_26_06_27, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_27_06_28 <- 
  Lysimetry_Super_Update$Weight_diff_06_27_06_28 / as.numeric(Lysimetry_Super_Update$Time_diff_06_27_06_28, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_28_06_29 <- 
  Lysimetry_Super_Update$Weight_diff_06_28_06_29 / as.numeric(Lysimetry_Super_Update$Time_diff_06_28_06_29, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_06_29_07_03 <- 
  Lysimetry_Super_Update$Weight_diff_06_29_07_03 / as.numeric(Lysimetry_Super_Update$Time_diff_06_29_07_03, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_03_07_05 <- 
  Lysimetry_Super_Update$Weight_diff_07_03_07_05 / as.numeric(Lysimetry_Super_Update$Time_diff_07_03_07_05, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_05_07_06 <- 
  Lysimetry_Super_Update$Weight_diff_07_05_07_06 / as.numeric(Lysimetry_Super_Update$Time_diff_07_05_07_06, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_06_07_07 <- 
  Lysimetry_Super_Update$Weight_diff_07_06_07_07 / as.numeric(Lysimetry_Super_Update$Time_diff_07_06_07_07, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_07_07_10 <- 
  Lysimetry_Super_Update$Weight_diff_07_07_07_10 / as.numeric(Lysimetry_Super_Update$Time_diff_07_07_07_10, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_10_07_11 <- 
  Lysimetry_Super_Update$Weight_diff_07_10_07_11 / as.numeric(Lysimetry_Super_Update$Time_diff_07_10_07_11, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_11_07_12 <- 
  Lysimetry_Super_Update$Weight_diff_07_11_07_12 / as.numeric(Lysimetry_Super_Update$Time_diff_07_11_07_12, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_11_07_13 <- 
  Lysimetry_Super_Update$Weight_diff_07_11_07_13 / as.numeric(Lysimetry_Super_Update$Time_diff_07_11_07_13, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_13_07_17 <- 
  Lysimetry_Super_Update$Weight_diff_07_13_07_17 / as.numeric(Lysimetry_Super_Update$Time_diff_07_13_07_17, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_17_07_18 <- 
  Lysimetry_Super_Update$Weight_diff_07_17_07_18 / as.numeric(Lysimetry_Super_Update$Time_diff_07_17_07_18, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_18_07_19 <- 
  Lysimetry_Super_Update$Weight_diff_07_18_07_19 / as.numeric(Lysimetry_Super_Update$Time_diff_07_18_07_19, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_18_07_20 <- 
  Lysimetry_Super_Update$Weight_diff_07_18_07_20 / as.numeric(Lysimetry_Super_Update$Time_diff_07_18_07_20, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_20_07_24 <- 
  Lysimetry_Super_Update$Weight_diff_07_20_07_24 / as.numeric(Lysimetry_Super_Update$Time_diff_07_20_07_24, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_24_07_25 <- 
  Lysimetry_Super_Update$Weight_diff_07_24_07_25 / as.numeric(Lysimetry_Super_Update$Time_diff_07_24_07_25, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_25_07_26 <- 
  Lysimetry_Super_Update$Weight_diff_07_25_07_26 / as.numeric(Lysimetry_Super_Update$Time_diff_07_25_07_26, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_24_07_27 <- 
  Lysimetry_Super_Update$Weight_diff_07_24_07_27 / as.numeric(Lysimetry_Super_Update$Time_diff_07_24_07_27, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_27_07_31 <- 
  Lysimetry_Super_Update$Weight_diff_07_27_07_31 / as.numeric(Lysimetry_Super_Update$Time_diff_07_27_07_31, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_07_31_08_01 <- 
  Lysimetry_Super_Update$Weight_diff_07_31_08_01 / as.numeric(Lysimetry_Super_Update$Time_diff_07_31_08_01, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_01_08_02 <- 
  Lysimetry_Super_Update$Weight_diff_08_01_08_02 / as.numeric(Lysimetry_Super_Update$Time_diff_08_01_08_02, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_01_08_03 <- 
  Lysimetry_Super_Update$Weight_diff_08_01_08_03 / as.numeric(Lysimetry_Super_Update$Time_diff_08_01_08_03, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_03_08_07 <- 
  Lysimetry_Super_Update$Weight_diff_08_03_08_07 / as.numeric(Lysimetry_Super_Update$Time_diff_08_03_08_07, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_07_08_08 <- 
  Lysimetry_Super_Update$Weight_diff_08_07_08_08 / as.numeric(Lysimetry_Super_Update$Time_diff_08_07_08_08, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_08_08_09 <- 
  Lysimetry_Super_Update$Weight_diff_08_08_08_09 / as.numeric(Lysimetry_Super_Update$Time_diff_08_08_08_09, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_07_08_10 <- 
  Lysimetry_Super_Update$Weight_diff_08_07_08_10 / as.numeric(Lysimetry_Super_Update$Time_diff_08_07_08_10, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_10_08_14 <- 
  Lysimetry_Super_Update$Weight_diff_08_10_08_14 / as.numeric(Lysimetry_Super_Update$Time_diff_08_10_08_14, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_14_08_15 <- 
  Lysimetry_Super_Update$Weight_diff_08_14_08_15 / as.numeric(Lysimetry_Super_Update$Time_diff_08_14_08_15, units = "hours")

Lysimetry_Super_Update$Transpiration_rate_08_15_08_16 <- 
  Lysimetry_Super_Update$Weight_diff_08_15_08_16 / as.numeric(Lysimetry_Super_Update$Time_diff_08_15_08_16, units = "hours")

# Create a dataframe with the columns of interest
Lysimetry_calculated <- data.frame(
  ID = Lysimetry_Super_Update$ID, # Plant IDs
  Interval_1 = "06/19 - 06/21", # Add a label for the first time interval
  Weight_Diff_1 = Lysimetry_Super_Update$Weight_diff_06_19_06_21,
  Time_Diff_1 = as.numeric(Lysimetry_Super_Update$Time_diff_06_19_06_21, units = "hours"),
  Transpiration_Rate_1 = Lysimetry_Super_Update$Transpiration_rate_06_19_06_21,
  
  Interval_2 = "06/21 - 06/22", # Add a label for the second time interval
  Weight_Diff_2 = Lysimetry_Super_Update$Weight_diff_06_21_06_22,
  Time_Diff_2 = as.numeric(Lysimetry_Super_Update$Time_diff_06_21_06_22, units = "hours"),
  Transpiration_Rate_2 = Lysimetry_Super_Update$Transpiration_rate_06_21_06_22,
  
  Interval_3 = "06/22 - 06/23", # Add a label for the second time interval
  Weight_Diff_3 = Lysimetry_Super_Update$Weight_diff_06_22_06_23,
  Time_Diff_3 = as.numeric(Lysimetry_Super_Update$Time_diff_06_22_06_23, units = "hours"),
  Transpiration_Rate_3 = Lysimetry_Super_Update$Transpiration_rate_06_22_06_23,
 
   Interval_4 = "06/23 - 06/26", # Add a label for the second time interval
  Weight_Diff_4 = Lysimetry_Super_Update$Weight_diff_06_23_06_26,
  Time_Diff_4 = as.numeric(Lysimetry_Super_Update$Time_diff_06_23_06_26, units = "hours"),
  Transpiration_Rate_4 = Lysimetry_Super_Update$Transpiration_rate_06_23_06_26,

  Interval_5 = "06/26 - 06/27", # Add a label for the second time interval
  Weight_Diff_5 = Lysimetry_Super_Update$Weight_diff_06_26_06_27,
  Time_Diff_5 = as.numeric(Lysimetry_Super_Update$Time_diff_06_26_06_27, units = "hours"),
  Transpiration_Rate_5 = Lysimetry_Super_Update$Transpiration_rate_06_26_06_27,
  
  Interval_6 = "06/27 - 06/28", # Add a label for the second time interval
  Weight_Diff_6 = Lysimetry_Super_Update$Weight_diff_06_27_06_28,
  Time_Diff_6 = as.numeric(Lysimetry_Super_Update$Time_diff_06_27_06_28, units = "hours"),
  Transpiration_Rate_6 = Lysimetry_Super_Update$Transpiration_rate_06_27_06_28,
  
  Interval_7 = "06/28 - 06/29", # Add a label for the second time interval
  Weight_Diff_7 = Lysimetry_Super_Update$Weight_diff_06_28_06_29,
  Time_Diff_7 = as.numeric(Lysimetry_Super_Update$Time_diff_06_28_06_29, units = "hours"),
  Transpiration_Rate_7 = Lysimetry_Super_Update$Transpiration_rate_06_28_06_29,
  
  Interval_8 = "06/29 - 07/03", # Add a label for the second time interval
  Weight_Diff_8 = Lysimetry_Super_Update$Weight_diff_06_29_07_03,
  Time_Diff_8 = as.numeric(Lysimetry_Super_Update$Time_diff_06_29_07_03, units = "hours"),
  Transpiration_Rate_8 = Lysimetry_Super_Update$Transpiration_rate_06_29_07_03,
  
  Interval_9 = "07/03 - 07/05", # Add a label for the second time interval
  Weight_Diff_9 = Lysimetry_Super_Update$Weight_diff_07_03_07_05,
  Time_Diff_9 = as.numeric(Lysimetry_Super_Update$Time_diff_07_03_07_05, units = "hours"),
  Transpiration_Rate_9 = Lysimetry_Super_Update$Transpiration_rate_07_03_07_05,
  
  Interval_10 = "07/05 - 07/06", # Add a label for the second time interval
  Weight_Diff_10 = Lysimetry_Super_Update$Weight_diff_07_05_07_06,
  Time_Diff_10 = as.numeric(Lysimetry_Super_Update$Time_diff_07_05_07_06, units = "hours"),
  Transpiration_Rate_10 = Lysimetry_Super_Update$Transpiration_rate_07_05_07_06,
  
  Interval_11 = "07/06 - 07/07", # Add a label for the second time interval
  Weight_Diff_11 = Lysimetry_Super_Update$Weight_diff_07_06_07_07,
  Time_Diff_11 = as.numeric(Lysimetry_Super_Update$Time_diff_07_06_07_07, units = "hours"),
  Transpiration_Rate_11 = Lysimetry_Super_Update$Transpiration_rate_07_06_07_07,
  
  Interval_12 = "07/07 - 07/10", # Add a label for the second time interval
  Weight_Diff_12 = Lysimetry_Super_Update$Weight_diff_07_07_07_10,
  Time_Diff_12 = as.numeric(Lysimetry_Super_Update$Time_diff_07_07_07_10, units = "hours"),
  Transpiration_Rate_12 = Lysimetry_Super_Update$Transpiration_rate_07_07_07_10,
  
  Interval_13 = "07/10 - 07/11", # Add a label for the second time interval
  Weight_Diff_13 = Lysimetry_Super_Update$Weight_diff_07_10_07_11,
  Time_Diff_13 = as.numeric(Lysimetry_Super_Update$Time_diff_07_10_07_11, units = "hours"),
  Transpiration_Rate_13 = Lysimetry_Super_Update$Transpiration_rate_07_10_07_11,
  
  Interval_14 = "07/11 - 07/12", # Add a label for the second time interval
  Weight_Diff_14 = Lysimetry_Super_Update$Weight_diff_07_11_07_12,
  Time_Diff_14 = as.numeric(Lysimetry_Super_Update$Time_diff_07_11_07_12, units = "hours"),
  Transpiration_Rate_14 = Lysimetry_Super_Update$Transpiration_rate_07_11_07_12, 
  
  Interval_15 = "07/11 - 07/13", # Add a label for the second time interval
  Weight_Diff_15 = Lysimetry_Super_Update$Weight_diff_07_11_07_13,
  Time_Diff_15 = as.numeric(Lysimetry_Super_Update$Time_diff_07_11_07_13, units = "hours"),
  Transpiration_Rate_15 = Lysimetry_Super_Update$Transpiration_rate_07_11_07_13,
  
  Interval_16 = "07/13 - 07/17", # Add a label for the second time interval
  Weight_Diff_16 = Lysimetry_Super_Update$Weight_diff_07_13_07_17,
  Time_Diff_16 = as.numeric(Lysimetry_Super_Update$Time_diff_07_13_07_17, units = "hours"),
  Transpiration_Rate_16 = Lysimetry_Super_Update$Transpiration_rate_07_13_07_17,
  
  Interval_17 = "07/17 - 07/18", # Add a label for the second time interval
  Weight_Diff_17 = Lysimetry_Super_Update$Weight_diff_07_17_07_18,
  Time_Diff_17 = as.numeric(Lysimetry_Super_Update$Time_diff_07_17_07_18, units = "hours"),
  Transpiration_Rate_17 = Lysimetry_Super_Update$Transpiration_rate_07_17_07_18,
  
  Interval_18 = "07/18 - 07/19", # Add a label for the second time interval
  Weight_Diff_18 = Lysimetry_Super_Update$Weight_diff_07_18_07_19,
  Time_Diff_18 = as.numeric(Lysimetry_Super_Update$Time_diff_07_18_07_19, units = "hours"),
  Transpiration_Rate_18 = Lysimetry_Super_Update$Transpiration_rate_07_18_07_19,
  
  Interval_19 = "07/18 - 07/20", # Add a label for the second time interval
  Weight_Diff_19 = Lysimetry_Super_Update$Weight_diff_07_18_07_20,
  Time_Diff_19 = as.numeric(Lysimetry_Super_Update$Time_diff_07_18_07_20, units = "hours"),
  Transpiration_Rate_19 = Lysimetry_Super_Update$Transpiration_rate_07_18_07_20,
  
  Interval_20 = "07/20 - 07/24", # Add a label for the second time interval
  Weight_Diff_20 = Lysimetry_Super_Update$Weight_diff_07_20_07_24,
  Time_Diff_20 = as.numeric(Lysimetry_Super_Update$Time_diff_07_20_07_24, units = "hours"),
  Transpiration_Rate_20 = Lysimetry_Super_Update$Transpiration_rate_07_20_07_24,
  
  Interval_21 = "07/24 - 07/25", # Add a label for the second time interval
  Weight_Diff_21 = Lysimetry_Super_Update$Weight_diff_07_24_07_25,
  Time_Diff_21 = as.numeric(Lysimetry_Super_Update$Time_diff_07_24_07_25, units = "hours"),
  Transpiration_Rate_21 = Lysimetry_Super_Update$Transpiration_rate_07_24_07_25,
  
  Interval_22 = "07/25 - 07/26", # Add a label for the second time interval
  Weight_Diff_22 = Lysimetry_Super_Update$Weight_diff_07_25_07_26,
  Time_Diff_22 = as.numeric(Lysimetry_Super_Update$Time_diff_07_25_07_26, units = "hours"),
  Transpiration_Rate_22 = Lysimetry_Super_Update$Transpiration_rate_07_25_07_26,
  
  Interval_23 = "07/24 - 07/27", # Add a label for the second time interval
  Weight_Diff_23 = Lysimetry_Super_Update$Weight_diff_07_24_07_27,
  Time_Diff_23 = as.numeric(Lysimetry_Super_Update$Time_diff_07_24_07_27, units = "hours"),
  Transpiration_Rate_23 = Lysimetry_Super_Update$Transpiration_rate_07_24_07_27,
  
  Interval_24 = "07/27 - 07/31", # Add a label for the second time interval
  Weight_Diff_24 = Lysimetry_Super_Update$Weight_diff_07_27_07_31,
  Time_Diff_24 = as.numeric(Lysimetry_Super_Update$Time_diff_07_27_07_31, units = "hours"),
  Transpiration_Rate_24 = Lysimetry_Super_Update$Transpiration_rate_07_27_07_31,
  
  Interval_25 = "07/31 - 08/01", # Add a label for the second time interval
  Weight_Diff_25 = Lysimetry_Super_Update$Weight_diff_07_31_08_01,
  Time_Diff_25 = as.numeric(Lysimetry_Super_Update$Time_diff_07_31_08_01, units = "hours"),
  Transpiration_Rate_25 = Lysimetry_Super_Update$Transpiration_rate_07_31_08_01,
  
  Interval_26 = "08/01 - 08/02", # Add a label for the second time interval
  Weight_Diff_26 = Lysimetry_Super_Update$Weight_diff_08_01_08_02,
  Time_Diff_26 = as.numeric(Lysimetry_Super_Update$Time_diff_08_01_08_02, units = "hours"),
  Transpiration_Rate_26 = Lysimetry_Super_Update$Transpiration_rate_08_01_08_02,
  
  Interval_27 = "08/01 - 08/03", # Add a label for the second time interval
  Weight_Diff_27 = Lysimetry_Super_Update$Weight_diff_08_01_08_03,
  Time_Diff_27 = as.numeric(Lysimetry_Super_Update$Time_diff_08_01_08_03, units = "hours"),
  Transpiration_Rate_27 = Lysimetry_Super_Update$Transpiration_rate_08_01_08_03,
  
  Interval_28 = "08/03 - 08/07", # Add a label for the second time interval
  Weight_Diff_28 = Lysimetry_Super_Update$Weight_diff_08_03_08_07,
  Time_Diff_28 = as.numeric(Lysimetry_Super_Update$Time_diff_08_03_08_07, units = "hours"),
  Transpiration_Rate_28 = Lysimetry_Super_Update$Transpiration_rate_08_03_08_07,
  
  Interval_29 = "08/07 - 08/08", # Add a label for the second time interval
  Weight_Diff_29 = Lysimetry_Super_Update$Weight_diff_08_07_08_08,
  Time_Diff_29 = as.numeric(Lysimetry_Super_Update$Time_diff_08_07_08_08, units = "hours"),
  Transpiration_Rate_29 = Lysimetry_Super_Update$Transpiration_rate_08_07_08_08,
  
  Interval_30 = "08/08 - 08/09", # Add a label for the second time interval
  Weight_Diff_30 = Lysimetry_Super_Update$Weight_diff_08_08_08_09,
  Time_Diff_30 = as.numeric(Lysimetry_Super_Update$Time_diff_08_08_08_09, units = "hours"),
  Transpiration_Rate_30 = Lysimetry_Super_Update$Transpiration_rate_08_08_08_09,
  
  Interval_31 = "08/07 - 08/10", # Add a label for the second time interval
  Weight_Diff_31 = Lysimetry_Super_Update$Weight_diff_08_07_08_10,
  Time_Diff_31 = as.numeric(Lysimetry_Super_Update$Time_diff_08_07_08_10, units = "hours"),
  Transpiration_Rate_31 = Lysimetry_Super_Update$Transpiration_rate_08_07_08_10,
  
  Interval_32 = "08/10 - 08/14", # Add a label for the second time interval
  Weight_Diff_32 = Lysimetry_Super_Update$Weight_diff_08_10_08_14,
  Time_Diff_32 = as.numeric(Lysimetry_Super_Update$Time_diff_08_10_08_14, units = "hours"),
  Transpiration_Rate_32 = Lysimetry_Super_Update$Transpiration_rate_08_10_08_14,
  
  Interval_33 = "08/14 - 08/15", # Add a label for the second time interval
  Weight_Diff_33 = Lysimetry_Super_Update$Weight_diff_08_14_08_15,
  Time_Diff_33 = as.numeric(Lysimetry_Super_Update$Time_diff_08_14_08_15, units = "hours"),
  Transpiration_Rate_33 = Lysimetry_Super_Update$Transpiration_rate_08_14_08_15,
  
  Interval_34 = "08/15 - 08/16", # Add a label for the second time interval
  Weight_Diff_34 = Lysimetry_Super_Update$Weight_diff_08_15_08_16,
  Time_Diff_34 = as.numeric(Lysimetry_Super_Update$Time_diff_08_15_08_16, units = "hours"),
  Transpiration_Rate_34 = Lysimetry_Super_Update$Transpiration_rate_08_15_08_16
)



#Create a new dataset
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)

days_measured <- read_excel("Lysimetry+Info.xlsx", sheet = "Days Measured")

#Creating a new dataset
Transpiration_rate_dataset <- days_measured %>%
  expand_grid(ID = treatments$ID) %>%  # Create all combinations of Dates and Plant IDs
  left_join(treatments, by = "ID")      # Join the treatments info based on the 'ID' column

# Convert the 'Dates' column to correct Date format
Transpiration_rate_dataset <- Transpiration_rate_dataset %>%
  mutate(Date = as.Date(Dates, format = "%m/%d/%y"))

# Join the calculated transpiration rate columns from Lysimetry_calculated to Transpiration_rate_dataset
Transpiration_rate_dataset <- Transpiration_rate_dataset %>%
  left_join(Lysimetry_calculated %>% select(ID, starts_with("Transpiration_Rate")), by = "ID")

#Reshape the 'days_measured' dataset into a key-value pair where each 'Transpiration_Rate_#' is matched with the corresponding 'Date'
date_mapping <- days_measured %>%
  rename(Date = Dates, Transpiration_Rate_Column = `Transpiration Rate Column`)

#Adding the new Date and Transpiration Rate info from the 'date_mapping' dataset
Transpiration_rate_dataset <- Transpiration_rate_dataset %>%
  mutate(
    Date = rep(date_mapping$Date, each = nrow(Transpiration_rate_dataset) / length(date_mapping$Date)),
    Transpiration_Rate_Column = rep(date_mapping$Transpiration_Rate_Column, each = nrow(Transpiration_rate_dataset) / length(date_mapping$Transpiration_Rate_Column))
  )

library(dplyr)
library(tidyr)

# Rename or remove the existing 'Transpiration_Rate_Column' to avoid conflict
Transpiration_rate_dataset <- Transpiration_rate_dataset %>%
  select(-`Transpiration_Rate_Column`) # Remove the conflicting column

# Reshape the dataset
Transpiration_rate_long <- Transpiration_rate_dataset %>%
  pivot_longer(
    cols = starts_with("Transpiration_Rate_"), # Matches only transpiration rate columns
    names_to = "Transpiration_Rate_Column",    # Column to hold original column names
    values_to = "Transpiration_Rate_Value"     # Column to hold the values
  )

#Delete the date column
Transpiration_rate_long <- Transpiration_rate_long %>%
  select(-`Date`) # Remove the conflicting column




#New graph using 'Transpiration+Rates' excel spreadsheet
library(ggplot2)
library(dplyr)
library(tidyverse)
library(zoo)

transpiration_data <- read_xlsx("Transpiration+Rates.xlsx")

#Filter out harvested plants
filtered_data <- transpiration_data %>%
  filter(Drought_Status != "harvested")

#Panel A: Control plants before drought
panel_a_data <- filtered_data %>%
  filter(Drought_Status == "before_drought", Treatment == "control") %>%
  group_by(Date, Species) %>%
  summarise(Average_Transpiration = mean(Transpiration_rate_value, na.rm = TRUE), .groups = "drop")

# Fill missing dates and interpolate missing values for Panel A
panel_a_data <- filtered_data %>%
  filter(Drought_Status == "before_drought", Treatment == "control") %>%
  group_by(Date, Species) %>%
  summarise(
    Average_Transpiration = mean(Transpiration_rate_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Species) %>%
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%  # Fill missing dates
  mutate(Average_Transpiration = na.approx(Average_Transpiration, na.rm = FALSE)) %>%  # Interpolate missing values
  ungroup()

# Calculate and remove outliers for Panel A data
panel_a_data <- filtered_data %>%
  filter(Drought_Status == "before_drought", Treatment == "control") %>%
  group_by(Date, Species) %>%
  summarise(
    Average_Transpiration = mean(Transpiration_rate_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Species) %>%
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%  # Fill missing dates
  mutate(Average_Transpiration = na.approx(Average_Transpiration, na.rm = FALSE)) %>%  # Interpolate missing values
  ungroup() %>%
  # Detect and remove outliers
  group_by(Species) %>%
  mutate(
    Q1 = quantile(Average_Transpiration, 0.25, na.rm = TRUE),
    Q3 = quantile(Average_Transpiration, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  filter(
    Average_Transpiration >= Lower_Bound & Average_Transpiration <= Upper_Bound | is.na(Average_Transpiration)
  ) %>%
  select(-Q1, -Q3, -IQR, -Lower_Bound, -Upper_Bound) 

#Set the order of the species
panel_a_data <- panel_a_data %>%
  mutate(Species = factor(Species, levels = c("NM", "RP", "SP", "TC", "R+S", "S+T")))
levels(panel_a_data$Species)
unique(panel_a_data$Species)

# Define the color mapping for each species
species_colors <- c(
  "NM" = "tomato",
  "SP" = "green3",
  "RP" = "goldenrod",
  "TC" = "darkturquoise", 
  "S+T" = "magenta",
  "R+S" = "dodgerblue"
)

# Create the graph with manual color assignment
panel_a_graph <- ggplot(panel_a_data, aes(x = Date, y = Average_Transpiration, color = Species)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = species_colors, breaks = c("NM", "RP", "SP", "TC", "R+S", "S+T")) +
  labs(
    title = "Average Transpiration Rate Before Drought",
    x = "Date",
    y = "Average Transpiration Rate (g)",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "bottom"
  )
print(panel_a_graph)

####Saved up to here ####








# Panel B: Drought plants during drought
panel_b_data <- filtered_data %>%
  filter(Drought_Status == "during_drought", Treatment == "drought") %>%
  group_by(Date, Species) %>%
  summarise(Average_Transpiration = mean(Transpiration_rate_value, na.rm = TRUE), .groups = "drop")

# Panel B: During Drought (Drought plants)
plot_b <- ggplot(panel_b_data, aes(x = Date, y = Average_Transpiration, color = Species)) +
  geom_line(size = 1) +
  labs(
    title = "Average Transpiration Rates During Drought",
    x = "Date",
    y = "Average Transpiration Rate",
    color = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  )
print(plot_b)