# Compare nutrient concentrations across years for 2022 TUCO report
# Original code by Rosemary Hartman (at end of NutChl.R from 2021 repository)
# Updated by Kristi Arend, 3/6/2023


##################################3
#nutrient statistics
library(lme4)
library(lmerTest)
library(emmeans)
library(DHARMa)
library(plyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)
library(cowplot)
library(sf)
library(ggmap)
library(deltamapr)
library(ggspatial)


# Load data files
nuts_all <- read.csv("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/data_clean/AllSources_DiscreteNuts_WideForm_2014_2022.csv")

load("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/data/Regions.RData")

#Keep only Apr - November
nuts_stats <- nuts_all %>%
  filter(Month %in% c(4, 5, 6,7,8,9,10,11))

#Add regions
nuts_stats_sf = st_as_sf(nuts_stats, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(reg3) %>%
  st_drop_geometry() %>%
  # filter(!is.na(Region)) %>%
  mutate(Station = case_when(Source == "USGS_CAWSC" ~str_sub(Station, start = 6),
                             TRUE ~ Station))

#First let's do nitrate. Heres's where the reportling limit thing could mess us sup.
nit = lmer(log(Nitrate+0.04) ~ as.factor(Year) + (1|Month) + (1|Stratum2),  data = nuts_stats_sf)
summary(nit)
plot(nit)
nitres = simulateResiduals(nit)
plot(nitres)
#OK, some issues, but not terrible


#Now the ammonium
Amm = lmer(log(Ammonium+0.05) ~ as.factor(Year) + (1|Month)+ (1|Stratum2),  data = nuts_stats_sf)
summary(Amm)
plot(simulateResiduals(Amm))


#Chlorophyll
chl= lmer(log(Chl+0.01) ~ as.factor(Year) + (1|Month)+ (1|Stratum2),  data = nuts_stats_sf)
summary(chl)
plot(simulateResiduals(chl))

#orthophosphate
Orth= lmer(log(Phosphorus+0.05) ~ as.factor(Year) + (1|Month)+ (1|Stratum2),  data = nuts_stats_sf)
summary(Orth)
plot(Orth)
plot(simulateResiduals(Orth))


#plot for report##################
#This is figure 2-26

Amg = plot(emmeans(Amm, specs = "Year", by = "Season"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Ammonia")

Nitg = plot(emmeans(nit, specs = "Year", by = "Season"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Nitrate + Nitrite")

CHLg = plot(emmeans(chl, specs = "Year", by = "Season"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Chlorophyll")

Orthg = plot(emmeans(Orth, specs = "Year", by = "Season"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Orthophosphate")

cowplot::plot_grid(Amg, CHLg, Nitg, Orthg, nrow = 2)

ggsave(filename = "Nutsemmeans.tiff", device = "tiff", path = "plots/",
       width = 8, height = 10)

##########################################################################################
