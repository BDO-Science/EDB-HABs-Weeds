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
library(here)


# Load data files
nuts_all <- read.csv(here::here("analysis_2022/data_clean/AllSources_DiscreteNuts_WideForm_2014_2022.csv"))

load(here::here("data/Regions.RData"))

#Keep only Apr - November
nuts_stats <- nuts_all %>%
  filter(Month %in% c(4, 5, 6,7,8,9,10,11))

#Add regions
nuts_stats_sf = st_as_sf(nuts_stats, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(reg3) %>%
  st_drop_geometry() %>%
  # filter(!is.na(Region)) %>%
  mutate(Station = case_when(Source == "USGS_CAWSC" ~str_sub(Station, start = 6),
                             TRUE ~ Station),
         Season = case_when(Month %in% c(9,10,11)~ "Fall",
                            Month %in% c(12,1,2)~ "Winter",
                            Month %in% c(3,4,5)~ "Spring",
                            Month %in% c(6,7,8)~ "Summer"),
         Season = factor(Season, levels = c("Spring", "Summer", "Fall")),
         fMonth = factor(Month))

#First let's do nitrate. Heres's where the reportling limit thing could mess us sup.
nit = lmer(log(Nitrate+0.04) ~ as.factor(Year) + (1|fMonth) + (1|Stratum2),  data = nuts_stats_sf)
summary(nit)
plot(nit)
resid.nit = resid(nit, type = "pearson")
hist(resid.nit)
boxplot(as.factor(nuts_stats_sf$Year), resid.nit)


nitres = simulateResiduals(nit)
plot(nitres)
#OK, some issues, but not terrible


#Now the ammonium
Amm = lmer(log(Ammonium+0.05) ~ as.factor(Year) + (1|fMonth)+ (1|Stratum2),  data = nuts_stats_sf)
summary(Amm)
plot(simulateResiduals(Amm))
plot(Amm)

#Chlorophyll
chl= lmer(log(Chl+0.01) ~ as.factor(Year) + (1|fMonth)+ (1|Stratum2),  data = nuts_stats_sf)
summary(chl)
plot(simulateResiduals(chl))
plot(chl)

#orthophosphate
Orth= lmer(log(Phosphorus+0.05) ~ as.factor(Year) + (1|fMonth)+ (1|Stratum2),  data = nuts_stats_sf)
summary(Orth)
plot(Orth)
plot(simulateResiduals(Orth))


#plot for report##################
#This is figure 2-26

Amg = plot(emmeans(Amm, specs = "Year"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Ammonia")

Amm.emm <- emmeans(Amm, "Year", data=nuts_stats_sf)
Ammresults <- as.data.frame(pairs(Amm.emm, adjust="tukey")) %>%
  mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
           arrange(sig)

Nitg = plot(emmeans(nit, specs = "Year"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Nitrate + Nitrite")

Nit.emm <- emmeans(nit, "Year", data=nuts_stats_sf)
(Nitresults <- as.data.frame(pairs(Nit.emm, adjust="tukey"))%>%
  mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
  arrange(sig))

CHLg = plot(emmeans(chl, specs = "Year"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Chlorophyll")

Chl.emm <- emmeans(chl, "Year", data=nuts_stats_sf)
(Chlresults <- as.data.frame(pairs(Chl.emm, adjust="tukey"))%>%
  mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
  arrange(sig))

Orthg = plot(emmeans(Orth, specs = "Year"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Orthophosphate")

Orth.emm <- emmeans(Orth, "Year", data=nuts_stats_sf)
(Orthresults <- as.data.frame(pairs(Orth.emm, adjust="tukey"))%>%
  mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
  arrange(sig))

cowplot::plot_grid(Amg, CHLg, Nitg, Orthg, nrow = 2)

ggsave(filename = "Nutsemmeans.tiff", device = "tiff", path = "plots/",
       width = 8, height = 10)


write_csv(Nitresults, here("analysis_2022", "data_clean", "Nitresults_emmeans.csv"))
write_csv(Chlresults, here("analysis_2022", "data_clean", "Chlresults_emmeans.csv"))
write_csv(Orthresults, here("analysis_2022", "data_clean", "Orthresults_emmeans.csv"))
write_csv(Ammresults, here("analysis_2022", "data_clean", "Ammresults_emmeans.csv"))

##########################################################################################
