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
         fMonth = factor(Month),
         fYear = factor(Year),
         Stratum2 = factor(Stratum2))


# Look at sample size
sampsize <- nuts_stats_sf %>%
  dplyr::group_by(Season, fYear) %>%
  dplyr::summarize(n = n())

ggplot(sampsize) + geom_tile(aes(x = fYear, y = Season, fill = n))

# Nitrate ----------------------------------------------------------
#First let's do nitrate. Heres's where the reporting limit thing could mess us up.
nit = lmer(log(Nitrate+0.04) ~ fYear  + Stratum2 + (1|fMonth) ,  data = nuts_stats_sf)
summary(nit)
plot(nit)

# Residuals
nitres = simulateResiduals(nit)
plot(nitres)
par(mfrow = c(2,4))
resid1 = resid(nit, type = "pearson")
df1 = nuts_stats_sf %>% filter(!is.na(Nitrate), !is.na(fMonth), !is.na(Stratum2), !is.na(fYear))
lev1a = hatvalues(nit)
plot(nit)
qqnorm(resid1)
qqline(resid(nit))
plot(lev1a, y = resid1)
hist(resid1)
plot(resid1)
plot(df1$fYear, resid1)
plot(df1$Stratum2, resid1)
acf(resid1)
#OK, some issues, but not terrible


#Ammonium--------------------------------------------------------------
Amm = lmer(log(Ammonium+0.05) ~ fYear + Stratum2 + (1|fMonth),  data = nuts_stats_sf)
summary(Amm)
plot(simulateResiduals(Amm))
plot(Amm)

# Residuals
par(mfrow = c(2,4))
residamm = resid(Amm, type = "pearson")
df_amm = nuts_stats_sf %>% filter(!is.na(Ammonium), !is.na(fMonth), !is.na(Stratum2), !is.na(fYear))
lev1a = hatvalues(Amm)
plot(Amm)
qqnorm(residamm)
qqline(resid(Amm))
plot(lev1a, y = residamm)
hist(residamm)
plot(residamm)
plot(df_amm$fYear, residamm)
plot(df_amm$Stratum2, residamm)
acf(residamm)
#Issues in the last region


#Chlorophyll -------------------------------------------------------------
chl= lmer(log(Chl+1) ~ fYear + Stratum2 + (1|fMonth),  data = nuts_stats_sf)
summary(chl)
plot(simulateResiduals(chl))
plot(chl)


# Residuals
par(mfrow = c(2,4))
residchl = resid(chl, type = "pearson")
df_chl = nuts_stats_sf %>% filter(!is.na(Chl), !is.na(fMonth), !is.na(Stratum2), !is.na(fYear))
lev1a = hatvalues(chl)
plot(chl)
qqnorm(residchl)
qqline(resid(chl))
plot(lev1a, y = residchl)
hist(residchl)
plot(residchl)
plot(df_chl$fYear, residchl)
plot(df_chl$Stratum2, residchl)
acf(residchl)


#orthophosphate ------------------------------------------------------------
Orth= lmer(log(Phosphorus+0.05) ~ fYear + Stratum2 + (1|fMonth),  data = nuts_stats_sf)
summary(Orth)
plot(Orth)
plot(simulateResiduals(Orth))

# Residuals
par(mfrow = c(2,4))
residorth = resid(Orth, type = "pearson")
df_Orth = nuts_stats_sf %>% filter(!is.na(Phosphorus), !is.na(fMonth), !is.na(Stratum2), !is.na(fYear))
lev1a = hatvalues(Orth)
plot(Orth)
qqnorm(residorth)
qqline(resid(Orth))
plot(lev1a, y = residorth)
hist(residorth)
plot(residorth)
plot(df_Orth$fYear, residorth)
plot(df_Orth$Stratum2, residorth)
acf(residorth)

#plot for report##################
#This is figure 2-26

# emmeans plots
Amg = plot(emmeans(Amm, specs = "Year"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Ammonia")

Nitg = plot(emmeans(nit, specs = "Year"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Nitrate + Nitrite")

CHLg = plot(emmeans(chl, specs = "Year"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Chlorophyll")

Orthg = plot(emmeans(Orth, specs = "Year"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Orthophosphate")

cowplot::plot_grid(Amg, CHLg, Nitg, Orthg, nrow = 2)

# emmeans tables

Amm.emm1 <- emmeans(Amm, "fYear", data=nuts_stats_sf)
Ammresults1 <- as.data.frame(pairs(Amm.emm1, adjust="tukey")) %>%
  mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
  arrange(sig)

Amm.emm2 <- emmeans(Amm, "Stratum2", data=nuts_stats_sf)
Ammresults2 <- as.data.frame(pairs(Amm.emm2, adjust="tukey")) %>%
  mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
  arrange(sig)

Nit.emm1 <- emmeans(nit, "fYear", data=nuts_stats_sf)
(Nitresults1 <- as.data.frame(pairs(Nit.emm1, adjust="tukey"))%>%
    mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
    arrange(sig))

Nit.emm2 <- emmeans(nit, "Stratum2", data=nuts_stats_sf)
(Nitresults2 <- as.data.frame(pairs(Nit.emm2, adjust="tukey"))%>%
    mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
    arrange(sig))

Chl.emm1 <- emmeans(chl, "fYear", data=nuts_stats_sf)
(Chlresults1 <- as.data.frame(pairs(Chl.emm1, adjust="tukey"))%>%
    mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
    arrange(sig))

Chl.emm2 <- emmeans(chl, "Stratum2", data=nuts_stats_sf)
(Chlresults2 <- as.data.frame(pairs(Chl.emm2, adjust="tukey"))%>%
    mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
    arrange(sig))

Orth.emm1 <- emmeans(Orth, "fYear", data=nuts_stats_sf)
(Orthresults1 <- as.data.frame(pairs(Orth.emm1, adjust="tukey"))%>%
  mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
  arrange(sig))

Orth.emm2 <- emmeans(Orth, "Stratum2", data=nuts_stats_sf)
(Orthresults2 <- as.data.frame(pairs(Orth.emm2, adjust="tukey"))%>%
    mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
    arrange(sig))


# ggsave(filename = "Nutsemmeans.tiff", device = "tiff", path = "plots/",
#        width = 8, height = 10)

# Write emmeans tables
write_csv(Nitresults1, here::here("analysis_2022", "data_clean", "model_Nitresults_year_emmeans.csv"))
write_csv(Chlresults1, here::here("analysis_2022", "data_clean", "model_Chlresults_year_emmeans.csv"))
write_csv(Orthresults1, here::here("analysis_2022", "data_clean", "model_Orthresults_year_emmeans.csv"))
write_csv(Ammresults1, here::here("analysis_2022", "data_clean", "model_Ammresults_year_emmeans.csv"))
write_csv(Nitresults2, here::here("analysis_2022", "data_clean", "model_Nitresults_stratum_emmeans.csv"))
write_csv(Chlresults2, here::here("analysis_2022", "data_clean", "model_Chlresults_stratum_emmeans.csv"))
write_csv(Orthresults2, here::here("analysis_2022", "data_clean", "model_Orthresults_stratum_emmeans.csv"))
write_csv(Ammresults2, here::here("analysis_2022", "data_clean", "model_Ammresults_stratum_emmeans.csv"))
##########################################################################################
