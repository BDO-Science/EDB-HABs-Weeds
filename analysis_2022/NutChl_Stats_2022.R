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

data_year <- nuts_stats_sf %>%
  dplyr::filter(Stratum2 %in% c("Lower Sac", "Lower SJ","OMR", "South Delta", "Franks"))
  # dplyr::filter(DissAmmonia_Sign=="=",
  #               DissNitrateNitrite_Sign=="=",
  #               DissOrthophos_Sign=="=")


# Datasets for each analyte - removed all the below reporting limit instances
data_nit <- data_year %>% filter(DissNitrateNitrite_Sign == "=") %>%
  filter(!is.na(DissNitrateNitrite))
data_amm <- data_year %>% filter(DissAmmonia_Sign == "=") %>%
  filter(!is.na(DissAmmonia))
data_chl <- data_year %>% filter(Chlorophyll_Sign == "=") %>%
  filter(!is.na(Chlorophyll))
data_ortho <- data_year %>% filter(DissOrthophos_Sign == "=") %>%
  filter(!is.na(DissOrthophos))

# Plots for each analyte
ggplot(data_nit) + geom_point(aes(x = Date, y = DissNitrateNitrite)) + facet_wrap(~Stratum2)
ggplot(data_amm) + geom_point(aes(x = Date, y = Ammonium)) + facet_wrap(~Stratum2)
ggplot(data_ortho) + geom_point(aes(x = Date, y = DissOrthophos)) + facet_wrap(~Stratum2)
ggplot(data_chl) + geom_point(aes(x = Date, y = Chlorophyll)) + facet_wrap(~Stratum2)

# Look at sample size
sampsize <- data_year %>%
  dplyr::group_by(fMonth, fYear) %>%
  dplyr::summarize(n = n())

ggplot(sampsize) + geom_tile(aes(x = fYear, y = fMonth, fill = n)) +
  geom_text(aes(label = n, y = fMonth, x = fYear), color = "white")

# Nitrate ----------------------------------------------------------
#First let's do nitrate. Heres's where the reporting limit thing could mess us up.
#nit = lmer(log(Nitrate+0.04) ~ fYear  + Stratum2 + (1|fMonth) ,  data = nuts_stats_sf)
nit = lmer(log(Nitrate+1) ~ fYear  + (1|fMonth) ,  data = data_nit)
summary(nit)
plot(nit)


# Residuals
nitres = simulateResiduals(nit)
plot(nitres)
par(mfrow = c(2,4))
resid1 = resid(nit, type = "pearson")
df1 = data_year %>% dplyr::filter(!is.na(Nitrate), !is.na(fMonth), !is.na(fYear))
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
#Amm = lmer(log(Ammonium+1) ~ fYear + Stratum2 +  (1|fMonth),  data = nuts_stats_sf)
Amm = lmer(log(Ammonium+1) ~ fYear +  (1|fMonth),  data = data_amm)
summary(Amm)
plot(simulateResiduals(Amm))
plot(Amm)

# Residuals
par(mfrow = c(2,4))
residamm = resid(Amm, type = "pearson")
df_amm = data_year %>% filter(!is.na(Ammonium), !is.na(fMonth), !is.na(Stratum2), !is.na(fYear))
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
#chl= lmer(log(Chl+1) ~ fYear  + Stratum2 + (1|fMonth),  data = nuts_stats_sf)
chl= lmer(log(Chl+1) ~ fYear  + (1|fMonth),  data = data_chl)
summary(chl)
plot(simulateResiduals(chl))
plot(chl)


# Residuals
par(mfrow = c(2,4))
residchl = resid(chl, type = "pearson")
df_chl = data_chl %>% filter(!is.na(Chl), !is.na(fMonth), !is.na(Stratum2), !is.na(fYear))
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
#Orth= lmer(log(Phosphorus+0.05) ~ fYear + Stratum2 + (1|fMonth),  data = nuts_stats_sf)
Orth= lmer(log(Phosphorus+1) ~ fYear  + (1|fMonth),  data = data_ortho)
summary(Orth)
plot(Orth)
plot(simulateResiduals(Orth))

# Residuals
par(mfrow = c(2,4))
residorth = resid(Orth, type = "pearson")
df_Orth = data_ortho %>% filter(!is.na(Phosphorus), !is.na(fMonth), !is.na(Stratum2), !is.na(fYear))
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


# CIs

library(effects)
library(emmeans)




# emmeans tables
# need to update dataset if we are going to use the updated datasets
effects_amm <- effects::effect(term = "fYear", mod = Amm)
summary(effects_amm)

Amm.emm1 <- emmeans(Amm, "fYear", data=data_year)
Ammresults1 <- as.data.frame(pairs(Amm.emm1, adjust="tukey")) %>%
  mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
  arrange(sig)
plot(Amm.emm1, comparisons = TRUE,type = "response")

# Amm.emm2 <- emmeans(Amm, "Stratum2", data=data_year)
# Ammresults2 <- as.data.frame(pairs(Amm.emm2, adjust="tukey")) %>%
#   mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
#   arrange(sig)

effects_nit <- effects::effect(term = "fYear", mod = nit)
summary(effects_nit)

Nit.emm1 <- emmeans(nit, "fYear", data=data_year)
(Nitresults1 <- as.data.frame(pairs(Nit.emm1, adjust="tukey"))%>%
    mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
    arrange(sig))
plot(Nit.emm1, comparisons = TRUE,type = "response")

# Nit.emm2 <- emmeans(nit, "Stratum2", data=data_year)
# (Nitresults2 <- as.data.frame(pairs(Nit.emm2, adjust="tukey"))%>%
#     mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
#     arrange(sig))
effects_chl <- effects::effect(term = "fYear", mod = chl)
summary(effects_chl)

Chl.emm1 <- emmeans(chl, "fYear", data=data_year)
(Chlresults1 <- as.data.frame(pairs(Chl.emm1, adjust="tukey"))%>%
    mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
    arrange(sig))
plot(Chl.emm1, comparisons = TRUE,type = "response")

# Chl.emm2 <- emmeans(chl, "Stratum2", data=data_year)
# (Chlresults2 <- as.data.frame(pairs(Chl.emm2, adjust="tukey"))%>%
#     mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
#     arrange(sig))
effects_orth <- effects::effect(term = "fYear", mod = Orth)
summary(effects_orth)

Orth.emm1 <- emmeans(Orth, "fYear", data=data_year)
(Orthresults1 <- as.data.frame(pairs(Orth.emm1, adjust="tukey"))%>%
    mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
    arrange(sig))
plot(Orth.emm1, comparisons = TRUE,type = "response")

# Orth.emm2 <- emmeans(Orth, "Stratum2", data=data_year)
# (Orthresults2 <- as.data.frame(pairs(Orth.emm2, adjust="tukey"))%>%
#     mutate(sig = ifelse(p.value<0.05, "sig", "not sig")) %>%
#     arrange(sig))










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
