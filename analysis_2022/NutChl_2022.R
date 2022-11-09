#Graph discrete chlorophyll and nutrients
#Origional code by Mine Berg mbearg@esassoc.com
#Updated by Rosemary Hartman (rosemary.hartman@water.ca.gov)
#Modified by Kristi Arend (karend@usbr.gov) for 2022 data


library(plyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(sf)
library(ggmap)
library(dplyr)

# read in data file; this is for 2022 data only
# load regions file

hab_nutr_chla_mvi <- read_csv("analysis_2022/data_clean/discrete_wq_2022.csv")
load("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/data/Regions.RData")

###################################################################
#Now some basic graphs of nutrients
#replace values below the reporting limits with zeros
#Note: There are better ways to do this, I was tired and I don't think it really matters that much
#maybe this is somethign Amanda can help with

#First start with just the data from 2022
Nuts = mutate(hab_nutr_chla_mvi, Month = month(Date),
              Year = year(Date),Chlorophyll = as.numeric(Chlorophyll),
              Nitrate = as.numeric(DissNitrateNitrite),
              Phosphorus = as.numeric(DissOrthophos)) %>%
  dplyr::mutate(Chl = case_when(Chlorophyll_Sign == "<" ~ 0,
                         TRUE ~ Chlorophyll),
         Nitrate = case_when(DissNitrateNitrite_Sign == "<" ~ 0,
                             TRUE ~ Nitrate),
        Nitrate = case_when(DissNitrateNitrite_Sign == "< (estimated)" ~ 0,
                            TRUE ~ Nitrate),
         Phosphorus = case_when(DissOrthophos_Sign == "<" ~ 0,
                                TRUE ~ Phosphorus)) %>%
  dplyr::filter(Month %in% c(2, 3, 4, 5, 6,7,8,9))


ggplot(Nuts, aes(x= Month, y = Nitrate, color = Station ))+geom_point() + geom_line() +
  xlab("month - 2022")+ ylab("Nitrate")

# a couple of the NCRO stations have different cdec codes than what NCRO uses and don't have lat and long
# also, time stamps have 8 hours added to the original collection times for some reason
## coordinates; insert correct WGS84 coordinates as follows:
### Station MHR is really MHO: 37.87618, -121.383
### Station OBD is really ODM: 37.81097, -121.544
### Station RSL is really RSCC: 37.97619, -121.64

Nuts$Station[Nuts$Station == "MHR"] <- "MHO"
Nuts$Station[Nuts$Station == "OBD"] <- "ODM"
Nuts$Station[Nuts$Station == "RSL"] <- "RSCC"

Nuts = dplyr::mutate(Nuts, Latitude = case_when(Station == "MHO" ~ 37.87618,
                          TRUE ~ Latitude),
  Longitude = case_when(Station == "MHO" ~ -121.383,
                                           TRUE ~ Longitude),
  Latitude = case_when(Station == "ODM" ~ 37.81097,
                                           TRUE ~ Latitude),
  Longitude = case_when(Station == "ODM" ~ -121.544,
                                            TRUE ~ Longitude),
  Latitude = case_when(Station == "RSCC" ~ 37.97619,
                                           TRUE ~ Latitude),
  Longitude = case_when(Station == "RSCC" ~ -121.640,
                                            TRUE ~ Longitude))

#add the regional assignments
nutssf = st_as_sf(Nuts, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(reg3) %>%
  st_drop_geometry() %>%
  # filter(!is.na(Region)) %>%
  mutate(Station = case_when(Source == "USGS_CAWSC" ~str_sub(Station, start = 6),
                             TRUE ~ Station),
         Ammonium = as.numeric(DissAmmonia)) %>%
  mutate(Ammonium = case_when(DissAmmonia_Sign == "<" ~ 0,
                              TRUE ~ Ammonium),
    Ammonium = case_when(DissAmmonia_Sign == "< (estimated)" ~ 0,
                            TRUE ~ Ammonium))

#make a color pallet
library(RColorBrewer)
pal = c(brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), brewer.pal(9, "Set1"), brewer.pal(12, "Paired"), "black", "grey")

#Plot all the nitrate data for 2022. This is figure 2-9
# find min and max values for y axis scaling
nit_min <- min(nutssf$Nitrate, na.rm=TRUE)
nit_max <- max(nutssf$Nitrate, na.rm=TRUE)

nutssf %>%
  droplevels() %>%
  filter(!is.na(Nitrate)) %>%
  split(.$Stratum2) %>%
  map(~ ggplot(., aes(x= Date, y = Nitrate, color = Station ))+geom_point() + geom_line() +
        xlab("Date")+ ylab("Nitrate + Nitrite (mg/L)") +
        scale_color_manual(values = pal)+
        ylim(nit_min, nit_max) +
        annotate("rect", xmin = min(nutssf$Date),
                 xmax =  max(nutssf$Date),
                 ymin = 0, ymax = 0.04, alpha = 0.5, fill = "gray")+
        ggtitle(.$Stratum2)+
        theme_bw()+
        theme(legend.margin = margin(0, 0,0,0),
              legend.text = element_text(size = 7),
              legend.title = element_blank(), legend.key.size = unit(1, "line"),
              legend.background = element_rect(fill = "transparent"))+
        guides(color = guide_legend(override.aes = list(size = 1), nrow = 7))) %>%
  cowplot::plot_grid(plotlist = ., nrow = 4)

ggsave(filename = "analysis_2022/figures/Nitrate_sameY.tiff", device = "tiff", width = 10, height = 12)


#Plot the ammonium data. This is figure 2-8
# find min and max values for y axis scaling
amm_min <- min(nutssf$Ammonium, na.rm=TRUE)
#amm_max <- max(nutssf$Ammonium, na.rm=TRUE)
## max value of 0.28 not showing up on plots, because isn't in one of the strata; set max at 0.15

nutssf %>%
  droplevels() %>%
  filter(!is.na(Ammonium)) %>%
  split(.$Stratum2) %>%
  map(~ ggplot(., aes(x= Date, y =Ammonium, color = Station ))+
        geom_point() + geom_line() +
        xlab("Date")+ ylab("Ammonium (mg/L)") +
        scale_color_manual(values = pal)+
        ylim(amm_min, 0.15) +
        annotate("rect", xmin = min(nutssf$Date),
                 xmax =  max(nutssf$Date),
                 ymin = 0, ymax = 0.05, alpha = 0.5, fill = "gray")+

        ggtitle(.$Stratum2)+
        theme_bw()+
        theme(legend.margin = margin(0, 0,0,0),
              legend.text = element_text(size = 7),
              legend.title = element_blank(), legend.key.size = unit(1, "line"),
              legend.background = element_rect(fill = "transparent"))+
        guides(color = guide_legend(override.aes = list(size = 1), nrow = 7))) %>%
  cowplot::plot_grid(plotlist = ., nrow = 4)

ggsave(filename = "analysis_2022/figures/Ammonium_sameY.tiff", device = "tiff", width = 10, height = 12)


#Plot Chlorophyll. FIgure 2-7
# find min and max values for y axis scaling
chla_min <- min(nutssf$Chl, na.rm=TRUE)
chla_max <- max(nutssf$Chl, na.rm=TRUE)

nutssf %>%
  droplevels() %>%
  filter(!is.na(Chlorophyll)) %>%
  split(.$Stratum2) %>%
  map(~ ggplot(., aes(x= Date, y = Chl, color = Station ))+geom_point() + geom_line() +
        xlab("Date")+ ylab("Chlorophyll (ug/L)") +
        scale_color_manual(values = pal)+
#        ylim(chla_min, chla_max) +
        ggtitle(.$Stratum2)+
        annotate("rect", xmin = min(nutssf$Date),
                 xmax =  max(nutssf$Date),
                 ymin = 0, ymax = 0.5, alpha = 0.5, fill = "gray")+
        theme_bw()+
        theme(legend.margin = margin(0, 0,0,0),
              legend.text = element_text(size = 7),
              legend.title = element_blank(), legend.key.size = unit(1, "line"),
              legend.background = element_rect(fill = "transparent"))+

        guides(color = guide_legend(override.aes = list(size = 1), nrow = 7))) %>%
  cowplot::plot_grid(plotlist = ., nrow = 4)

ggsave(filename = "analysis_2022/figures/Chlorophyll.tiff", device = "tiff", width = 10, height = 12)


#Plot ortho-phosphate, figure 2-10

# find min and max values for y axis scaling
Phos_min <- min(nutssf$Phosphorus, na.rm=TRUE)
Phos_max <- max(nutssf$Phosphorus, na.rm=TRUE)

nutssf %>%
  droplevels() %>%
  filter(!is.na(Phosphorus)) %>%
  split(.$Stratum2) %>%
  map(~ ggplot(., aes(x= Date, y = Phosphorus, color = Station ))+geom_point() + geom_line() +
        xlab("Date")+ ylab("Dissolved Orthophosphate (mg/L)") +
        scale_color_manual(values = pal)+
        ylim(Phos_min,Phos_max)+
        ggtitle(.$Stratum2)+
        theme_bw()+
        annotate("rect", xmin = min(nutssf$Date),
                 xmax =  max(nutssf$Date),
                 ymin = 0, ymax = 0.05, alpha = 0.5, fill = "gray")+
        theme(legend.margin = margin(0, 0,0,0),
              legend.text = element_text(size = 7),
              legend.title = element_blank(), legend.key.size = unit(1, "line"),
              legend.background = element_rect(fill = "transparent"))+
        guides(color = guide_legend(override.aes = list(size = 1), nrow = 7))) %>%
  cowplot::plot_grid(plotlist = ., nrow = 4)

ggsave(filename = "analysis_2022/figures/Orthophos_sameY.tiff", device = "tiff", width = 10, height = 12)


#P8 had some really high values. Is that normal?
P8 = filter(mutate(hab_nutr_chla_mvi, Month = month(Date),
                   Year = year(Date),Chlorophyll = as.numeric(Chlorophyll),
                   Nitrate = as.numeric(DissNitrateNitrite)), Station %in% c("P8", "D19", "OSJ", "C9"))
ggplot(P8, aes(x = Date, y = Nitrate, color = Station)) + geom_line()
ggplot(filter(P8, !is.na(DissAmmonia)), aes(x = Date, y = as.numeric(DissAmmonia), color = Station)) + geom_line()
#OK, yes, it is normal

###########################################################
#South Delta Nutrients by season and year
#from 2014-2022

#2014-2021 data from last year's report
Nuts_pre22 <- read_csv("data/hab_nutr_chla_mvi.csv")

# remove microcystis and region columns
Nuts_pre22_v2 <- subset(Nuts_pre22, select = -c(Region, Microcystis))

# manipulate data like did for 2022
Nuts_pre22_v3 = mutate(Nuts_pre22_v2, Month = month(Date),
              Year = year(Date),Chlorophyll = as.numeric(Chlorophyll),
              Nitrate = as.numeric(DissNitrateNitrite),
              Phosphorus = as.numeric(DissOrthophos)) %>%
  dplyr::mutate(Chl = case_when(Chlorophyll_Sign == "<" ~ 0,
                                TRUE ~ Chlorophyll),
                Nitrate = case_when(DissNitrateNitrite_Sign == "<" ~ 0,
                                    TRUE ~ Nitrate),
                Nitrate = case_when(DissNitrateNitrite_Sign == "< (estimated)" ~ 0,
                                    TRUE ~ Nitrate),
                Phosphorus = case_when(DissOrthophos_Sign == "<" ~ 0,
                                       TRUE ~ Phosphorus)) %>%
  dplyr::filter(Month %in% c(2, 3, 4, 5, 6,7,8,9)) %>%
  # rearrange columns to match 2022 dataframe
  dplyr::select(Source, Station, Latitude, Longitude,
                Date, Datetime, DissAmmonia, DissAmmonia_Sign, DissNitrateNitrite,
                DissNitrateNitrite_Sign, DissOrthophos, DissOrthophos_Sign,
                Chlorophyll, Chlorophyll_Sign, Month, Year, Nitrate, Phosphorus, Chl)

#combine 2022 data with 2014-2021 data
Nuts_all <- rbind(Nuts_pre22_v3, Nuts)


###
SoNuts = mutate(Nuts_all, Month = month(Date),
                Year = year(Date),
                Ammonium = as.numeric(DissAmmonia)) %>%
  mutate(Ammonium = case_when(DissAmmonia_Sign == "<" ~ 0,
                              TRUE ~ DissAmmonia),
         Ammonium = case_when(DissAmmonia_Sign == "< (estimated)" ~ 0,
                              TRUE ~ Ammonium))


#Filter it to just the regions we are interested in, and add seasons
Sonutssf = st_as_sf(SoNuts, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(reg3) %>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum2)) %>%
  mutate(Station = case_when(Source == "USGS_CAWSC" ~str_sub(Station, start = 6),
                             TRUE ~ Station))%>%
  filter(Stratum2 %in% c("Lower SJ", "South Delta", "Lower Sac")) %>%
  mutate(Month = month(Date), Year = year(Date), Season = case_when(
    Month %in% c(12,1,2) ~ "Winter",
    Month %in% c(3,4,5) ~ "Spring",
    Month %in% c(6,7,8) ~ "Summer",
    Month %in% c(9,10,11) ~ "Fall"
  ))

#Calculate mean and standard error for each parameter
SoNutsmean = Sonutssf %>%
  pivot_longer(cols = c(Ammonium, Chl, Nitrate, Phosphorus), names_to= "Analyte",
               values_to = "Concentration")  %>%
  group_by(Year, Season, Analyte) %>%
  summarize(ConcentrationM = mean(Concentration, na.rm = T),
            SEc = sd(Concentration, na.rm = T)/sqrt(n())) %>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))

#Plot all the nutrient data across years THis is figure 2-25
ggplot(SoNutsmean, aes(x=Year, y = ConcentrationM, fill = Season)) + geom_col()+
  geom_errorbar(aes(ymin = ConcentrationM - SEc, ymax = ConcentrationM + SEc ))+
  facet_grid(Analyte~Season, scales = "free_y")+
  scale_fill_brewer(palette = "Set2", guide = NULL)+
  ylab("Concentration")+
  theme_bw()

ggsave(filename = "analysis_2022/figures/Nutrients.tiff", device = "tiff", width = 6, height = 5)
##################################3
#nutrient statistics
library(lme4)
library(lmerTest)
library(emmeans)
library(DHARMa)

#First let's do nitrate. Heres's where the reportling limit thing could mess us sup.
nit = lmer(log(Nitrate+0.04) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(nit)
plot(nit)
nitres = simulateResiduals(nit)
plot(nitres)
#OK, some issues, but not terrible


#Now the ammonium
Amm = lmer(log(Ammonium+0.05) ~ as.factor(Year)+Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(Amm)
plot(simulateResiduals(Amm))


#Chlorophyll
chl= lmer(log(Chl+0.01) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(chl)
plot(simulateResiduals(chl))

#orthophosphate
Orth= lmer(log(Phosphorus+0.05) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(Orth)
plot(Orth)
plot(simulateResiduals(Orth))


#pllot for report##################
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

