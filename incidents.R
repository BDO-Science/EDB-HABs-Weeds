
#install.packages("devtools")
#devtools::install_github("InteragencyEcologicalProgram/deltamapr")

library(tidyverse)
library(sf)
library(deltamapr)

#Set workspace
#setwd("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/data_raw/SWB_Incident_2022only.csv")

#load data from the water board's portal
## SWB_Incident_All2022 includes historical data through May 2022
## SWB_Incident_2022only includes all 2022 data (so, more recent than May)

fhabportal <- read_csv("analysis_2022/data_raw/SWB_Incident_All2022.csv")
fhabportal22 <- read_csv("analysis_2022/data_raw/SWB_Incident_2022only.csv")

#some of the lats an dlongs are mistakes

fhabportal = filter(fhabportal, Latitude >30, Latitude <45, Longitude >-130, Longitude < -110)
fahportal22 = filter(fhabportal, Latitude >30, Latitude <45, Longitude >-130, Longitude < -110)

fhabsf = filter(fhabportal, !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ggplot()+ geom_sf(data = WW_Delta)+ geom_sf(data = fhabsf)

delta = st_read("analysis_2022/LegalDelta/i03_LegalDeltaBoundary.shp")

# put in same projection;
delta4326 <- st_transform(delta, crs = st_crs(fhabsf))

#crop to observations in the Delta
fhabsf2 = st_crop(fhabsf, delta4326)

#summary(fhabportal)
#write.csv(fhabportal, "IncidentReports.csv")


ggplot()+ geom_sf(data = WW_Delta)+ geom_sf(data = fhabsf2, aes(color = TypeofSign))

fhabsf2$ObservationDate = mdy(fhabsf2$ObservationDate)

fhabsf2 = mutate(fhabsf2, Advisory = case_when(
  str_detect(TypeofSign, "caution") ~ "Caution",
  str_detect(TypeofSign, "none") ~ "No Advisory",
  str_detect(TypeofSign, "warning") ~ "Warning",
  str_detect(TypeofSign, "danger") ~ "Danger"
)) %>%
  filter(!is.na(Advisory)) %>%
  mutate(Year = year(ObservationDate))


ggplot()+ geom_sf(data = WW_Delta)+ geom_sf(data = fhabsf2, aes(color = Advisory))+
  facet_wrap(~Year) + geom_sf(data = delta4326)

ggplot()+ geom_sf(data = WW_Delta)+
  geom_sf(data = reg3, aes(fill = Stratum2), alpha = 0.4) +
  scale_fill_manual(values = reg3$colors, guide = NULL)+
  geom_sf_label(data = reg3, aes(label = Stratum2),
                label.size = 0.05,
                label.padding = unit(0.1, "lines"),
                nudge_y = reg3$nudge, alpha = 0.8, fontface = "bold")+

  #geom_sf(data = cdecsf,shape = 16, size = 4, aes(color = "Temperature stations")) +
   geom_sf(data = filter(fhabsf2, Year == 2021, Advisory != "No Advisory"), aes(color = Advisory))+
 scale_color_manual(values = c("yellow", "red"))
  coord_sf(xlim = c(-121.9, -121.2), ylim = c(37.6, 38.6))+
  scalebar(dist = 10, dist_unit = "km",
           transform = TRUE, st.dist = .05, x.min = -121.6, x.max = -121.8, y.min = 37.6, y.max = 37.8) +

  #there are a number of different optinos for north arrow symbols. ?north
  north(data = reg3, symbol = 2) +
  theme_bw()+ylab("")+xlab("")
