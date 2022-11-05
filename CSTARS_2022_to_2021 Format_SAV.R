#2022 CSTARS data reformatting to fit the CSTARS_2021_GroundTruthing_SAV.R structure
#franks Tract data

#2022 author Nick Bertrand
#Nbertrand@usbr.gov

library(tidyverse)
library(splitstackshape)
library(sf)

cstars22.start <- read_csv("https://raw.githubusercontent.com/BDO-Science/EDB-HABs-Weeds/main/analysis_2022/data_raw/Veg/FranksTract_2022_formatted.csv")

view(cstars22.start)



#gather function consolidates the species into a count column and drops out any NAs from the data.
cstars22.gath <- cstars22.start %>% gather("species","rake_prop", Richardson:Unknown) %>%
  drop_na(rake_prop)
#view(cstars22.gath)

#gather function consolidates the species into a count column and drops out any NAs from the data.
cstars22.gath <- cstars22.start %>% gather("species","rake_prop", Richardson:Unknown) %>%
  drop_na(rake_prop)
#view(cstars22.gath)


library(sf)

wgs84conversion <- cstars22.gath %>%
  st_as_sf(coords = c("easting_26910", "northing_26910"), crs = 26910) %>%
  st_transform(4326) %>%
  st_coordinates()

wgs84conversion <-as.data.frame(wgs84conversion)

#view(wgs84conversion)

wgs84conversion<- rename(wgs84conversion, latitude_wgs84 = Y , longitude_wgs84 = X)

cstars22.con <-cbind(cstars22.gath, wgs84conversion) %>%
  select(latitude_wgs84, longitude_wgs84, date, time, feat,rake_teeth, species, rake_prop) %>%
  rename(survey_method = feat, rake_teeth_corr = rake_teeth)

view(cstars22.con)
cstars22.con$time <- as.character(cstars22.con$time)
cstars <- cstars22.con

