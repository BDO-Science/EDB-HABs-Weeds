##Final Code used in 2022 submerged aquatic vegetion in HABS report. This is modified from code written by DWR staff as noted below.
#AquaticVegetation_Analysis_SeProSurveys_TimeSeriesPlots (3).r is the original file

#Code written to take in 2022 SePro data and reformat it to bind it into the FranksTractManagement_2014-2021_formatted.csv


#written by Nick Bertrand
#nbertrand@usbr.gov

library(tidyverse)
library(sf)

data_hist <- read_csv("C:/Users/nbertrand/Desktop/Bertrand/GitHub/EDB-HABs-Weeds/analysis_2022/data_clean/FranksTractManagement_2014-2021_formatted.csv")
#View(data_hist)
library(readr)
#I formatted the date in excel before importing.
SePro_FranksTract_Veg_10042022_dataonly <- read_csv("analysis_2022/data_raw/Veg/SePro_FranksTract_Veg_10042022_dataonly.csv")
#View(SePro_FranksTract_Veg_10042022_dataonly)

#reformats data into long format by species columns
sepro_wide <- SePro_FranksTract_Veg_10042022_dataonly
sepro_long <- sepro_wide %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = "Egeria":"Hybrid_CLP",names_to = c("org_species"))

#view(sepro_long)

#extracts northing and easting and converts to WGS84
sepro_coords <- sepro_long %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = "+proj=utm +zone=10") %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  st_coordinates()
#view(sepro_coords)
#changes to a data frame and renames columns
sepro_coords <- as.data.frame(sepro_coords) %>%
  rename(longitude_wgs84 = X, latitude_wgs84 = Y)
#view(sepro_coords)

#binds the converted coordinates back to the dataframe
sepro_long_coords <- cbind(sepro_long,sepro_coords)
#View(sepro_long_coords)

#selects relevant columns and renames some
sepro_long_coords <- sepro_long_coords %>%
  select(ID,
         program,
         Waypoint,
         latitude_wgs84,
         longitude_wgs84,
         sample_date,
         org_species,
         sample_method,
         value) %>%
  rename(station = Waypoint,
         date = sample_date,
         survey_method = sample_method,
         rake_coverage_ordinal = value,
         SeProName = org_species)

#View(sepro_long_coords)

#assigns incidence
sepro_incid <- sepro_long_coords %>% replace_na(list(rake_coverage_ordinal = 0)) %>%
  mutate(species_incidence = ifelse(rake_coverage_ordinal > 1, 1,0), .after = survey_method) %>%
  group_by(ID) %>%
  mutate(total_incedence = sum(species_incidence)) %>%
  mutate(sav_incidence = ifelse(total_incedence == 1, 1,0), .after = date) %>%
  select(-total_incedence)
#View(sepro_incid)

#imports species names and abreviations table to provided matching names to historic data
library(readr)
SpeciesNameswithOrigin <- read_csv("analysis_2022/data_raw/Veg/SpeciesNameswithOrigin.csv")
#View(SpeciesNameswithOrigin)


sepro_join <- left_join(sepro_incid,SpeciesNameswithOrigin, by = "SeProName")


sepro_almost <- sepro_join %>%
  select(
         program,
         station,
         latitude_wgs84,
         longitude_wgs84,
         date,
         sav_incidence,
         Latin_Name,
         survey_method,
         species_incidence,
         rake_coverage_ordinal) %>%
  rename(species = Latin_Name)

sepro_clean <- sepro_almost %>%
  ungroup() %>%
  select(-ID)

view(sepro_clean)
