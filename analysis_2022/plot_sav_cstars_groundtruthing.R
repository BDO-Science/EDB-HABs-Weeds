## Edited down version of groundtruthing code
  # from analysis_2022/data_raw/Veg/CSTARS_2021_GroundTruthing_SAV.R
## 2022 Data file comes from clean_sav_cstars_data.R
    # That was originally edited down from CSTARSReformatting BB SJ FT.R,
    # which turned into BigBreakDataFormatting.R and FranksTract_2022_formatted.csv

# Authors: Nick Rasmussen (DWR) -- Nick Bertrand (USBR) -- Cat Pien (USBR)

library(dplyr)
library(ggplot2)
library(readr)
library(here)
library(lubridate)
library(tidyverse)
library(tibble)
library(sf)
library(janitor)
library(deltamapr)
library(plotrix)

# Read data -----------------------------
org <- read_csv("analysis_2022/data_raw/Veg/SpeciesNameswithOrigin.csv") %>%
  clean_names() %>%
  mutate(species = case_when(grepl("Richarsdon", species) ~ "Richardson",
                             grepl("Cambomba", species) ~ "Cabomba",
                             grepl("Uknown", species) ~ "Unidentified",
                             TRUE~ species))%>%
  mutate(latin_name = case_when(grepl("Algae", species) ~ "Algae",
                             grepl("Unidentified", species) ~ "Unidentified",
                             TRUE~ latin_name))

cstars_2021 <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/AquaticVegetationPWT/main/MasterDataSet_SAV/Data_Formatted/CSTARS_2021_formatted.csv") %>%
  arrange(latitude_wgs84,longitude_wgs84) %>%
  dplyr::select(-depth_to_sav_m, -secchi_bottom, -secchi_depth_m) %>%
  mutate(species = gsub("_", " ", species))

# Format data -----------------------------------------
## Add zeroes to the NAs ---------------------------------
cstars_2021_format <- cstars_2021 %>%
  filter(!is.na(rake_prop))  %>%
  mutate(species = ifelse(is.na(species), "Nocatch", species)) %>%
  mutate(species = case_when(
    #change Stuckenia filiformis to S. pectinata
    str_detect(species,"filiformis")~"Stuckenia pectinata"
    ,TRUE ~ as.character(species)
  )) %>%
  #combine rake proportions for the two Stuckenia spp in the few samples where they are both present
  group_by(program, latitude_wgs84, longitude_wgs84, rake_teeth_corr,date, time, species) %>%
  summarize(rake_prop = sum(rake_prop, na.rm = TRUE), .groups = 'drop')

cstars_2021_zeros <- cstars_2021_format %>%
  pivot_wider(names_from= "species", values_from = "rake_prop", values_fill = 0,
              id_cols = c(latitude_wgs84, longitude_wgs84, rake_teeth_corr, date, time)) %>%
  pivot_longer(cols = c(`Egeria densa`: `Vallisneria australis`), names_to = "species", values_to = "rake_prop")%>%
  filter(species!="Nocatch") %>%
  rename(latin_name = species) %>%
  left_join(org %>% dplyr::select(species, latin_name))

cstars_2022<- read_csv(here("analysis_2022", "data_clean", "SAV_cstars_bb_frk_sj_2022.csv")) %>%
  arrange(latitude_wgs84,longitude_wgs84) %>%
  mutate(program = "CSTARS") %>%
  dplyr::select(program, latitude_wgs84, longitude_wgs84, date, time, survey_method, rake_teeth_corr, species, rake_prop)

cstars_2022_zeros <- cstars_2022 %>%
  pivot_wider(names_from= "species", values_from = "rake_prop", values_fill = 0,
              id_cols = c(latitude_wgs84, longitude_wgs84, rake_teeth_corr,date, time)) %>%
  pivot_longer(cols = c(Egeria: Cabomba), names_to = "species", values_to = "rake_prop") %>%
  mutate(species = case_when(grepl("Curlyleaf", species) ~ "CurlyLeaf",
                             grepl("Unknown", species) ~ "Unidentified",
                                   TRUE~ species))%>%
  left_join(org %>% dplyr::select(species, latin_name))

## Combine 2021 and 2022 ---------------------------------
cstars <- rbind(cstars_2022_zeros, cstars_2021_zeros) %>%
  mutate(year = year(date))


## Read and convert to sf --------------------------------
sf_franks <- read_sf(here("Spatial_data_weeds", "FranksTractarea_wgs84.shp"))
sf_bbreak <- read_sf(here("Spatial_data_weeds", "BigBreak_wgs84.shp"))
sf_ccourt <- read_sf(here("Spatial_data_weeds", "Cliftoncourtarea_wgs84.shp"))
WW_Delta_4326 <- st_transform(WW_Delta, crs = 4326)
sf_sj <- st_crop(WW_Delta_4326, xmin =-121.550, xmax = -121.400, ymin = 37.980, ymax = 38.110)

# Change projection of sf files
#Look at CRS for CSTARS shape file for S Franks Tract, Big Break, and Clifton Court
st_crs(sf_franks)
sf_franks_4326 <- st_transform(sf_franks, crs = 4326)

st_crs(sf_bbreak)
sf_bbreak_4326 <- st_transform(sf_bbreak, crs = 4326)

st_crs(sf_ccourt)
sf_ccourt_4326 <- st_transform(sf_ccourt, crs = 4326)

st_crs(sf_ccourt)
sf_ccourt_4326 <- st_transform(sf_ccourt, crs = 4326)

st_crs(sf_sj)
sf_sj_4326 <- st_transform(sf_sj, crs = 4326)

## cstars data conversion

# cstars_format <- cstars %>%
#   mutate(species = case_when(
#     #find and replace Stuckenia_filiformis
#     str_detect(species,"filiformis")~"Stuckenia_pectinata"
#     #for all other names, keep them as they are
#     ,TRUE ~ as.character(species)
#   ))

# cstars_format2<-cstars %>%
#   #combine rake proportions for the two Stuckenia spp in the few samples where they are both present
#   group_by(latitude_wgs84,  longitude_wgs84, year, date,rake_teeth_corr,species) %>%
#   summarize(rake_prop = sum(rake_prop), .groups = 'drop')

cstars_format3 <- cstars %>%
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude_wgs84',y='latitude_wgs84'),
           crs = 4326
           ,remove=F #retains original columns
  ) %>%   #EPSG code for WGS84
  glimpse()

## Add site name for Big Break, Franks Tract, SJ ---------------------------------------

weeds_franks <- cstars_format3 %>%
  add_column(site="Franks Tract") %>%
  st_filter(sf_franks_4326)
#n=106 (not samples which are fewer)

#Filter CSTARS data set to just those within the Big Break polygon
weeds_bbreak <- cstars_format3 %>%
  add_column(site="Big Break") %>%
  st_filter(sf_bbreak_4326)
#n=61 rows (not samples which are fewer)

#Filter CSTARS data set to just those within the Clifton Court polygon
weeds_ccourt <- cstars_format3 %>%
  add_column(site="Clifton Court") %>%
  st_filter(sf_ccourt_4326)
#n=0; no sampling in clifton court

#Filter CSTARS data set to just those within the SJ polygon
weeds_sj <- cstars_format3 %>%
  add_column(site="San Joaquin") %>%
  st_filter(sf_sj_4326)
#n=0; no sampling in SJ

## Combine the  site df's into one df ---------------------------
frbb <- bind_rows(weeds_bbreak,weeds_franks)
#glimpse(frbb)
#view(frbb)
#view(org)

# Look at the species present ----------------------------------
fbs<- left_join(frbb,org)

#look at set of species
taxnat<-fbs %>%
  #removes geometry
  st_set_geometry(NULL) %>%
  distinct(across(c('species','native')))
#looks like all species are matched to an origin status as expected
#view(fbs)

#format df to create bar plot showing % rake cover by spp and site
fb_spp_cov <- fbs %>%
  #removes geometry
  st_set_geometry(NULL) %>%
  #add column that calculates absolute rake coverage by spp within sample
  mutate(rake_index = (rake_teeth_corr/100)*(rake_prop/100)) %>%
  #calculate summary stats by site and species
  group_by(site, year, species,latin_name,native) %>%
  summarize(
    rake_mean = mean(rake_index),
    rake_se = std.error(rake_index),
    rake_n = n(),
    .groups = 'drop'
  ) %>%
  #drop unneeded categories
  filter(species!="Algae" & !is.na(species) & species!= "Unidentified"& rake_n>1) %>%
  mutate(site=as.factor(site)
         ,species=as.factor(species)
  ) %>%
  glimpse()

#which species
unique(fb_spp_cov$species)


# Make facet plot ------------------------------

#plot species mean abundances by site (faceted by site) - species shortname
(plot_spp_score_avg <-ggplot(fb_spp_cov, aes(x=species, y= rake_mean, fill = species))+
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=rake_mean-rake_se, ymax=rake_mean+rake_se), width = 0.2) +
  viridis::scale_fill_viridis(discrete = TRUE, option = "turbo") +
  ylab("Mean percent of rake head covered") + xlab("Site") +
  facet_grid(site~year) +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 0.95),
         axis.title.x = element_blank(),
         strip.text = element_text(size = 10)))

#plot species mean abundances by site (faceted by year and site) - latin name
(plot_spp_score_avg <-ggplot(fb_spp_cov, aes(x=latin_name, y= rake_mean, fill = latin_name))+
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin=rake_mean-rake_se, ymax=rake_mean+rake_se), width = 0.2) +
    viridis::scale_fill_viridis(discrete = TRUE, option = "turbo") +
    ylab("Mean percent of rake head covered")  +
    facet_grid(site~year) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 0.95),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 10),
          legend.position = "none"))


# Look at data

data <- fb_spp_cov %>%
  arrange(year,latin_name)
# Make maps of sampling sites ----------------------------------

## Create bounding boxes ---------------------------------------------
# Create a bounding box based on the Franks Tract shapefile
#will be used to crop base map in plots
bbox_fr_4326 <- st_bbox(sf_franks_4326)

# Create a bounding box based on the Big Break shapefile
#will be used to crop base map in plots
bbox_bb_4326 <- st_bbox(sf_bbreak_4326)

# Create a bounding box based on the San Joaquin shapefile
#will be used to crop base map in plots
bbox_sj_4326 <- st_bbox(sf_sj_4326)

## Create maps ---------------------------------------------

# Filter to year of interest
weeds_fb_2022 <- weeds_fb %>% filter(year == "2022")

#create map showing Franks Tract SAV data points
#this shape file excludes two field points in False River I'd like to include
(sav_map_ft_only <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data=weeds_fb_2022 %>% filter(site == "Franks Tract"), fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    coord_sf(
      xlim = c(bbox_fr_4326$xmin, bbox_fr_4326$xmax),
      ylim = c(bbox_fr_4326$ymin, bbox_fr_4326$ymax)
    ) +
    theme_bw()+
    ggtitle("Franks Tract")
)

#create map showing Big Break SAV data points
(sav_map_bb_only <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data=weeds_fb_2022 %>% filter(site == "Big Break"), fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    coord_sf(
      xlim = c(bbox_bb_4326$xmin, bbox_bb_4326$xmax),
      ylim = c(bbox_bb_4326$ymin, bbox_bb_4326$ymax)
    ) +
    theme_bw()+
    ggtitle("Big Break")
)
#ggsave(file = "C:/Users/nbertrand/Desktop/Bertrand/GitHub/EDB-HABs-Weeds/analysis_2022/data_raw/Veg/BigBreak_CSTARS_SampleSites_2022.png")



#create map showing Big Break SAV data points
(sav_map_sj_only <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data=weeds_fb_2022 %>% filter(site == "San Joaquin"), fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    coord_sf(
      xlim = c(bbox_sj_4326$xmin, bbox_sj_4326$xmax),
      ylim = c(bbox_sj_4326$ymin, bbox_sj_4326$ymax)
    ) +
    theme_bw()+
    ggtitle("San Joaquin")
)

## Combine maps -------------------------------

library(patchwork)

plot_cstars_sites <- sav_map_bb_only / sav_map_ft_only

# Write plots ---------------------------------

# Comparison Franks and BB
png(filename = here("analysis_2022", "figures", "plot_cstars_sites_bb_frk.png"),
    width = 8,
    height = 6,
    units = "in", pointsize = 9,
    family = "sans", res = 300)
plot_cstars_sites
dev.off()

# Facet plot
png(filename = here("analysis_2022", "figures", "plot_cstars_rakeavg.png"),
    width = 8,
    height = 6,
    units = "in", pointsize = 9,
    family = "sans", res = 300)
plot_spp_score_avg
dev.off()
