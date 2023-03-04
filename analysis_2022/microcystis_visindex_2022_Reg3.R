# Science Hour March 2022 - Microcystis visual index figures
# Purpose: Create figures of the Microcystis visual index data for Science Hour talk
# 1) Stacked bar plot of 5 index categories by Year for Delta and Suisun Marsh
  # from 2014-2021
# 2) Stacked bar plot of 5 index categories by Month (June-Sept) and Region
  # for 2021
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

######
# Modified to include 2022 data by Kristi Arend for use in TUCO 2022 Condition 8 HABs report
## karend@usbr.gov


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(sf)
library(deltamapr)
library(patchwork)
library(fs)
library(readxl)

# Set working directory and THEN load here package
setwd("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022")
library(here)

# Check if we are in the correct working directory
i_am("analysis_2022/microcystis_visindex_2022.R")


# 2. Import and Prepare Data ----------------------------------------------

# Import Microcystis visual index data and region data
load("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/data_raw/HABsw2022.RData")
load("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/data/Regions.RData")

# Remove DOP data because different water collection method results in very different ratings
HABsVis <- HABs2022 %>%
  subset(Source != "DOP") %>%
  filter(!(Source == "FMWT" & Year == '2022'))

# Check that Sept 2022 FMWT data were removed
#check_Source_Year <- unique(HABsVis[,c("Source","Year")])

# Check most recent date for each survey (except DOP, which we won't use)
HABsVis_Summy <- HABsVis %>%
  group_by(Source) %>%
  summarize(first=min(as.Date(Date)),last=max(as.Date(Date)))

####### ADD DATA #######

#### Add complete 2022 FMWT data ####
FMWT_AllData<-read.csv("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/data_raw/FMWT 1967-2022 Catch Matrix_updated_tidy.csv")

# Add a month column
FMWT_AllData$Month <- month(ymd(FMWT_AllData$SampleDate))

# Select columns to add to HABsVis; rearrange order, rename, reformat as needed
FMWT_2022 <- FMWT_AllData %>%
  filter(Year == '2022') %>%
  select((StationCode), StationLat, StationLong, SampleDate, Microcystis, Secchi, WaterTemperature, Month, Year) %>%
  rename(Station = StationCode, Latitude = StationLat, Longitude = StationLong, Date = SampleDate,
         Temperature = WaterTemperature) %>%
  mutate(across(Station, as.character)) %>%
  mutate(Date = as.Date(Date))

# FMWT_2022 has multiple rows per sample station*date, because repeates for each fish species caught
## Trim to just one row per site*date
FMWT_2022trim <- unique(FMWT_2022)

# Add a column, Source, that labels the data as FMWT
FMWT_2022trim$Source <- replicate(nrow(FMWT_2022trim), 'FMWT')

# Merge FMWT_2022 with HABsVis
HABsVis2022 <- full_join(HABsVis, FMWT_2022trim)

# make sure Station is character format, not double
HABsVis2022$Station <- as.character(HABsVis2022$Station)

#### Add fall USGS data ####
# Extract data from the third tab in the file updated on Feb 10, 2023
USGS_FallNew <- read_excel("data_raw/USGS_data_WW_Spatt_Microcystis_updated20230210.xlsx", 3)


# Add lat and long columns
## extract Station name and lat and long from existing 2022 data
USGS_Site_LatLong <- HABsVis2022 %>%
  filter(Source == "USGS") %>%
  select(Station, Latitude, Longitude) %>%
  rename(field_ID = Station) %>%
  unique()

## Add lat and long to updated data frame
USGS_FallNew <- left_join(USGS_FallNew, USGS_Site_LatLong, by = "field_ID")
## create df that can be merged with HABsVis2022
USGS_FallNew_Select <- USGS_FallNew %>%
  select(field_ID, Latitude, Longitude, 'Start_timestamp_(PST)', MC_rating) %>%
  rename(Station = field_ID, Microcystis = MC_rating, Date = 'Start_timestamp_(PST)') %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Microcystis = as.double(Microcystis))

# Add month and year columns
USGS_FallNew_Select$Month <- month(ymd(USGS_FallNew_Select$Date))
USGS_FallNew_Select$Year <- year(ymd(USGS_FallNew_Select$Date))

# Add a column, Source, that labels the data as USGS
USGS_FallNew_Select$Source <- replicate(nrow(USGS_FallNew_Select), 'USGS')

# Reorder columns to match HABsVis2022
USGS_FallNew_Select <- USGS_FallNew_Select %>%
  select(Source, Station, Latitude, Longitude, Date, Microcystis, Month, Year)


# Merge USGS_FallNew_Select with HABsVis2022
HABsVis2022 <- full_join(HABsVis2022, USGS_FallNew_Select)


#### Add fall EMP data ####
# Import additional DWR_EMP field data for 2022 - this includes EZ station
# coordinates - provided from personal data request


# File path
fp_nutr_chla <- dir(here("analysis_2022/data_raw"), full.names = TRUE)

df_emp_FallField_2022 <-
  read_excel(
    path = str_subset(fp_nutr_chla, "EMP_Discrete_WQ_Data_Sep_Nov_2022"),
    range = "A86:AE168",
    col_types = "text",
    skip=1
  )

df_emp_Fallnutschl <-
  read_excel(
    path = str_subset(fp_nutr_chla, "EMP_Discrete_WQ_Data_Sep_Nov_2022"),
    range = "A2:Y84",
    col_types = "text",
    skip=1
  )

df_emp_coord <- read_csv(here("analysis_2022", "data_raw", "stations_emp.csv"))

# Prepare DWR_EMP station coordinates
df_emp_coord_c <- df_emp_coord %>%
  select(Station, Latitude, Longitude) %>%
  drop_na()

# Prepare DWR_EMP EZ station coordinates
df_emp_coord_ez21 <- df_emp_FallField_2022 %>%
  select(
    SampleCode = `Sample Code`,
    Latitude_field = contains("Latitude"),
    Longitude_field = contains("Longitude")
  ) %>%
  filter(!if_any(c(Latitude_field, Longitude_field), ~ .x == "N/A")) %>%
  mutate(across(ends_with("_field"), as.numeric))


# Prepare 2022 EMP discrete nutrient and chlorophyll-a data to be combined with
# all other data
df_emp_FallField_2022_c <- df_emp_FallField_2022 %>%
  # Select and standardize variable names
  select(
    Station = `Station Name`,
    StationNumber = `Station Number`,
    SampleCode = `Sample Code`,
    Datetime = `Sample Date`,
    Microcystis = contains("Microcystis"),
    Secchi = contains("Secchi"),
    Temperature = 'Water Temperature °C EPA 170.1 (Field) - [1]*',
    Chlorophyll = 'Chlorophyll Fluorescence ug/L of Chl Fluorescence, Sonde - [1]*'
  ) %>%
  # Parse Datetime (as PST) and create Source and Date variables
  mutate(
    Datetime = mdy_hm(Datetime, tz = "Etc/GMT+8"),
    Date = date(Datetime),
    Month = month(Datetime),
    Year = year(Datetime),
    Source = "EMP",
    Microcystis = as.double(Microcystis),
    Secchi = as.double(Secchi),
    Temperature = as.double(Temperature),
    Chlorophyll = as.double(Chlorophyll)
  ) %>%
  mutate(
    # Standardize Stations
    Station = case_when(
      str_detect(Station, "^SF Estuarine") ~ StationNumber,
      str_detect(Station, "- C3A") ~ "C3A",
      str_detect(Station, "^NZ068") ~ "NZ068",
      str_detect(Station, " - ") ~ str_extract(Station, ".+(?= - )")
    )) %>%
  # Add station coordinates
  left_join(df_emp_coord_c, by = "Station") %>%
  left_join(df_emp_coord_ez21, by = "SampleCode") %>%
  mutate(
    Latitude = if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude = if_else(is.na(Longitude), Longitude_field, Longitude)
  ) %>%
  # remove station EZ6 data because it is redundant with station D22 (EZ6 nutrient samples were
    # collected at D22, so water samples differed but field data did not)
  filter(!Station == 'EZ6') %>%
  # Select variable order
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Microcystis,
    Secchi,
    Temperature,
    Month,
    Year,
    Chlorophyll
  )

# get lab chlorophyll data from lab data and append to field data
df_emp_ChlLab <- df_emp_Fallnutschl %>%
  select(
    Station = `Station Name`,
    StationNumber = 'Station Number',
    Datetime = `Sample Date`,
    Chlorophyll_lab = contains("Chlorophyll"),
  ) %>%
  # Parse Datetime (as PST) and create Source and Date variables
  mutate(
    Datetime = mdy_hm(Datetime, tz = "Etc/GMT+8"),
    Date = date(Datetime),
    Chlorophyll_lab = as.double(Chlorophyll_lab)
  ) %>%
  mutate(
    # Standardize Stations
    Station = case_when(
      str_detect(Station, "^SF Estuarine") ~ StationNumber,
      str_detect(Station, "- C3A") ~ "C3A",
      str_detect(Station, "^NZ068") ~ "NZ068",
      str_detect(Station, " - ") ~ str_extract(Station, ".+(?= - )")
    )) %>%
  # remove station EZ6 data because it is redundant with station D22 and only want the lab
  # chl data from one water sample to match with the visual microcystis score
  filter(!Station == 'EZ6') %>%
  select(!c(StationNumber, Datetime))

# Append lab chlorophyll a data to the rest of the EMP data
df_emp_FallVisMicro_2022_c <- right_join(df_emp_FallField_2022_c,df_emp_ChlLab, by=c("Station", "Date"))

# Merge df_emp_FallVisMicro_2022_c with HABsVis2022
HABsVis2022 <- full_join(HABsVis2022, df_emp_FallVisMicro_2022_c)


##### Add fall NCRO data #####
# Data file contains all of the 2022 data (July-December)
df_ncro_FallVisMicro_2022 <-
  read_excel("data_raw/NCRO_HabObs_20220705-20221231.xlsx")

## Remove all NCRO 2022 data from HABsVis2022
HABsVis2022 <- HABsVis2022 %>%
  filter(!(Source == "FMWT" & Year == '2022'))

# Load NCRO coordinate data
df_ncro_coord <- read.csv("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/data_clean/discrete_wq_stations_all_2022.csv")

df_ncro_VisMic2022_c <- df_ncro_FallVisMicro_2022 %>%
  # Select and standardize station names
  mutate(
    Station = case_when(
      str_detect(StationName, "^Bethel") ~ "BET",
      str_detect(StationName, "^False") ~ "FAL",
      str_detect(StationName, "^Fisherman") ~ "FCT",
      str_detect(StationName, "^Holland") ~ "NA_HOL",
      str_detect(StationName, "^Middle River at Howard") ~ "MHO",
      str_detect(StationName, "^Middle River at Undine") ~ "MRU",
      str_detect(StationName, "^Middle River near Holt") ~ "HLT",
      str_detect(StationName, "^Middle River near Tracy") ~ "MRX",
      str_detect(StationName, "^Old River at Tracy") ~ "OLD",
      str_detect(StationName, "^Old River near Bacon") ~ "OBI",
      str_detect(StationName, "^Old River near Frank") ~ "OSJ",
      str_detect(StationName, "^Old River near Head") ~ "OH1",
      str_detect(StationName, "^Old River at Clifton Court Intake") ~ "ORI",
      str_detect(StationName, "^Old River below DMC barrier") ~ "OBD",
      str_detect(StationName, "^Old River above Doughty Cut") ~ "ORX",
      str_detect(StationName, "^Old River Upstream of Mountain") ~ "ORM",
      str_detect(StationName, "^Miner Slough") ~ "MIR",
      str_detect(StationName, "^Paradise") ~ "NA_PDC",
      str_detect(StationName, "^Rock Slough at CCWD/Trash Rack") ~ "NA_RSL",
      str_detect(StationName, "^Sacramento R Downstream of Isleton") ~ "SOI",
      str_detect(StationName, "^San Joaquin River at Blind") ~ "BLP",
      str_detect(StationName, "Steamboat Slough near Sacramento River") ~ "SXS",
      str_detect(StationName, "^Sugar Cut") ~ "SGA",
      str_detect(StationName, "^Three") ~ "TSL",
      str_detect(StationName, "^Turner") ~ "TRN",
      str_detect(StationName, "^Victoria") ~ "VCU",
      str_detect(StationName, "^West Canal") ~ "WCI",
      str_detect(StationName, "^Yolo") ~ "LIS",
      str_detect(StationName, "^Grant Line Canal East") ~ "GLE",
      str_detect(StationName, "^Grant Line Canal near Old River") ~ "DGL"))
df_ncro_VisMic2022_c <- df_ncro_VisMic2022_c %>%
  mutate(MicroText = (FldObsWaterHabs),
    Date = date(DeploymentEnd),
    Month = month(Date),
    Year = year(Date))

# Add station coordinates
df_ncro_VisMic2022_c <- df_ncro_VisMic2022_c %>%
  left_join(df_ncro_coord, by = "Station") %>%
  # Add Source column
  mutate(
    Source = "NCRO"
  )

# Convert text rankings of microcystis to numeric rankings
## Absent = 1; Low = 2; Moderate = 3; High = 4; Very High = 5 per methods
## NCRO text rankings:
NCRO_VisMicro_RankCategories <- unique(df_ncro_VisMic2022_c$MicroText)
# Not Visible = 1; Low = 2; Medium = 3; High = 4; Extreme = 5; NA = not recorded
df_ncro_VisMic2022_c <- df_ncro_VisMic2022_c %>%
  mutate(
    Microcystis = as.numeric(c("Not Visible"="1", "Low"="2", "Medium"="3",
                               "High"="4", "Extreme"="5")[df_ncro_VisMic2022_c$MicroText])
  ) %>%
  # Select variable order
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Microcystis,
    Month,
    Year
  )

# Merge df_ncro_VisMic2022_c with HABsVis2022
HABsVis2022 <- full_join(HABsVis2022, df_ncro_VisMic2022_c)

##### Check start and end dates for each source #####
HABsVis_Summy_All <- HABsVis2022 %>%
  group_by(Source) %>%
  summarize(first=min(as.Date(Date)),last=max(as.Date(Date)))

##### Add column to HABsVis2022 that just indicates presence (1) and absence (0)

# Remove any NA Microcystis entries
HABsVis_final_2007_2022 <- HABsVis2022 %>%
  drop_na(Microcystis)

# Add column with presence / absence...this a very clunky way to do this
HABsVis_final_2007_2022 <- HABsVis_final_2007_2022 %>%
  mutate(
    MicroPA = as.numeric(c("1"="0", "2"="1", "3"="1",
                 "4"="1", "5"="1")[HABsVis_final_2007_2022$Microcystis])) %>%
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Month,
    Year,
    Microcystis,
    MicroPA,
    Secchi,
    Temperature,
    Chlorophyll,
    Chlorophyll_lab
  )

##### Export dataframe to clean data folder #####
write_csv(HABsVis_final_2007_2022, here("analysis_2022", "data_clean", "HABsVis_final_2007_2022.csv"))


######## PREP DATA FOR FIGS #######
#### Remove data with missing information; add region 3 strata (Shows Franks, OMR, etc.)

# Load HABsVis_final_2007_2022
HABsVis_final_2007_2022 <- read.csv("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/data_clean/HABsVis_final_2007_2022.csv")

# Load region data if needed
load("C:/Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/data/Regions.RData")

df_mvi_c <- HABsVis_final_2007_2022 %>%
  select(Source, Station, Latitude, Longitude, Date, Month, Year, Microcystis, MicroPA) %>%
  filter(
    # Only keep data from 2014-2022 in April-November
    Year %in% 2014:2022,
    Month %in% 4:11,
    !is.na(Latitude)
  ) %>%
  # Assign Strata from R_EDSM_Strata_1718P1 shapefile
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  #st_transform(crs = st_crs(R_EDSM_Strata_1718P1)) %>%
  st_join(reg3) %>%
  #st_join(R_EDSM_Strata_1718P1, join = st_intersects) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>%
  # Remove any stations outside of the R_EDSM_Strata_1718P1 strata and from
    # Western Delta since there are so few observations with Microcystis present
    # in this region
  filter(
    !is.na(Stratum)
 #  , Stratum != "Western Delta"
  ) %>%
  mutate(
    # Convert Stratum to factor and rename it Region
#    Region = factor(Stratum, levels = vec_strata_levels, labels = vec_strata_labels),
    Region = factor(Stratum2),
    # Convert Microcystis to factor
#    Microcystis = factor(Microcystis, labels = c("Absent", "Low", "Medium", "High", "Very High"))

  ) %>%
  # Clean up data frame
  select(
    Source,
    Station,
    Region,
    Latitude,
    Longitude,
    Date,
    Month,
    Year,
    Microcystis,
    MicroPA
  )

# Add columns that have character strings for Microcystis ranking and presence/absence

df_mvi_c_factor <- df_mvi_c %>%
  mutate(
    MicroRank = as.character(c("1"="Absent", "2"="Low", "3"="Medium",
                             "4"="High", "5"="Very High")[df_mvi_c$Microcystis]),
    MicroPA_txt = factor(MicroPA, levels = c(0,1), labels = c("Absent", "Present")),
  )



######## CREATE FIGS #######


# set strata levels for plotting
df_mvi_c_factor$Region = factor(df_mvi_c$Region, levels=c("Upper Sac", "Cache/Liberty", "Lower Sac", "East Delta",
                                                   "Lower SJ", "Franks", "OMR", "South Delta"))
# Set ranking levels for plotting
df_mvi_c_factor$MicroRank = factor(df_mvi_c$MicroRank, levels=c("Absent", "Low",
                                                   "Medium", "High", "Very High"))

# Create stacked bar plot of 5 index categories by Year for all data from 2014-2022
barplt_year <- df_mvi_c_factor %>%
  ggplot(aes(x = Year, fill = MicroRank)) +
  geom_bar(position = "fill") +
#  scale_fill_mvi +
  scale_fill_manual(
    name = "Ranking",
    values = c("gray65", viridis::viridis(5, option = "plasma")[1:4]),
    breaks=c("Absent", "Low","Medium", "High", "Very High")
  ) +
  scale_y_continuous(
    name = "Relative Frequency",
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_x_continuous(
    breaks = c(2014:2022),
    expand = expansion(mult = c(0.02, 0.02))
  )

# Create stacked bar plots of 5 index categories by by Month (Apr-Nov) and Region
  # for 2021 and 2022, separately
barplt_2021 <- df_mvi_c_factor %>%
  filter(
    Year == 2021
  ) %>%
  ggplot(aes(x = Region, fill = MicroRank)) +
  geom_bar(position = "fill") +
  facet_grid(cols = vars(Month)) +
  scale_fill_manual(
    name = "Ranking",
    values = c("gray65", viridis::viridis(5, option = "plasma")[1:4]),
    breaks=c("Absent", "Low","Medium", "High", "Very High")
  ) +
  scale_y_continuous(
    name = "Relative Frequency",
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.025))
  ) +
  ggtitle('2021') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

barplt_2022 <- df_mvi_c_factor %>%
  filter(
    Year == 2022) %>%
  ggplot(aes(x = Region, fill = MicroRank)) +
  geom_bar(position = "fill") +
  facet_grid(cols = vars(Month)) +
  scale_fill_manual(
    name = "Ranking",
    values = c("gray65", viridis::viridis(5, option = "plasma")[1:4]),
    breaks=c("Absent", "Low","Medium", "High", "Very High")
  ) +
  scale_y_continuous(
    name = "Relative Frequency",
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.025))
  ) +
  ggtitle('2022') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Use patchwork to put them together
VisMicro_Month_2021_2022 = barplt_2021 / barplt_2022


# Create stacked bar plot of Presence/Absence by Year for all data from 2014-2022
barplt_PA_year <- df_mvi_c_factor %>%
  ggplot(aes(x = Year, fill = MicroPA_txt)) +
  geom_bar(position = "fill") +
  scale_fill_manual("legend", values = c("Absent" = "lightcyan1", "Present" = "green4")) +
  scale_y_continuous(
    name = "Relative Frequency",
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_x_continuous(
    breaks = c(2014:2022),
    expand = expansion(mult = c(0.02, 0.02))
  )

# Create stacked bar plots of Presence/Absence by Month (Apr-Nov) and Region
# for 2021 and 2022, separately
barplt_PA_2021 <- df_mvi_c_factor %>%
  filter(
    Year == 2021
  ) %>%
  ggplot(aes(x = Region, fill = MicroPA_txt)) +
  geom_bar(position = "fill") +
  facet_grid(cols = vars(Month)) +
  scale_fill_manual("legend", values = c("Absent" = "lightcyan1", "Present" = "green4")) +
  scale_y_continuous(
    name = "Relative Frequency",
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.025))
  ) +
  ggtitle('2021') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

barplt_PA_2022 <- df_mvi_c_factor %>%
  filter(
    Year == 2022) %>%
  ggplot(aes(x = Region, fill = MicroPA_txt)) +
  geom_bar(position = "fill") +
  facet_grid(cols = vars(Month)) +
  scale_fill_manual("legend", values = c("Absent" = "lightcyan1", "Present" = "green4")) +
  scale_y_continuous(
    name = "Relative Frequency",
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.025))
  ) +
  ggtitle('2022') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Use patchwork to put them together
VisMicro_PA_Month_2021_2022 = barplt_PA_2021 / barplt_PA_2022


# 4. Export Figures -------------------------------------------------------

# Stacked bar plot by Year for all data from 2014-2021
# this code decided to stop working for some unknown reason
#ggsave(
#  "C://Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/figures/Microcystis_visindex_by_Year_030323.jpg",
#  plot = barplt_year,
#  height = 4.5,
#  width = 6.5,
#  units = "in"
#)

jpeg("C://Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/figures/Microcystis_visindex_by_Year_030323.jpg")
plot(barplt_year,
     height = 4.5,
     width = 6.5,
     units = "in")
dev.off()
# Stacked bar plot by Month and Region for 2021 and 2022

jpeg("C://Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/figures/Microcystis_visindex_month_reg_20212022_030323.jpg")
plot(VisMicro_Month_2021_2022,
     height = 4.5,
     width = 6.5,
     units = "in")
dev.off()


# Stacked P/A bar plot by Year for all data from 2014-2021
jpeg("C://Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/figures/Microcystis_visindex_PA_by_Year_030323.jpg")
plot(barplt_PA_year,
     height = 4.5,
     width = 6.5,
     units = "in")
dev.off()


# Stacked bar plot by Month and Region for 2021 and 2022
ggsave(
  here("analysis_2022/figures/Microcystis_visindex_PA_month_reg_20212022_030323.jpg"),
  plot = VisMicro_PA_Month_2021_2022,
  height = 12,
  width = 9,
  units = "in"
)

jpeg("C://Users/karend/Desktop/HABs_AqVeg/EDB-HABs-Weeds/analysis_2022/figures/Microcystis_visindex_PA_month_reg_20212022_030323.jpg")
plot(VisMicro_PA_Month_2021_2022,
     height = 4.5,
     width = 6.5,
     units = "in")
dev.off()

