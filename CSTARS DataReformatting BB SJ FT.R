#Final Code used in 2022 submerged aquatic vegetion in HABS report. This is modified from code written by DWR staff as noted below.
#The original code contained some exploratory efforts not needed to generate the figures for the report.
#This code is streamlined to convert 2022 datafiles (which were formatted differently from 2021) and
#only generate the figueres needed for the report.



#2022 author Nick Bertrand
#Nbertrand@usbr.gov
#Edited in 2022 by Nick Bertrand nbertrand@usbr.gov


# Aquatic Vegetation Project Work Team
# Master data set
# Submersed aquatic vegetation

#original code author
#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#####################
########
#####
#2022 CSTARS data reformatting to fit the CSTARS_2021_GroundTruthing_SAV.R structure
#franks Tract data

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


cstars22.con$time <- as.character(cstars22.con$time)
#view(cstars22.con)

#####################################
##############
#########

#data reformatting for the Big Break, San Joaquin sites begins here and colminates in combining Franks tract, Big break,
#and San Joaquin into a single data frame to be used by the CSTARS ground Truthing file.

#original file taken from link below.
#https://github.com/InteragencyEcologicalProgram/AquaticVegetationPWT/blob/e8efc9630dbdcddaa68d2255f9ee3d39f8e88777/MasterDataSet_SAV/Scripts_DataFormatting/CSTARS_GroundTruthing_DataFormatting_FranksTract2022.R

#Edited in 2022 by Nick Bertrand nbertrand@usbr.gov
#edited to reformat BBgrid_2022.dbf
##############################################
#############################
#############

#install.packages('janitor', dependencies=TRUE, repos='http://cran.rstudio.com/')



#original file taken from link below.
#https://github.com/InteragencyEcologicalProgram/AquaticVegetationPWT/blob/e8efc9630dbdcddaa68d2255f9ee3d39f8e88777/MasterDataSet_SAV/Scripts_DataFormatting/CSTARS_GroundTruthing_DataFormatting_FranksTract2022.R

#Edited in 2022 by Nick Bertrand nbertrand@usbr.gov
#edited to reformat BBgrid_2022.dbf

# Aquatic Vegetation Project Work Team
# Master data set
# Submersed aquatic vegetation
# CSTARS ground truthing
# 2022 Franks Tract

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

# Packages--------
library(tidyverse) #suite of data science tools
library(sf) #work with GPS coordinates
library(janitor) #make column names tidier
library(foreign) #read dbf files
library(lubridate) #formatting dates
library(deltamapr) #Sam's package with shapefiles for delta waterways
library(plotrix) #standard error function

# Read in the data----------------------------------------------

#2022 Big Break data
bb <- read.dbf("C:/Users/nbertrand/Desktop/Bertrand/GitHub/EDB-HABs-Weeds/analysis_2022/data_raw/Veg/BBgrid_2022.dbf")

bb$GPS_Time <- as.character(parse_date_time(bb$GPS_Time, "%I:%M:%S%p"))
bb$GPS_Time <- as.POSIXct(bb$GPS_Time)
bb$GPS_Time <-format(bb$GPS_Time,"%H:%M:%OS")
bb$GPS_Time <-as.character(bb$GPS_Time)


#Sepro survey grid
#CRS is WGS 84; already contains a geometry column
#sepro <- st_read("Data_Raw/sepro_franks_tract/Franks Points.gpx")
#glimpse(sepro)

#Format the data -----------------

#reduce data set to just the columns I need
bb22 <- bb %>%
  #use janitor function to clean up column names
  clean_names() %>%
  #rename some columns
  rename(
    ucd_id = point_id
    #,sepro_id = grid_id
    #CSTARS said the distances are in meters
    #,pt_dist_m = near_dist
    ,date = gps_date
    ,time = gps_time
    ,feat = feat_name
    #CRS is UTM NAD 83 (Zone 10N) (EPSG = 26910)
    ,northing_26910 = northing
    ,easting_26910 = easting
  ) %>%
  #removes all % from data frame but converts all columns to character which is a hassle
  #mutate_all(str_replace_all, "%", "")
  #instead remove % from specific columns and make those columns numeric
  mutate(rake_teeth = str_replace(rake_teeth,pattern = "%", replacement = "")
         ,rake_spec2 = str_replace(rake_spec2,pattern = "%", replacement = "")
         ,rake_spec4 = str_replace(rake_spec4,pattern = "%", replacement = "")
         ,rake_spec6 = str_replace(rake_spec6,pattern = "%", replacement = "")
         ,rake_spec8 = str_replace(rake_spec8,pattern = "%", replacement = "")
         ,rake_spe10 = str_replace(rake_spe10,pattern = "%", replacement = "")
         ,across(c(rake_teeth,rake_spec2,rake_spec4,rake_spec6, rake_spec8, rake_spe10),as.numeric)
  ) %>%
  #create new column that calculates total rake coverage of spp
  #should all be either 0 or 100
  rowwise() %>%
  mutate(tot_cover_spp = sum(c(rake_spec2,rake_spec4,rake_spec6, rake_spec8, rake_spe10),na.rm=T)) %>%
  #make new columns that will allow for comparison of rake coverage to spp level coverage
  #values for rake_teeth_logical and tot_cover_spp_logical should match
  #ie, both be zero or both be one; mismatch often, but not always, indicates errors
  mutate(rake_teeth_logical = if_else(rake_teeth > 0, 1,0)
         ,tot_cover_spp_logical = if_else(tot_cover_spp > 0, 1,0)
         ,rake_diff = abs(rake_teeth_logical-tot_cover_spp_logical)
         #there are five samples indicating a difference (ie, possible error)
         #ucd #24: replace one incorrect case of rake_teeth=100% to rake_teeth=0%
         #confirmed this error with CSTARS
         ,rake_teeth_corr = if_else((rake_diff==1 & is.na(rake_speci)),0,rake_teeth)
         #ucd #29: replace one incorrect case of southern naiad listed as 0% when should be 100%
         #confirmed this error with CSTARS
         ,rake_spec2_corr = ifelse(ucd_id == 29, 100,rake_spec2)
         #the other three samples were trace amounts of spp so rake teeth was entered at 0%
         #even though there were species present
  ) %>%
  arrange(rake_diff)  %>%
  select(ucd_id
         #,sepro_id
         #,pt_dist_m
         ,northing_26910
         ,easting_26910
         ,date
         ,time
         ,feat
         ,rake_teeth_corr
         ,rake_speci
         ,rake_spec2_corr
         ,rake_spec3:rake_spe10
         ,tot_cover_spp:rake_diff
  ) %>%
  glimpse()

#convert data frame from wide to long
bb_long <- bb22 %>%
  #drop some unneeded columns
  select(-c(tot_cover_spp:rake_diff)) %>%
  #in prep for converting wide-ish to longest, rename some columns
  rename(rake_teeth = rake_teeth_corr
         ,rake_spec1 = rake_speci
         ,rake_prop1 = rake_spec2_corr
         ,rake_prop3 = rake_spec4
         ,rake_prop5 = rake_spec6
         ,rake_prop7 = rake_spec8
         ,rake_prop9 = rake_spe10
  )  %>%
  #convert the rake prop columns back to factors for converting from wide to long
  mutate(across(c(rake_prop1,rake_prop3,rake_prop5,rake_prop7,rake_prop9),as.factor)) %>%
  #convert wide to long
  pivot_longer(cols="rake_spec1":"rake_prop9" #select range of columns
               , names_to = c("name","num") #specify column names
               , names_pattern = '([^0-9]+)([0-9]+)' #indicate where to split names (before and after numbers)
               , values_to = "value")  %>%
  glimpse()


#convert from long to wideish
bb_wide <- bb_long %>%
  #now pivot back to a bit wider
  pivot_wider(names_from=name, values_from=value) %>%
  #remove rows where species is NA
  #these are just artifacts of original data structure
  #NOTE: this also drops three samples that aren't SAV (ie, FAV, EAV)
  #If a species is listed but has a rake prop = 0 it means trace amounts present
  filter(!(is.na(rake_spec))) %>%
  mutate(
    #make rake prop numeric instead of factor
    rake_prop = as.numeric(as.character(rake_prop))
    #fix one case in which a species is listed but rake prop is NA when it should be zero
    #indicating trace amounts of it on the rake
    ,rake_prop_corr = if_else((is.na(rake_prop)),0,rake_prop)
  )  %>%
  #drop unneeded columns
  select(-c(num,rake_prop)) %>%
  rename(rake_prop = rake_prop_corr) %>%
  glimpse()
#view(bb_wide)


#make data set wider so it matches SePRO past data
bb_wider <- bb_wide %>%
  #drop the SAV prefix from spp names
  mutate(species = str_replace_all(rake_spec,"SAV-","")) %>%
  #drop unneeded columns
  select(-c(rake_spec)) %>%
  #pivot wider
  pivot_wider(names_from = "species", values_from = "rake_prop")
#view(bb_wider)

########################
##############
########
#big break reformatting to Cstars 2021 format

#creates object bb22.start from bb_wider
bb22.start <- bb_wider

#gather function consolidates the species into a count column and drops out any NAs from the data.
bb22.gath <- bb22.start %>% gather("species","rake_prop", Watermilfoil:Snaiad) %>%
  drop_na(rake_prop)
#view(bb22.gath)

wgs84conversion <- bb22.gath %>%
  st_as_sf(coords = c("easting_26910", "northing_26910"), crs = 26910) %>%
  st_transform(4326) %>%
  st_coordinates()

wgs84conversion <-as.data.frame(wgs84conversion)

#view(wgs84conversion)

wgs84conversion<- rename(wgs84conversion, latitude_wgs84 = Y , longitude_wgs84 = X)

#creates big break dataframe to be joined to final data frame
bb22.con <-cbind(bb22.gath, wgs84conversion) %>%
  select(latitude_wgs84, longitude_wgs84, date, time,feat,rake_teeth, species, rake_prop) %>%
  rename(survey_method = feat, rake_teeth_corr = rake_teeth)

#view(bb22.con)

#######################
###############
#########

#code is the same as above but modified to accept the SAVFLOAT.dbf data file which is the data from the
#SanJoaquin River sampling

# Read in the data----------------------------------------------

#2022 San Joaquin data collected by CSTARS
sj <- read.dbf("C:/Users/nbertrand/Desktop/Bertrand/GitHub/EDB-HABs-Weeds/analysis_2022/data_raw/Veg/SAVFLOAT.dbf")
#view(sj22)
sj$GPS_Time <- as.character(parse_date_time(sj$GPS_Time, "%I:%M:%S%p"))
sj$GPS_Time <- as.POSIXct(sj$GPS_Time)
sj$GPS_Time <-format(sj$GPS_Time,"%H:%M:%OS")
sj$GPS_Time <-as.character(sj$GPS_Time)

#view(sj)


#Format the data -----------------

#reduce data set to just the columns I need
sj22 <- sj %>%
  #use janitor function to clean up column names
  clean_names() %>%
  #rename some columns
  rename(
    ucd_id = point_id
    #,sepro_id = grid_id
    #CSTARS said the distances are in meters
    #,pt_dist_m = near_dist
    ,date = gps_date
    ,time = gps_time
    ,feat = feat_name
    #CRS is UTM NAD 83 (Zone 10N) (EPSG = 26910)
    ,northing_26910 = northing
    ,easting_26910 = easting
  ) %>%
  #removes all % from data frame but converts all columns to character which is a hassle
  #mutate_all(str_replace_all, "%", "")
  #instead remove % from specific columns and make those columns numeric
  mutate(rake_teeth = str_replace(rake_teeth,pattern = "%", replacement = "")
         ,rake_spec2 = str_replace(rake_spec2,pattern = "%", replacement = "")
         ,rake_spec4 = str_replace(rake_spec4,pattern = "%", replacement = "")
         ,rake_spec6 = str_replace(rake_spec6,pattern = "%", replacement = "")
         ,rake_spec8 = str_replace(rake_spec8,pattern = "%", replacement = "")
         ,rake_spe10 = str_replace(rake_spe10,pattern = "%", replacement = "")
         ,across(c(rake_teeth,rake_spec2,rake_spec4,rake_spec6, rake_spec8, rake_spe10),as.numeric)
  ) %>%
  #create new column that calculates total rake coverage of spp
  #should all be either 0 or 100
  rowwise() %>%
  mutate(tot_cover_spp = sum(c(rake_spec2,rake_spec4,rake_spec6, rake_spec8, rake_spe10),na.rm=T)) %>%
  #make new columns that will allow for comparison of rake coverage to spp level coverage
  #values for rake_teeth_logical and tot_cover_spp_logical should match
  #ie, both be zero or both be one; mismatch often, but not always, indicates errors
  mutate(rake_teeth_logical = if_else(rake_teeth > 0, 1,0)
         ,tot_cover_spp_logical = if_else(tot_cover_spp > 0, 1,0)
         ,rake_diff = abs(rake_teeth_logical-tot_cover_spp_logical)
         #there are five samples indicating a difference (ie, possible error)
         #ucd #24: replace one incorrect case of rake_teeth=100% to rake_teeth=0%
         #confirmed this error with CSTARS
         ,rake_teeth_corr = if_else((rake_diff==1 & is.na(rake_speci)),0,rake_teeth)
         #ucd #29: replace one incorrect case of southern naiad listed as 0% when should be 100%
         #confirmed this error with CSTARS
         ,rake_spec2_corr = ifelse(ucd_id == 29, 100,rake_spec2)
         #the other three samples were trace amounts of spp so rake teeth was entered at 0%
         #even though there were species present
  ) %>%
  arrange(rake_diff)  %>%
  select(ucd_id
         #,sepro_id
         #,pt_dist_m
         ,northing_26910
         ,easting_26910
         ,date
         ,time
         ,feat
         ,rake_teeth_corr
         ,rake_speci
         ,rake_spec2_corr
         ,rake_spec3:rake_spe10
         ,tot_cover_spp:rake_diff
  ) %>%
  glimpse()


#convert data frame from wide to long
sj_long <- sj22 %>%
  #drop some unneeded columns
  select(-c(tot_cover_spp:rake_diff)) %>%
  #in prep for converting wide-ish to longest, rename some columns
  rename(rake_teeth = rake_teeth_corr
         ,rake_spec1 = rake_speci
         ,rake_prop1 = rake_spec2_corr
         ,rake_prop3 = rake_spec4
         ,rake_prop5 = rake_spec6
         ,rake_prop7 = rake_spec8
         ,rake_prop9 = rake_spe10
  )  %>%
  #convert the rake prop columns back to factors for converting from wide to long
  mutate(across(c(rake_prop1,rake_prop3,rake_prop5,rake_prop7,rake_prop9),as.factor)) %>%
  #convert wide to long
  pivot_longer(cols="rake_spec1":"rake_prop9" #select range of columns
               , names_to = c("name","num") #specify column names
               , names_pattern = '([^0-9]+)([0-9]+)' #indicate where to split names (before and after numbers)
               , values_to = "value")  %>%
  glimpse()



#convert from long to wideish
sj_wide <- sj_long %>%
  #now pivot back to a bit wider
  pivot_wider(names_from=name, values_from=value) %>%
  #remove rows where species is NA
  #these are just artifacts of original data structure
  #NOTE: this also drops three samples that aren't SAV (ie, FAV, EAV)
  #If a species is listed but has a rake prop = 0 it means trace amounts present
  filter(!(is.na(rake_spec))) %>%
  mutate(
    #make rake prop numeric instead of factor
    rake_prop = as.numeric(as.character(rake_prop))
    #fix one case in which a species is listed but rake prop is NA when it should be zero
    #indicating trace amounts of it on the rake
    ,rake_prop_corr = if_else((is.na(rake_prop)),0,rake_prop)
  )  %>%
  #drop unneeded columns
  select(-c(num,rake_prop)) %>%
  rename(rake_prop = rake_prop_corr) %>%
  glimpse()
#view(sj_wide)


#make data set wider so it matches SePRO past data
sj_wider <- sj_wide %>%
  #drop the SAV prefix from spp names
  mutate(species = str_replace_all(rake_spec,"SAV-","")) %>%
  #drop unneeded columns
  select(-c(rake_spec)) %>%
  #pivot wider
  pivot_wider(names_from = "species", values_from = "rake_prop")
#view(sj_wider)

########################
##############
########
#San Joaquin reformatting to Cstars 2021 format

#creates object sj22.start from sj_wider
sj22.start <- sj_wider

#gather function consolidates the species into a count column and drops out any NAs from the data.
sj22.gath <- sj22.start %>% gather("species","rake_prop", Coontail:Watermilfoil) %>%
  drop_na(rake_prop)
#view(sj22.gath)

wgs84conversion <- sj22.gath %>%
  st_as_sf(coords = c("easting_26910", "northing_26910"), crs = 26910) %>%
  st_transform(4326) %>%
  st_coordinates()

wgs84conversion <-as.data.frame(wgs84conversion)

#view(wgs84conversion)

wgs84conversion<- rename(wgs84conversion, latitude_wgs84 = Y , longitude_wgs84 = X)

#this generates data frame to be joined to the final dataframe
sj22.con <-cbind(sj22.gath, wgs84conversion) %>%
  select(latitude_wgs84, longitude_wgs84, date, time,feat,rake_teeth, species, rake_prop) %>%
  rename(survey_method = feat, rake_teeth_corr = rake_teeth)

#view(sj22.con)

##############################
########
###

#begin combining reforamtted data.
bbsj <- union(bb22.con, sj22.con)

#view(bbsj)

cstarsbbsj <- union(cstars22.con,bbsj)

#view(cstarsbbsj)

cstars <- cstarsbbsj
view(cstars)

