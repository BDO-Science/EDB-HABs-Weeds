#Drought Barrier
#Aquatic vegetation
#Hyperspectral imagery coverage data
#Franks Tract, Big Break, and Clifton Court, Delta Common Area

#Script combines and formats data for all four regions into a single data frame
#and creates small data frame that indicates which discrete water quality station
#matches which of the three small regions

#load packages
library(tidyverse) #variety of data science tools
library(janitor) #clean up column names
library(lubridate) #manipulate dates
library(DEGreport) #correlations

#read in data ------------------

#Clifton court: 2004-2008, 2014-2015, 2019-2020
cc <- read_csv("./weeds_regional_area_estimates/CliftonCourt_wgs84_area_ha.csv")%>%
  add_column("site" = as.factor("Clifton Court"))

#Franks Tract:2004-2008, 2014-2020
ft <- read_csv("./weeds_regional_area_estimates/FranksTract_wgs84_area_ha.csv")%>%
  add_column("site" = as.factor("Franks Tract"))

#Big Break: 2004-2008, 2014-2020
bb <- read_csv("./weeds_regional_area_estimates/BigBreak_wgs84_area_ha.csv")%>%
  add_column("site" = as.factor("Big Break"))

#read in time series of area data for common area of the delta from SMR repo
#smr <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/data/AquaticVegCoverage_2004-2020_CSTARS_report.csv")
#smr <- read_csv("./analysis_2022/hyperspect_veg_2022/AquaticVegCoverage_CSTARS_report.csv")
smr <- read_csv("./analysis_2022/hyperspect_veg_2022/DWR_All_common_area_042023.csv")

#2022 data for all regions
cc22 <- read_csv("./analysis_2022/data_raw/Veg/HyperSAV_cstar_CliftonCourt_calc_2022QAQC.csv")%>%
  add_column("site" = as.factor("Clifton Court"))
ft22 <- read_csv("./analysis_2022/data_raw/Veg/HyperSAV_cstar_FranksTract_calc_2022QAQC.csv")%>%
  add_column("site" = as.factor("Franks Tract"))
bb22 <- read_csv("./analysis_2022/data_raw/Veg/HyperSAV_cstar_BigBreak_calc_2022QAQC.csv")%>%
  add_column("site" = as.factor("Big Break"))


#estimated waterway area for each of the three sites, legal delta, and common area of delta
#use these to calculate proportion of area covered by SAV and FAV
ww <- read_csv("./weeds_regional_area_estimates/waterway_area_ha.csv")

#combine all veg area estimates data into one data set-----------

all <- bind_rows(ft22,cc22,bb22)%>%
  rename(year_month = Year
         ,sav_ha =SAV) %>%
  mutate(
    #create year column from year-month
    year=as.integer(str_sub(year_month,1,4))
    #sum all acreage categories to get total area
    , total = rowSums(across(Soil:Shadow))
    #create column that sums the two FAV species
    ,fav_ha = WHyacinth + WPrimrose + SpongePlant #Added spongeplant / pennywort
    #calculate proportion of area that is SAV
    ,sav_prop_h = sav_ha/total
    #calculate proportion of area that is FAV
    ,fav_prop_h = fav_ha/total
  )  %>%
  #reduce to just needed columns and reorder them
  select(
   year
    ,site
    ,sav_ha
    ,fav_ha
    ,sav_prop_h
    ,fav_prop_h
  ) %>%
  glimpse()

###
# Data don't include month, so this didn't work
###

#create df that has month and year to add month to common delta area df
vmonth <- all %>%
  distinct(year) %>%
  glimpse()

#format time series for Delta common area that is just missing 2022
veg_cm <- smr %>%
  #add column for region
  add_column(site=as.factor("Delta Common Area")) %>%
  #just keep the needed columns
  select(year, site, sav, floating, perc_sav, perc_flt) %>%
  rename(sav_prop_h = perc_sav) %>%
  rename(sav_ha = sav) %>%
  #the original fav_tot_prop column includes pennywort
  #kept pennywort in
  mutate(year = as.integer(year)
    ,fav_prop_h = perc_flt
         ,fav_ha = floating) %>%
  #drop unneeded columns
  select(-c(floating,perc_flt)) %>%
  glimpse()
  #convert to long format
  #pivot_longer(cols = sav:fav, names_to = "type", values_to = "prop")

#add month column to common delta area df
#veg_cm_m <-left_join(veg_cm,vmonth)

#add common delta area df to df of other sites
#glimpse(veg_cm_m)
#glimpse(all)
allsites <- bind_rows(all,veg_cm_m)


#look at number of years for each imaging month
#season <- allsites %>%
#  distinct(year,month) %>%
#  group_by(month) %>%
#  summarize(count = n())

#add another way of calculating proportion of area as SAV and FAV-------------
#existing columns sum area of all classes for denominator
#also try using standard waterway area, mostly derived from DBW data

allnw <- left_join(allsites,ww) %>%
  #create SAV and FAV proportions using waterway area
  mutate(
    sav_prop_w = sav_ha/waterways_ha
    ,fav_prop_w = fav_ha/waterways_ha
  ) %>%
  #dropped unneeded column
  select(-waterways_ha)

#quick plots of correlation between two proportion types
ggplot(allnw,aes(sav_prop_h,sav_prop_w))+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_smooth(method = "lm")  +
  geom_point() +
  geom_cor(method = "pearson")

ggplot(allnw,aes(fav_prop_h,fav_prop_w))+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_smooth(method = "lm")  +
  geom_point() +
  geom_cor(method = "pearson")

#convert data from wide to long---------------

#total veg: convert wide to long
veg_tot <- allnw %>%
  select(year:fav_ha) %>%
  pivot_longer(c(sav_ha:fav_ha), names_to = "type", values_to = "area_ha") %>%
  mutate(veg_type=as_factor(str_sub(type,1,3))) %>%
  select(-type) %>%
  glimpse()

#proportion veg based on imagery: convert wide to long
veg_prop_h <- allnw %>%
  select(year:site,sav_prop_h:fav_prop_h)%>%
  pivot_longer(c(sav_prop_h:fav_prop_h), names_to = "type", values_to = "area_prop_h") %>%
  mutate(veg_type=as_factor(str_sub(type,1,3))) %>%
  select(-type) %>%
  glimpse()

#proportion veg based on DBW waterway area: convert wide to long
veg_prop_w <- allnw %>%
  select(year:site,sav_prop_w:fav_prop_w)%>%
  pivot_longer(c(sav_prop_w:fav_prop_w), names_to = "type", values_to = "area_prop_w") %>%
  mutate(veg_type=as_factor(str_sub(type,1,3))) %>%
  select(-type) %>%
  glimpse()

#join the three types of data back together
list_df = list(veg_tot,veg_prop_h,veg_prop_w)
vmetrics <- list_df %>%
  reduce(full_join, by=c("year","site","veg_type")) %>%
  select(site,year,veg_type,area_ha,area_prop_h,area_prop_w) %>%
  arrange(site,year,veg_type)

write_csv(vmetrics,"./analysis_2022/data_clean/ucd_imagery_regional_area_estimates_2022_QAQC.csv")


#create data frame to match WQ stations and sites--------------------------
#stm <- as.data.frame(
#  cbind(
#    agency = c("DFW","DWR","DWR")
#    ,station = c("853", "C9", "D19")
#    , site = c("Big Break","Clifton Court","Franks Tract")
#  )
#)
#write_csv(stm,"./analysis_2022/hyperspect_veg_2022/water_quality_discrete_stations_2022.csv")


