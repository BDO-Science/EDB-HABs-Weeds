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
library(DEGreport) #correlations

#read in data ------------------

#Clifton court: 2004-2008, 2014-2015, 2019-2020
cc <- read_csv("./weeds/data_input/ucd_hyperspectral_regional_area_estimates/CliftonCourt_wgs84_area_ha.csv")%>%
  add_column("site" = as.factor("Clifton Court"))

#Franks Tract:2004-2008, 2014-2020
ft <- read_csv("./weeds/data_input/ucd_hyperspectral_regional_area_estimates/FranksTract_wgs84_area_ha.csv")%>%
  add_column("site" = as.factor("Franks Tract"))

#Big Break: 2004-2008, 2014-2020
bb <- read_csv("./weeds/data_input/ucd_hyperspectral_regional_area_estimates/BigBreak_wgs84_area_ha.csv")%>%
  add_column("site" = as.factor("Big Break"))

#read in time series of area data for common area of the delta from SMR repo
#smr <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/data/AquaticVegCoverage_2004-2020_CSTARS_report.csv")
smr <- read_csv("./weeds/data_input/ucd_hyperspectral_regional_area_estimates/AquaticVegCoverage_2004-2020_CSTARS_report.csv")


#2021 data for all regions
new <- read_csv("./weeds/data_input/ucd_hyperspectral_regional_area_estimates/RegionalAreaEstimates_2021_Provisional.csv")

#estimated waterway area for each of the three sites, legal delta, and common area of delta
#use these to calculate proportion of area covered by SAV and FAV
ww <- read_csv("./weeds/data_input/ucd_hyperspectral_regional_area_estimates/waterway_area_ha.csv")

#combine all veg area estimates data into one data set-----------

all <- bind_rows(ft,cc,bb)%>%
  rename(year_month = Year
         ,sav_ha =sav) %>%
  mutate(
    #create year column from year-month
    year=as.integer(str_sub(year_month,1,4))
    #create month column from year-month
    ,month=as.integer(str_sub(year_month,5,6))
    #sum all acreage categories to get total area
    ,total = rowSums(across(soil:shadow))
    #create column that sums the two FAV species
    ,fav_ha = hyacinth + primrose
    #calculate proportion of area that is SAV
    ,sav_prop_h = sav_ha/total
    #calculate proportion of area that is FAV
    ,fav_prop_h = fav_ha/total
  )  %>%
  #reduce to just needed columns and reorder them
  select(
    year
    ,month
    ,site
    ,sav_ha
    ,fav_ha
    ,sav_prop_h
    ,fav_prop_h
  ) %>%
  glimpse()

#create df that has month and year to add month to common delta area df
vmonth <- all %>%
  distinct(year, month) %>%
  glimpse()

#format time series for Delta common area that is just missing 2021
veg_cm <- smr %>%
  #add column for region
  add_column(site=as.factor("Delta Common Area")) %>%
  #drop 2021 because it's just NAs
  filter(year!="2021") %>%
  #just keep the needed columns
  select(year, site, sav_ha,wh_ha,wp_ha,sav_prop,wh_prop,wp_prop) %>%
  rename(sav_prop_h = sav_prop) %>%
  #the original fav_tot_prop column includes pennywort
  #lets make a new one that is just water hyacinth and water primrose
  mutate(year = as.integer(year)
    ,fav_prop_h = wh_prop + wp_prop
         ,fav_ha = wh_ha + wp_ha) %>%
  #drop unneeded columns
  select(-c(wh_ha,wp_ha,wh_prop,wp_prop)) %>%
  glimpse()
  #convert to long format
  #pivot_longer(cols = sav:fav, names_to = "type", values_to = "prop")

#add month column to common delta area df
veg_cm_m <-left_join(veg_cm,vmonth)

#add common delta area df to df of other sites
#glimpse(veg_cm_m)
#glimpse(all)
allsites <- bind_rows(all,veg_cm_m)

#format the 2021 data to combine with rest of data series
new_formatted <- new %>%
  #only keep the column for area estimated in ha
  select(year:site,hectares) %>%
  #for now drop, the whole delta data
  filter(site!="Legal Delta") %>%
  #convert to wide form
  pivot_wider(id_cols=c(year, month, site),names_from = type,values_from = hectares) %>%
  #clean up column names
  clean_names()  %>%
  rename(sav_ha = sav)  %>%
  mutate(
    #make year an integer
    year=as.integer(year)
    #make month an integer
    ,month=as.integer(month)
    #sum all acreage categories to get total area
    ,total = rowSums(across(arundo:w_primrose),na.rm = T)
    #create column that sums the two FAV species
    ,fav_ha = w_hyacinth + w_primrose + empr
    #calculate proportion of area that is SAV
    ,sav_prop_h = sav_ha/total
    #calculate proportion of area that is FAV
    ,fav_prop_h = fav_ha/total
  )   %>%
  #reduce to just needed columns and reorder them
  select(
    year
    ,month
    ,site
    ,sav_ha
    ,fav_ha
    ,sav_prop_h
    ,fav_prop_h
  ) %>%
  glimpse()

#combine 2021 data with rest of time series
alln <- bind_rows(allsites,new_formatted) %>%
  arrange(site,year)

#look at number of years for each imaging month
season <- alln %>%
  distinct(year,month) %>%
  group_by(month) %>%
  summarize(count = n())

#add another way of calculating proportion of area as SAV and FAV-------------
#existing columns sum area of all classes for denominator
#also try using standard waterway area, mostly derived from DBW data

allnw <- left_join(alln,ww) %>%
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
  reduce(full_join, by=c("year","month","site","veg_type")) %>%
  select(site,year,month,veg_type,area_ha,area_prop_h,area_prop_w) %>%
  arrange(site,year,veg_type)

#write_csv(vmetrics,"./weeds/data_output/ucd_imagery_regional_area_estimates.csv")


#create data frame to match WQ stations and sites--------------------------
stm <- as.data.frame(
  cbind(
    agency = c("DFW","DWR","DWR")
    ,station = c("853", "C9", "D19")
    , site = c("Big Break","Clifton Court","Franks Tract")
  )
)
#write_csv(stm,"./weeds/data_output/water_quality_discrete_stations.csv")


