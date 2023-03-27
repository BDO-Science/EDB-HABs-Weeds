#Purpose: import, clean, wrangle and plot GreenWater Labs epiphytic algal community data
#(scraped from submerged aquatic vegetation by DWR NCRO) and from 1 meter grab samples at D19
#(collected by DWR EMP) from the Central Delta 2022
#Created on 10/19/2022
#Author: Elena Huynh, adapted from Rosemary Hartman and Dave Bosworth

rm(list = ls())

#Load packages
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(ggplot2)
library(lubridate)
library(scales)
library(tibble)
library(viridis)

library(here)
library(readxl)
##### 1. import data -----------------------------------------------------------
#setwd("GWL data")


#GreenWater Data
may <- read_excel(here("analysis_2022/data_raw/DWR_EpiphyticAlgae_Counts_220512.xlsx"),
                  col_names= c("Sample ID", "Site","Date", "Taxa", "Algal Group", "Counting Unit", "Cells_unit", "SpeciesUnits_ml", "SpeciesCells_ml","GroupTotalUnits_ml", "GroupTotalCells_ml","SampleTotalUnits_ml","SampleTotalCells_ml"), skip = 2) %>%
  mutate(Date = mdy(Date))

may24 <- read_excel(here("analysis_2022/data_raw/DWR_EpiphyticAlgae_Counts_220524.xlsx"),
                    col_names=c("Sample ID", "Site","Date", "Taxa", "Algal Group", "Counting Unit", "Cells_unit", "SpeciesUnits_ml", "SpeciesCells_ml","GroupTotalUnits_ml", "GroupTotalCells_ml","SampleTotalUnits_ml","SampleTotalCells_ml"), skip = 2) %>%
  mutate(Date = mdy(Date))

jun <- read_excel(here("analysis_2022/data_raw/DWR_EpiphyticAlgae_Counts_220607.xlsx"),
                  col_names=c("Sample ID", "Site","Date", "Time","Taxa", "Algal Group", "Counting Unit", "Cells_unit", "SpeciesUnits_ml", "SpeciesCells_ml","GroupTotalUnits_ml", "GroupTotalCells_ml","SampleTotalUnits_ml","SampleTotalCells_ml"), skip = 2) %>%
                  mutate(Date = mdy(Date))%>%
  dplyr::select(Time)

jun28 <- read_excel(here("analysis_2022/data_raw/DWR_EpiphyticAlgae_Counts_220628.xlsx"),
                    col_names=c("Sample ID", "Site","Time","Date","Taxa", "Algal Group", "Counting Unit", "Cells_unit", "SpeciesUnits_ml", "SpeciesCells_ml","GroupTotalUnits_ml", "GroupTotalCells_ml","SampleTotalUnits_ml","SampleTotalCells_ml"), skip = 2) %>%
  mutate(Date = lubridate::mdy(Date))%>%
  dplyr::select(-Time)

jul <- read_excel(here("analysis_2022/data_raw/DWR_EpiphyticAlgae_Counts_220713.xlsx"),
                  col_names=c("Sample ID", "Site","Date","Time", "Taxa","Algal Group", "Counting Unit", "Cells_unit", "SpeciesUnits_ml", "SpeciesCells_ml","GroupTotalUnits_ml", "GroupTotalCells_ml","SampleTotalUnits_ml","SampleTotalCells_ml"), skip = 2)%>%
  mutate(Date = mdy(Date))%>%
  dplyr::select(-Time)

jul26 <- read_excel(here("analysis_2022/data_raw/DWR_EpiphyticAlgae_Counts_220726.xlsx"),
                    col_names=c("Sample ID", "Site","Time", "Date", "Taxa","Algal Group", "Counting Unit", "Cells_unit", "SpeciesUnits_ml", "SpeciesCells_ml","GroupTotalUnits_ml", "GroupTotalCells_ml","SampleTotalUnits_ml","SampleTotalCells_ml"), skip = 2) %>%
  mutate(Date = lubridate::mdy(Date))%>%
  dplyr::select(-Time)

aug <- read_excel(here("analysis_2022/data_raw/DWR_EpiphyticAlgae_Counts_220823.xlsx"),
                  col_names=c("Sample ID", "Site","Date","Time", "Taxa","Algal Group", "Counting Unit", "Cells_unit", "SpeciesUnits_ml", "SpeciesCells_ml","GroupTotalUnits_ml", "GroupTotalCells_ml","SampleTotalUnits_ml","SampleTotalCells_ml"), skip = 2)%>%
  mutate(Date = mdy(Date))%>%
  dplyr::select(-Time)

aug31 <- read_excel(here("analysis_2022/data_raw/DWR_EpiphyticAlgae_Counts_220831.xlsx"),
                    col_names=c("Sample ID", "Site","Time", "Date","Taxa","Algal Group", "Counting Unit", "Cells_unit", "SpeciesUnits_ml", "SpeciesCells_ml","GroupTotalUnits_ml", "GroupTotalCells_ml","SampleTotalUnits_ml","SampleTotalCells_ml"), skip = 2) %>%
  mutate(Date = mdy(Date)) %>%
  dplyr::select(-Time)

#Phytoplankton classification data to apply to GWL data
phytoclass <- read_excel("analysis_2022/data_raw/DWR_EpiphyticAlgae_Phyto ClassificationwGreenwater.xlsx") %>%
  filter(!is.na(`Algal Type`)) %>%
  filter(!(is.na(Kingdom) & is.na(Phylum) & is.na(Class) & is.na(Genus)))

#EMP Data
AllEMPphyto <- read.csv("analysis_2022/data_raw/DWR_EpiphyticAlgae_AllEMPphyto.csv")
#EMP_data <- readxl::read_excel(here("analysis_2022/data_raw/EMP_Phytoplankton_Jan-Sept-2022.xlsx"))


##### 2. combine data sets -----------------------------------------------------
phyto <- bind_rows(may, may24, jun, jun28, jul, jul26, aug, aug31)

#### 3. high level plots of algal group ----------------------------------------

#save as new data frame
phyto_broad <- phyto

#parse date
phyto_broad <- phyto %>%

  mutate(Date = gsub("1899", "2022", Date))  %>%
  mutate(Date = ymd(Date))
phyto_broad$Site <- as.factor(phyto_broad$Site) #change Site to factor
phyto_broad <- phyto_broad %>% rename(AlgalGroup = 'Algal Group') #remove space in variable name


###### a. relative abundance of algal groups by site ####
broad_bysite <- ggplot(phyto_broad, aes(Site, SpeciesUnits_ml, fill = AlgalGroup)) +
  geom_col(position= "fill", alpha = 0.7) +
  theme_bw() +
  guides(fill = guide_legend(title="Algal Group")) +
  ylab("Species units \nper mL")
broad_bysite #Bacillariophyta (diatoms) are dominant across central Delta sites


###### b. relative abundance of algal groups by site and time ####

broad_bysitedate <- ggplot(phyto_broad, aes(Date, SpeciesUnits_ml, fill = AlgalGroup)) +
  geom_col(position= "fill", alpha = 0.7) +
  theme_bw() +
  guides(fill = guide_legend(title="Algal Group")) +
  ylab("Species units \nper mL")+
  facet_wrap(vars(Site)) +
  scale_x_date(date_breaks = "month", date_labels = "%m/%d")
broad_bysitedate


##### 4. Finer phyto classification --------------------------------------------

###### a. wrangle to match GWL data to phyto classification data ####

#match column name in classification data frame and GWL data frame
phytoclass_v2 <- phytoclass %>% rename(Taxa = Taxon)

#join phyto and phytoclass_v2 by Taxa column
new_phyto <- left_join(phyto_broad, phytoclass_v2, by = "Taxa")
#there are some observations where Algal Type = NA

#see observations with Algal Type = NA
algal_type_nas <- new_phyto[is.na(new_phyto$'Algal Type'),]
#these observations are duplicates

#remove duplicates, rename Algal Type without space
new_phyto2 <- new_phyto %>%
  group_by('Sample ID', Taxa) %>%
  slice_max(!is.na(`Algal Type`)) %>%
  rename(AlgalType = 'Algal Type') %>%
  filter(!is.na(AlgalType)) %>%
  ungroup()
#convert AlgalType to factor
new_phyto2$AlgalType <- as.factor(new_phyto2$AlgalType)


###### b. plot relative abundances by algal type ####

fine_bysitedate <- ggplot(new_phyto2, aes(Date, SpeciesUnits_ml, fill = AlgalType)) +
  geom_col(position= "fill", alpha = 0.7) +
  theme_bw() +
  guides(fill = guide_legend(title='Algal Type')) +
  ylab("Species units \nper mL")+
  facet_wrap(~Site) +
  scale_x_date(date_breaks = "month", date_labels = "%m/%d")
fine_bysitedate

#plot composition of pennate diatoms
diatom <- new_phyto2 %>% filter(AlgalGroup == "Bacillariophyta")
diatom$AlgalType <- as.factor(diatom$AlgalType)

diatom_bysitedate <- ggplot(diatom, aes(Date, SpeciesUnits_ml, fill = Genus)) +
  geom_col(position = "identity", alpha = 0.7) +
  theme_bw() +
  ylab("Species units \nper mL") +
  facet_wrap(vars(AlgalType), scales = "free") +
  scale_x_date(date_labels = "%m/%d")
diatom_bysitedate



###### c. cyanobacteria only plots ####

#new data frame for cyanobacteria
cyano <- new_phyto2 %>% filter(AlgalGroup == "Cyanobacteria")

cyano_bysitedate <- ggplot(cyano, aes(Date, SpeciesUnits_ml, fill = Genus)) +
  geom_col(position = "fill", alpha = 0.7) +
  theme_bw() +
  ylab("Species units \nper mL") +
  facet_wrap(vars(Site)) +
  scale_x_date(date_labels = "%m/%d")
cyano_bysitedate



##### 5. Prepare GWL data for join with EMP data --------------------------------

#create source ID column, rename columns to match EMP data
GW_data <- new_phyto2 %>%
  mutate(Source = "GWL") %>%
  rename(Taxon = Taxa,
        AlgalType = AlgalType,
        OrganismsPerMl = SpeciesUnits_ml)


#subset EMP data for only Franks Tract site in 2022, create source column, and rename Station column
EMP_Franks <- AllEMPphyto %>% mutate(Source = 'EMP') %>%
  mutate(Date = ymd(Date)) %>%
  filter(Station == "D19"& Year == "2022") %>%
  rename(Site = Station)

#join EMP and GWL data sets
all_phytos <- full_join(EMP_Franks, GW_data, by = c("Site","Date", "Taxon", "Genus", "OrganismsPerMl", "Source", "AlgalType")) %>%
  dplyr::select(c("Site", "Date", "Taxon", "Genus", "OrganismsPerMl", "Source", "AlgalType"))


#create a summary data frame with Genus and Algal Type
taxa <- group_by(all_phytos, Genus, AlgalType) %>%
  summarize(n = n())


#add zeroes to data frame to get average abundances later
#data frame is organized by Genus
phyto_wide <- pivot_wider(all_phytos, id_cols = c(Site, Date, Source), names_from = Genus,
                          values_from = 'OrganismsPerMl', values_fill = 0, values_fn = sum)

#join summarized taxa data (counts of observations of each Genus)
phyto_wzeroes <- pivot_longer(phyto_wide, cols = 'Cocconeis':last_col() ,names_to = 'Genus', values_to = 'OrgsPerMl') %>%
  left_join(taxa)

phyto_wzeroes$AlgalType <- as.factor(phyto_wzeroes$AlgalType)
phyto_wzeroes <- phyto_wzeroes %>%
  mutate(Date = ymd(Date)) %>%
  filter(!is.na(Site))


#viridis codes NAs as white, so create vector manually with viridis hex codes
vir <- c("#440154FF", "#481568FF", "#482677FF", "#453781FF","#3F4788FF","#39558CFF",
         "#32648EFF","#2D718EFF", "#287D8EFF", "#238ADFF", "#1F968BFF", "#20A386FF",
         "#29AF7FFF", "#3CBC75FF", "#56C667FF", "#74D055FF", "#94D840FF", "#B8DE29FF",
         "#DCE318FF", "#FDE725FF")
show_col(viridis_pal()(20)) #use as hex code reference

#partial viridis palette for range of colors that is more easily distinguishable
vir2 <-c("#440154FF", "#453781FF","#3F4788FF",
         "#2D718EFF",
         "#3CBC75FF",  "#94D840FF",
         "#FDE725FF")

##### 6. Plot GreenWater and EMP samples ---------------------------------------

###### a. all taxa --------
#adjust and save geom_col() position argument (default or fill) and facet_wrap() scales arguments (free and fixed)
ggplot(phyto_wzeroes, aes(Date, OrgsPerMl, fill = AlgalType)) +
  geom_col(alpha = 0.7, width = 24, position = "fill") +
  theme_bw() +
  guides(fill = guide_legend(title='Algal Type')) +
  ylab("Organisms \nper mL")+
  facet_wrap(vars(Site), scales = "free") +
  scale_x_date(date_breaks = "month", date_labels = "%b")+
  scale_fill_manual(values = vir2)+
  theme(axis.text.x = element_text(angle = 90))
ggsave("allalgaeprop.tiff", device = "tiff", width = 9, height = 7)




###### b. cyanobacteria only -------
#subset just cyanos
allcyano <- phyto_wzeroes %>% filter(AlgalType == "Cyanobacterium")


#Figure 27
plot_allcyano <- ggplot(allcyano, aes(Date, OrgsPerMl, fill = Genus)) +
  geom_col( width = 24) +
  theme_bw() +
  guides(fill = guide_legend(title='Genus')) +
  ylab("Organisms \nper mL")+
  facet_wrap(~Site) +
  scale_x_date(date_breaks = "month", date_labels = "%b")+
  scale_fill_viridis(option = 'H', discrete=TRUE)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank())
#D19 has magnitudes greater abundance than other sites

png(filename = here("analysis_2022", "figures", "plot_epiphytic_allcyano.png"), width = 8, height = 7, units = "in", pointsize = 12, family = "sans", res = 300)
plot_allcyano
dev.off()
#ggsave("figures/allcyano.tiff", device = "tiff", width = 9, height = 7)

#Figure 28
plot_cyanoprop <- ggplot(allcyano, aes(Date, OrgsPerMl, fill = Genus)) +
  geom_col(position = "fill",   width = 24) +
  theme_bw() +
  guides(fill = guide_legend(title='Genus')) +
  ylab("Organisms \nper mL")+
  facet_wrap(~Site) +
  scale_x_date(date_breaks = "month", date_labels = "%b")+
  scale_fill_viridis(option = 'H', discrete=TRUE)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank())
#D19 has magnitudes greater abundance than other sites

png(filename = here("analysis_2022", "figures", "plot_epiphytic_allcyano_prop.png"), width = 8, height = 7, units = "in", pointsize = 12, family = "sans", res = 300)
plot_cyanoprop
dev.off()

#ggsave("figures/allcyanoprop.tiff", device = "tiff", width = 9, height = 7)


####### c. toxic cyano species ----------
toxsp <- allcyano %>%
  filter(Genus %in% c("Aphanizomenon", "Anabaena",  "Anabaenopsis","Aphanocapsa","Cylindrospermopsis","Dolichospermum",
                      "Lyngbya","Merismopedia", "Microcystis", "Nostoc", "Oscillatoria",
                      "Phormidium","Planktothrix", "Pseudanabaena", "Trichodesmium"))

ggplot(toxsp, aes(Date, OrgsPerMl, fill = Genus)) +
  geom_col(position="fill", alpha = 0.7, width = 24) +
  theme_bw() +
  guides(fill = guide_legend(title='Genus')) +
  ylab("Organisms \nper mL")+
  facet_wrap(vars(Site), scales = "fixed") +
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  scale_fill_viridis( discrete=TRUE, option = "turbo")
#ggsave("toxHABprop.tiff", device = "tiff", width = 9, height = 7)


# Fig 29
plot_toxins <- ggplot(toxsp, aes(Date, OrgsPerMl, fill = Genus)) +
  geom_col( width = 24) +
  theme_bw() +
  guides(fill = guide_legend(title='Genus')) +
  ylab("Organisms \nper mL")+
  facet_wrap(~Site) +
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  scale_fill_viridis( discrete=TRUE, option = "turbo") +
  theme(axis.text.x = element_text(angle =  90))+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank())
#ggsave("figures/toxHABprop.tiff", device = "tiff", width = 9, height = 7)

png(filename = here("analysis_2022", "figures", "plot_epiphytic_toxins_prop.png"), width = 8, height = 7, units = "in", pointsize = 12, family = "sans", res = 300)
plot_toxins
dev.off()

