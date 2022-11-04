#This script organizes and graphs of State Water Board incident report data
#Modfied from Rosemary Hartman toxins.R, 5/29/2022
#Kristi Arend
#10/25/22

library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(DroughtData)
library(RColorBrewer)
library(deltamapr)
library(ggmap)
library(ggsn)

#set up a color palette for later
HABcol = data.frame(Color = brewer.pal(7, "Dark2"),
                    Genus = c( "Anabaenopsis", "Aphanizomenon","Cylindrospermopsis", "Dolichospermum" ,
                               "Microcystis","Oscillatoria","Planktothrix"))


################################################################################
#incident data from State Water Resources Control Board

incidents = read_excel("data/HABs/Legal Delta HAB incidents 2016-2021.xlsx")

incsf = st_as_sf(incidents, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  rename(Advisory = `Initial Advisory Level`) %>%
  dplyr::filter(Advisory != "No Advisory") %>%
  mutate(Advisory = factor(Advisory, levels = c("Caution",  "Warning","Danger"), labels = c("Caution",  "Warning","Danger")),
         Year = year(`Incident date`))

#add warning levels from samples we have

levels = filter(Alltoxsf, Analyte == "Microcystins") %>%
  mutate(Advisory = case_when(result > 0.8 & result < 6 ~ "Caution",
                              result >= 6 & result < 20 ~ "Warning",
                              result >= 20 ~ "Danger"),
         Advisory2 = case_when(result > 0.8 & result < 6 ~ 1,
                               result >= 6 & result < 20 ~ 2,
                               result >= 20 ~ 3)) %>%
  filter(!is.na(Advisory)) %>%
  group_by(Station) %>%
  mutate(Max = max(Advisory2), Year = 2021) %>%
  dplyr::select(Max, Station, Year) %>%
  distinct()%>%
  mutate(Advisory = factor(Max, levels = c(1,  2,3), labels = c("Caution",  "Warning","Danger")))

#Combine incidents from our toxin levels with the ones reported to the Board
incsf2 = bind_rows(incsf, levels)
inc2021 =
  filter(incsf2, Year == 2021)

#Now plot it. THis is figure 2-18 in the report
ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+

  geom_sf(data = reg3crop, aes(fill = Stratum2), alpha = 0.4) +
  scale_fill_manual(values = reg3$colors, guide = NULL)+

  theme_bw()+ylab("")+xlab("")+

  geom_sf(data = inc2021, aes(color = Advisory), size = 3)+
  scale_color_manual(values = c("yellow",  "red"), labels = c("Caution", "Danger"), name = "Incident Reports\nAdvisory Level")+
  theme_bw()+ scalebar(dist = 10, dist_unit = "km",
                       transform = TRUE, st.dist = .1, x.min = -121.6, x.max = -121.8, y.min = 37.75, y.max = 37.9) +
  north(data = inc2021, symbol = 2) +
  scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.7, 38.4))

ggsave("plots/Incidentsmap.pdf", device = "pdf", width = 6, height = 6)


ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+
  geom_sf(data = incsf2, aes(fill = Advisory), shape = 21, color = "black", size = 3)+
  scale_fill_manual(values = c("yellow",  "orange", "red"), labels = c("Caution", "Warning","Danger"))+
  theme_bw()+
  scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))+
  facet_wrap(~Year)

###############################################################
#Big break
#The data that East Bay Regional Parks sent me was a little different
#Than what the water boards had, but it's the origional source, so it's what I'll use

Bigbreak = read_excel("data/HABs/2015-22 BB HAB Monitoring.xlsx", sheet = "Sheet1")
Bigbreak = filter(Bigbreak, Analyte == "Microcystins") %>%
  mutate(Year = year(Date), DOY = yday(Date))

#This is Figure 2-30
ggplot(Bigbreak, aes(x = DOY, y = Result)) + geom_point()+
  geom_hline(data = filter(health, Analyte == "Microcystins"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_wrap(~Year) + theme_bw() + ylab("Microcystin concentration ug/L")+
  scale_x_continuous(breaks = c(30, 91, 152, 213, 274, 335),
                     labels = c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec"))
ggsave(filename = "plots/BigBreak.tiff", device = "tiff", width = 7, height = 6)

#This is figure 2-17 in the report
ggplot(filter(Bigbreak, Year == 2021), aes(x = DOY, y = Result)) + geom_point()+
  geom_hline(data = filter(health, Analyte == "Microcystins"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_wrap(~Year) + theme_bw() + ylab("Microcystin concentration ug/L")+
  scale_x_continuous(breaks = c( 91, 152, 213, 274),
                     labels = c("Apr", "Jun", "Aug", "Oct"))
ggsave(filename = "plotsBigBreak2021.tiff", device = "tiff", width = 4, height = 4)

#############################3
#add health adivsory levels to toxin data
#This is for the table in the appendix
load("data/HABs/Alltoxindata.RData")

Alltox3 = mutate(Alltoxsf, Advisory = case_when(Analyte == "Microcystins" & result > 0.8 & result < 6 ~ "Caution",
                                                Analyte == "Microcystins"  & result >= 6 & result < 20 ~ "Warning",
                                                Analyte == "Microcystins"  & result >= 20 ~ "Danger",
                                                Analyte == "Microcystins"  & result < 0.8 ~ "No Advisory",
                                                Analyte == "Anatoxins" & result > 20 & result <90 ~ "Warning",
                                                Analyte == "Anatoxins"  & result > 0 & result < 20 ~ "Caution",
                                                Analyte == "Anatoxins"  & result == 0 ~ "No Advisory"))

write.csv(Alltox3, "data/Alltoxindata.csv")

#Export a shapefile so that ICF can use it for the EJ chapter
Alltoxsf = st_as_sf(Alltox3, coords = c("Latitude", "Longitude"), crs = 4326)
st_write(Alltoxsf, "data/HABs/Toxindata.shp", )

############################################
