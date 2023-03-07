### Make fancier maps for Fluoroprobe Data ###

# Read Packages ---------
library(tidyverse)
library(lubridate) #today()
library(readxl)
library(here)
library(sf)
library(deltamapr)
library(readr)

# Visualization
library(ggmap)
library(ggspatial)
library(deltamapr)
library(viridis)
here()
data_root = "analysis_2022/data_raw/"

# Read data ----------------

fp_oct <- sf::st_read(here(data_root, "DeltaMappingOctober2022Shapefiles", "DeltaMapping_Oct2022_Bluegreen_Clip.shp")) %>%
  mutate(Month = "October")
fp_jul <- sf::st_read(here(data_root, "DeltaMappingJuly2022Shapefiles", "DeltaMapping_July2022_Bluegreen_Clip.shp"))%>%
  mutate(Month = "July")
fp_may <- sf::st_read(here(data_root, "DeltaMappingMay2022Shapefiles", "DeltaMapping_May2022_Bluegreen_Clip.shp")) %>%
  mutate(Month = "May")

fp_data <- rbind(fp_oct, fp_jul, fp_may) %>%
  mutate(Month = factor(Month, c("May", "July", "October")))

# Plots ---------------------

(map_fp <- ggplot()+
   geom_sf(data =fp_data, aes(fill = ContourMin, color = ContourMin), inherit.aes = FALSE)+
   facet_wrap(~Month) +
   annotation_north_arrow(location = "tr", which_north = "true",
                          pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                          style = north_arrow_fancy_orienteering) +
   annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white"))  +
   scale_colour_gradientn(colours = c("#0d0887","#47039FFF",  "#7301A8FF",
                                      "#BD3786FF", "#ED7953FF","#F0F921FF"),
                          values = scales::rescale(c(0, 1, 3, 6, 10, max(fp_data$ContourMin))))+

   scale_fill_gradientn(colours = c("#0d0887","#47039FFF",  "#7301A8FF",
                                      "#BD3786FF", "#ED7953FF","#F0F921FF"),
                          values = scales::rescale(c(0, 1, 3, 6, 10, max(fp_data$ContourMin))))+

   #viridis::scale_fill_viridis(option = "plasma") +
   # scale_fill_gradientn(colours = c("#0D0887FF", "#6A00A8FF", "#B12A90FF",
   # "#E16462FF" ,"#FCA636FF" ,"#F0F921FF"),
   #                        values = scales::rescale(c(min(fp_data$ContourMin),
   #                                                   3, 6, 9, 12, 18, 30, max(fp_data$ContourMin))))+
   labs(fill = "Cyanobacterial \nChlorophyll (µg/L)")+
   guides(colour = "none")+
   theme_classic() +
   theme(axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "top",
         strip.text = element_text(size = 12),
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10)))


### Combine
library(patchwork)

(map_usgs <- map_fp_mayMin + map_fp_julMin + map_fp_octMin + plot_layout(nrow = 1))

## Write figure
png(filename = here("analysis_2022", "figures", "FP_map_extrap_usgs_2022.png"), width = 8, height = 5, units = "in", pointsize = 12, family = "sans", res = 300)
map_fp
dev.off()


png(filename = here("analysis_2022", "figures", "FP_map_extrap_usgs_2022.png"), width = 8, height = 5, units = "in", pointsize = 12, family = "sans", res = 300)
map_usgs
dev.off()
