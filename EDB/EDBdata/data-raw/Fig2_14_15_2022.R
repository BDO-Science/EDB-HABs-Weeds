#Add in 2022 data, and reproduce figures 2-14 and 2-15 for the 2022 report
setwd("C:/Users/lelliott/OneDrive - DOI/Documents/GitHub/EDB-HABs-Weeds")
df_hab_sat_counts_jun_oct <- read.csv("EDB/df_hab_sat_counts_jun_oct.csv")
nrow(df_hab_sat_counts_jun_oct)

df_hab_sat_counts_nov <- read.csv("EDB/hab_sat_ow_delta2019_2021_Nov.csv")
df_hab_sat_counts_nov$VeryHigh <- rep(0)
nrow(df_hab_sat_counts_nov)


hab_sat_ow_delta2022 <- read.csv("EDB/hab_sat_ow_delta2022_Nov.csv")
hab_sat_ow_delta2022 <- rbind(hab_sat_ow_delta2022, df_hab_sat_counts_nov)

lst_hab_sat_clean_2022 <- lst(hab_sat_ow_delta2022, hab_sat_ow_delta) %>%
  map(
    ~ mutate(
      .x,
      YearFct = factor(year(Date)),
      Region = factor(
        Region,
        levels = c(
          "Liberty Island",
          "Franks Tract",
          "Mildred Island",
          "Clifton Court Forebay"
        )
      ),
      DOY = yday(Date)
    ) %>%
      relocate(YearFct, .before = Region)
  )

df_hab_sat_counts_jun_oct <- rbind(df_hab_sat_counts_jun_oct, lst_hab_sat_clean_2022$hab_sat_ow_delta2022)
nrow(df_hab_sat_counts_jun_oct)
df_hab_sat_counts_jun_oct$Date <- as.Date(df_hab_sat_counts_jun_oct$Date, "%Y-%m-%d")

## Area Plot (Figure 2-14)
ci_cat_levels <- c("NonDetect", "Low", "Moderate", "High", "VeryHigh", "InvalidOrMissing")
ci_cat_labels <- c("Not Detected", "Low", "Moderate", "High", "Very High", "Invalid/Missing")


#prepare data area plot
# Prepare HAB satellite data for stacked area plot
df_hab_sat_area_plt <- df_hab_sat_counts_jun_oct %>%
  # Add placeholder rows for data gaps that are greater than 7 days to prevent
  # interpolation of large data gaps in the plot
  group_by(YearFct, Region) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
  arrange(Date) %>%
  mutate(
    na_val = is.na(NonDetect),
    na_val_run_total = sequence(rle(na_val)$lengths)
  ) %>%
  filter(!(na_val == TRUE & na_val_run_total < 8)) %>%
  ungroup() %>%
  select(!starts_with("na_val")) %>%
  # Restructure data to long format
  select(-c(InvalidOrMissing)) %>%
  pivot_longer(
    cols = NonDetect:VeryHigh,
    names_to = "CIcategory",
    values_to = "CIcount"
  ) %>%
  # Apply factor order to the Cyano Index categories
  mutate(
    CIcategory = factor(
      CIcategory,
      levels = ci_cat_levels,
      labels = ci_cat_labels
    )
  ) %>%
  # Convert NA values in the count column to zero to break interpolation of
  # large data gaps in the plot
  replace_na(list(CIcount = 0))


#create area plot, fig.height = 8}
library(ggplot2)
library(scales)
library(viridis)
ci_cat_color_pal <- c("gray", "#56106EFF", "#BB3754FF", "#F98C0AFF", "#FCFFA4FF")
# Create Area Plot
plt_hab_sat_area <-df_hab_sat_area_plt %>%
  ggplot(aes(x = Date, y = CIcount, fill = CIcategory)) +
  geom_area(position = "fill") +
  facet_grid(
    rows = vars(Region),
    cols = vars(YearFct),
    scales = "free_x"
  ) +
  scale_x_date(
    name = "Date",
    breaks = breaks_pretty(6),
    labels = label_date_short(c(NA, "%b", "%d", NA)),
    expand = expansion(mult = 0.03)
  ) +
  scale_y_continuous(
    name = "Percent of valid pixels within each Cyano Index Category",
    labels = percent_format()
  ) +
  scale_fill_manual(
    name = "Cyano Index\nCategory",
    values = ci_cat_color_pal
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

plt_hab_sat_area #FIG 2-14 for 2022

###########################################
## 2022 Maps

#Figure 2-15A: A time series of maps for Franks Tract

### Import and Prepare Satellite Data

#From the area plots, there was a large bloom in Franks Tract during the summer of 2022. Let's take a closer look at the HAB satellite data during this time to see how the bloom appeared spatially. In order to do this, we will need to download the HAB satellite data from the SFEI website for July and August 2021 if that hasn't been done already. Then we'll need to prepare it to be used in the maps.

# download hab satellite data jul aug 2021
# Download HAB satellite data for July and August 2021:
# Set download to TRUE if need to download harmful algal bloom (HAB) satellite data
download <- FALSE

# Download HAB satellite data to local computer if necessary
if (download == TRUE) {
  # Define subfolder directory to store .tif files
  dir_hab_sat <- here("EDB/Spatial_data")

  # Function to download and unzip harmful algal bloom (HAB) satellite data (cyanobacteria abundance)
    # from the https://fhab.sfei.org/ website
  download_hab <- function(hab_yr, hab_month) {
    hab_url <- glue("https://fhab.sfei.org/lib/download.php?request=download&dltype=month&year={hab_yr}&month={hab_month}&product=Mosaic")
    out_path <- file.path(dir_hab_sat, glue("mosaic_{hab_yr}_{hab_month}.zip"))

    curl_download(hab_url, out_path)
    unzip(out_path, exdir = dir_hab_sat)
    Sys.sleep(5)
  }

  # Download data for July and August 2021
  hab_2021 <- c(7, 8)
  for (i in hab_2022) {download_hab(2022, i)}

  # Remove .zip files
  invisible(file.remove(dir(path = dir_hab_sat, pattern = "zip$", full.names = TRUE)))
}


#Let's import all the necessary spatial data. For the satellite imagery, we'll focus on the be beginning, peak, and end of the Cyano bloom in Franks Tract during the summer of 2021.

#import spatial data}
# Create a vector of all file paths for the spatial data data
fp_spt_data <- dir("EDB/EDBdata/data-raw/HAB_satellite_data", full.names = TRUE)
#GOING TO NEED TO IMPORT HAB_SAT_dat_nov2019-2021?

# Import HAB satellite data for the beginning, peak, and end of the Cyano bloom
  # in Franks Tract in 2022:
# Pull out four .tif files representing the beginning, peak, and end of the Cyano bloom
fp_fr_bloom <- str_subset(fp_spt_data, "sentinel-3a.+(0711|0730|0811|0826)")

# Create a nested data frame to prepare the HAB satellite data for maps
df_hab_sat <-
  tibble(
    date_chr = c("July 11, 2022", "July 30, 2022", "Aug 11, 2022", "Aug 26, 2022"),
    fp = fp_fr_bloom,
    strs_prx_obj = map(fp, read_stars, proxy = TRUE)
  ) %>%
  select(-fp)

# Import the polygon shapefile for the four open water regions in the Delta
  # including Franks Tract
sf_ow_delta <- read_sf("EDB/Spatial_data/Franks_Mildr_CCF_LibIsl.shp")


#Next, we'll need to transform the coordinate reference system (CRS) of the open water regions and WW_Delta shapefiles to the CRS of the HAB satellite data so they can all be plotted together on the maps. For the shapefile of open water regions, we'll only keep the polygon for Franks Tract since that's the region we are most interested in.

#prepare sf polygon data}
crs_hab_sat <- st_crs(df_hab_sat$strs_prx_obj[[1]])
sf_franks_32611 <- st_transform(sf_ow_delta[1,], crs = crs_hab_sat) #%>% filter(HNAME == "Franks Tract")
WW_Delta_32611 <- st_transform(WW_Delta, crs = crs_hab_sat)


#We also need to create a bounding box of the Franks Tract shapefile which will be used to crop the satellite data and define the boundaries of the maps. We'll add a 2.5 km buffer to slightly expand the bounding box to ensure no desired data is removed.

#create bbox sf franks tract
bbox_franks <- st_bbox(st_buffer(sf_franks_32611, 2500))


#Lastly, we need to prepare the HAB satellite data for the maps.

#prepare hab satellite data}
df_hab_sat_c <- df_hab_sat %>%
  mutate(
    strs_obj_f =
      # Crop HAB satellite data to bounding box of the Franks Tract shapefile
      map(strs_prx_obj, ~st_crop(.x, bbox_franks) %>%
        # rename attribute to be more descriptive
        setNames("pixel_val") %>%
        # Convert pixel values to Cyano Index categories
        mutate(
          pixel_val = as.numeric(as.character(pixel_val)),
          pixel_val = case_when(
            pixel_val == 0 ~ "NonDetect",
            pixel_val <= 41 ~ "Low",
            pixel_val <= 99 ~ "Moderate",
            pixel_val <= 183 ~ "High",
            pixel_val <= 250 ~ "VeryHigh",
            TRUE ~ NA_character_
          ),
          pixel_val = factor(pixel_val, levels = ci_cat_levels, labels = ci_cat_labels)
        ) %>%
        # Convert to stars object
        st_as_stars()
    )
  ) %>%
  select(-strs_prx_obj)


### Create Maps
#hab satellite maps function
# Function to create maps of HAB satellite data
create_hab_map <- function(strs_obj, map_title, x_txt_lab, y_txt_lab) {
  p <- ggplot() +
    geom_stars(data = strs_obj, na.rm = TRUE) +
    scale_fill_manual(
      name = "Cyano Index\nCategory",
      drop = FALSE,
      na.translate = FALSE,
      values = ci_cat_color_pal
    ) +
    geom_sf(
      data = WW_Delta_32611,
      alpha = 0,
      color = "royalblue3",
      size = 0.3
    ) +
    geom_sf(
      data = sf_franks_32611,
      alpha = 0,
      color = "green3",
      size = 1.1
    ) +
    coord_sf(
      xlim = c(bbox_franks$xmin, bbox_franks$xmax),
      ylim = c(bbox_franks$ymin, bbox_franks$ymax)
    ) +
    ggtitle(map_title) +
    ylab(NULL) +
    scale_x_continuous(
      name = NULL,
      breaks = seq(-121.68, -121.56, by = 0.04)
    ) +
    theme_bw()

  # Only include x-axis tick labels for the bottom most maps
  if (x_txt_lab == FALSE) {
    p <- p + theme(axis.text.x = element_blank())
  }

   # Only include y-axis tick labels for the left most maps
  if (y_txt_lab == FALSE) {
    p <- p + theme(axis.text.y = element_blank())
  }

  return(p)
}


#create hab satellite maps, fig.height = 7}
# Create maps of HAB satellite data for each day
map_hab_sat <- df_hab_sat_c %>%
  mutate(
    x_txt = c(FALSE, FALSE, TRUE, TRUE),
    y_txt = c(TRUE, FALSE, TRUE , FALSE),
    hab_map = pmap(
      list(strs_obj_f, date_chr, x_txt, y_txt),
      create_hab_map
    )
  )

# Combine maps for each day into one
map_hab_sat_c <- wrap_plots(map_hab_sat$hab_map, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

map_hab_sat_c

############################################################################
#Figure 2-15B: A time series of maps for Clifton Court Forebay

# Create a nested data frame to prepare the HAB satellite data for maps
df_hab_sat_cc <-
  tibble(
    date_chr = c("July 11, 2022", "July 30, 2022", "Aug 11, 2022", "Aug 26, 2022"),
    fp = fp_fr_bloom,
    strs_prx_obj = map(fp, read_stars, proxy = TRUE)
  ) %>%
  select(-fp)

#Next, we'll need to transform the coordinate reference system (CRS) of the open water regions and WW_Delta shapefiles to the CRS of the HAB satellite data so they can all be plotted together on the maps. For the shapefile of open water regions, we'll only keep the polygon for Franks Tract since that's the region we are most interested in.

#prepare sf polygon data}
sf_cc_32611 <- st_transform(sf_ow_delta[3,], crs = crs_hab_sat) #%>% filter(HNAME == "Franks Tract")



#We also need to create a bounding box of the Franks Tract shapefile which will be used to crop the satellite data and define the boundaries of the maps. We'll add a 2.5 km buffer to slightly expand the bounding box to ensure no desired data is removed.

#create bbox sf franks tract
bbox_cc <- st_bbox(st_buffer(sf_cc_32611, 2500))


#Lastly, we need to prepare the HAB satellite data for the maps.

#prepare hab satellite data}
df_hab_sat_cc_c <- df_hab_sat_cc %>%
  mutate(
    strs_obj_f =
      # Crop HAB satellite data to bounding box of the Franks Tract shapefile
      map(strs_prx_obj, ~st_crop(.x, bbox_cc) %>%
            # rename attribute to be more descriptive
            setNames("pixel_val") %>%
            # Convert pixel values to Cyano Index categories
            mutate(
              pixel_val = as.numeric(as.character(pixel_val)),
              pixel_val = case_when(
                pixel_val == 0 ~ "NonDetect",
                pixel_val <= 41 ~ "Low",
                pixel_val <= 99 ~ "Moderate",
                pixel_val <= 183 ~ "High",
                pixel_val <= 250 ~ "VeryHigh",
                TRUE ~ NA_character_
              ),
              pixel_val = factor(pixel_val, levels = ci_cat_levels, labels = ci_cat_labels)
            ) %>%
            # Convert to stars object
            st_as_stars()
      )
  ) %>%
  select(-strs_prx_obj)


### Create Maps
#hab satellite maps function
# Function to create maps of HAB satellite data
create_hab_map_cc <- function(strs_obj, map_title, x_txt_lab, y_txt_lab) {
  p <- ggplot() +
    geom_stars(data = strs_obj, na.rm = TRUE) +
    scale_fill_manual(
      name = "Cyano Index\nCategory",
      drop = FALSE,
      na.translate = FALSE,
      values = ci_cat_color_pal
    ) +
    geom_sf(
      data = WW_Delta_32611,
      alpha = 0,
      color = "royalblue3",
      size = 0.3
    ) +
    geom_sf(
      data = sf_cc_32611,
      alpha = 0,
      color = "green3",
      size = 1.1
    ) +
    coord_sf(
      xlim = c(bbox_cc$xmin, bbox_cc$xmax),
      ylim = c(bbox_cc$ymin, bbox_cc$ymax)
    ) +
    ggtitle(map_title) +
    ylab(NULL) +
    scale_x_continuous(
      name = NULL,
      breaks = seq(-121.60, -121.54, by = 0.04)
    ) +
    theme_bw()

  # Only include x-axis tick labels for the bottom most maps
  if (x_txt_lab == FALSE) {
    p <- p + theme(axis.text.x = element_blank())
  }

  # Only include y-axis tick labels for the left most maps
  if (y_txt_lab == FALSE) {
    p <- p + theme(axis.text.y = element_blank())
  }

  return(p)
}


#create hab satellite maps, fig.height = 7}
# Create maps of HAB satellite data for each day
map_hab_sat_cc <- df_hab_sat_cc_c %>%
  mutate(
    x_txt = c(FALSE, FALSE, TRUE, TRUE),
    y_txt = c(TRUE, FALSE, TRUE , FALSE),
    hab_map = pmap(
      list(strs_obj_f, date_chr, x_txt, y_txt),
      create_hab_map_cc
    )
  )

# Combine maps for each day into one
map_hab_sat_cc_c <- wrap_plots(map_hab_sat_cc$hab_map, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

map_hab_sat_cc_c


##LHE Note: I manually exported the figures to the EDB folder.
############################################################################

#EXTRA CODE:






## Export Figures

#Export the area plot and maps of HAB satellite data for the June 2022 version of the HABs/Weeds report. These .jpg files were moved to the EDB SharePoint site.

#export figures, eval = FALSE}
# Export area plot
ggsave(
  here("EDB/CI_category_area_plot.jpg"),
  plot = plt_hab_sat_area,
  width = 6.5,
  height = 7.5,
  units = "in"
)

# Export map of HAB satellite data
ggsave(
  here("EDB/CI_category_map_2021.jpg"),
  plot = map_hab_sat_c,
  width = 6.5,
  height = 6.5,
  units = "in"
)


## Quantitative Analysis for Report

#Calculate percentages of the counts of valid pixels within the Cyano Index categories to be used to describe the results in the report.
#calc ci perc
df_hab_sat_perc <- df_hab_sat_counts_jun_oct %>%
  select(-c(AvgCI, InvalidOrMissing, DOY)) %>%
  mutate(
    TotalCount = rowSums(across(where(is.integer))),
    across(where(is.integer), ~ .x / TotalCount * 100),
    HighSum = High + VeryHigh
  ) %>%
  select(-TotalCount)


#Look at the 2022 bloom in Franks Tract from July through 9/6/2021:

#franks 2022 ci perc}
df_hab_sat_perc %>%
  filter(
    Region == "Franks Tract",
    Date >= "2022-07-01" & Date <= "2022-09-06"
  ) %>%
  print_kable(fixed_header = TRUE)


#Look at the blooms in 2020 and 2021 in Mildred Island:

#mildred ci perc 2020}
# 2020:
df_hab_sat_perc %>%
  filter(
    Region == "Mildred Island",
    Date >= "2020-06-30" & Date <= "2020-09-16"
  ) %>%
  print_kable(fixed_header = TRUE)


```{r mildred ci perc 2021}
# 2021:
df_hab_sat_perc %>%
  filter(
    Region == "Mildred Island",
    Date >= "2021-06-24" & Date <= "2021-08-02"
  ) %>%
  print_kable(fixed_header = TRUE)
```

Look at the blooms in 2020 and 2021 in Clifton Court Forebay:

```{r ccf ci perc 2020}
# 2020:
df_hab_sat_perc %>%
  filter(
    Region == "Clifton Court Forebay",
    Date >= "2020-06-23" & Date <= "2020-09-05"
  ) %>%
  print_kable(fixed_header = TRUE)
```

```{r ccf ci perc 2021}
# 2021:
df_hab_sat_perc %>%
  filter(
    Region == "Clifton Court Forebay",
    Date >= "2021-06-29" & Date <= "2021-08-21"
  ) %>%
  print_kable(fixed_header = TRUE)
```



