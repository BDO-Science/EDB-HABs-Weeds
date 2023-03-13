library(deltamapr)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(here)
library(readr)
library(lubridate)
library(sf)
library(ggnewscale)

root <- "analysis_2022/" # this is to tell here where to start


# Cyanotoxin ------------------------------------------------------
ct_all <- readRDS(here(root, "data_clean", "ct_all_2022.rds")) %>%
  filter(!Analyte %in% c("Chlorophyll a", "Pheophytin a"),
         !is.na(Stratum2))

ct_all_years <- readRDS(here(root, "data_clean", "ct_years_to_2022.rds")) %>%
  select(-Month) %>%
  mutate(Month = month(Date))

ct_dates <- ct_all %>%
  group_by(Study) %>%
  summarize(first = min(Date),
            last = max(Date))


# WQ -----------------------------------------------------------------
wq_data <- readRDS(here(root, "data_clean", "continuous_wq_2013_2022.rds")) %>%
  mutate(WY = ifelse(as.numeric(Month) >9, as.numeric(Year) + 1, as.numeric(Year)),
         fWY = factor(WY),
         fYear = factor(Year)) %>%
  arrange(Site, Date) %>%
  filter(
         Analyte !="DO.Pct",
         Date < ymd("2022-10-01"),
         !Site %in% c("BLP"))

wq_dates <- wq_data %>%
  filter(!Analyte %in% c("Flow", "Velocity")) %>%
  group_by(Site) %>%
  summarize(first = min(Date),
            last = max(Date))

write_csv(wq_dates, "analysis_2022/data_clean/continuous_dates_decdraft.csv")

unique(wq_data$Site)


# NCRO -----------------------------------------------------------

ncro <- readRDS("analysis_2022/data_raw/NCRO_combined_2022.rds")
str(ncro)
ncro_dates <- ncro %>%
  arrange(station, date) %>%
  group_by(station) %>%
  summarize(first = first(date),
            last = last(date))

FAL <- ncro %>%
  filter(station == "FAL")

write_csv(ncro_dates, "analysis_2022/data_clean/ncro_dates_decdraft.csv")
