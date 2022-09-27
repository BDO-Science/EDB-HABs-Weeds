## Graph WQ Values at Franks Tract (FRK) and NCRO sites
## March 2022

library(tidyverse)
library(lubridate)
library(timetk)

# Set working directory
setwd("./WQ-continuous")
getwd()

# Set theme for ggplot
theme_set(theme_bw())

# Clear workspace
rm(list=ls())

# Import NCRO data from False River near Oakley (FAL) --------------------------
df_FAL <- read_csv("NCRO/HYCSV_FAL_20150101_20211117.csv",
                skip = 3,
                col_names = c("DateTime","Temp","Qual.Temp","SpCond",
                              "Qual.SpCond","DO.Conc","Qual.DO","Chl.a",
                              "Qual.Chl.a","Turb","Qual.Turb","Key"),
                id = "StationCode")

# Import NCRO data from Middle River near Holt (HLT) ---------------------------
df_HLT <- read_csv("NCRO/HYCSV_HLT_20150101_20211208.csv",
                skip = 3,
                col_names = c("DateTime","Temp","Qual.Temp","SpCond",
                              "Qual.SpCond","Chl.a","Qual.Chl.a","Turb",
                              "Qual.Turb","Key"),
                id = "StationCode")

# Import data from Holland Cut near Bethel Island (HOL) ------------------------
df_HOL <- read_csv("NCRO/HYCSV_HOL_20150101_20220119.csv",
                skip = 3,
                col_names = c("DateTime","Temp","Qual.Temp","SpCond",
                              "Qual.SpCond","DO.Conc","Qual.DO","Turb",
                              "Qual.Turb","Key"),
                id = "StationCode")

# Import Data from Old River at Quimby Island (ORQ) ----------------------------
df_ORQ <- read_csv("NCRO/HYCSV_ORQ_20150101_20211208.csv",
                skip = 3,
                col_names = c("DateTime","Temp","Qual.Temp","SpCond",
                              "Qual.SpCond","Turb","Qual.Turb","Key"),
                id = "StationCode")

# Import data from Old River near Franks Tract (OSJ) ---------------------------
df_OSJ <- read_csv("NCRO/HYCSV_OSJ_20150101_20220106.csv",
                skip = 3,
                col_names = c("DateTime","Temp","Qual.Temp","SpCond",
                              "Qual.SpCond","DO.Conc","Qual.DO","Chl.a",
                              "Qual.Chl.a","Turb","Qual.Turb","Key"),
                id = "StationCode")

df_WQ <- bind_rows(df_FAL,df_HLT,df_HOL,df_ORQ,df_OSJ)

print(unique(df_WQ$StationCode))

# Replace file names with actual StationCodes
df_WQ$StationCode <- gsub("NCRO/HYCSV_FAL_20150101_20211117.csv", "FAL", df_WQ$StationCode)
df_WQ$StationCode <- gsub("NCRO/HYCSV_HLT_20150101_20211208.csv", "HLT", df_WQ$StationCode)
df_WQ$StationCode <- gsub("NCRO/HYCSV_HOL_20150101_20220119.csv", "HOL", df_WQ$StationCode)
df_WQ$StationCode <- gsub("NCRO/HYCSV_ORQ_20150101_20211208.csv", "ORQ", df_WQ$StationCode)
df_WQ$StationCode <- gsub("NCRO/HYCSV_OSJ_20150101_20220106.csv", "OSJ", df_WQ$StationCode)

# Check that station IDs are fixed
print(unique(df_WQ$StationCode))

# Get dates to match
df_WQ$DateTime <- mdy_hm(df_WQ$DateTime)

# Remove key column
df_WQ$Key <- NULL

# Remove bad quality NCRO Data -------------------------------------------------

df_WQ_data <- df_WQ %>%
  select(StationCode:Temp,SpCond,DO.Conc,Chl.a,Turb)

df_WQ_qual <- df_WQ %>%
  select(StationCode:DateTime,Qual.Temp,Qual.SpCond,Qual.DO,Qual.Chl.a,Qual.Turb)

df_WQ_qual <- df_WQ_qual %>%
  rename("Temp" = "Qual.Temp") %>%
  rename("SpCond" = "Qual.SpCond") %>%
  rename("DO.Conc" = "Qual.DO") %>%
  rename("Chl.a" = "Qual.Chl.a") %>%
  rename("Turb" = "Qual.Turb")

df_WQ_data <- df_WQ_data %>%
  pivot_longer(
    cols = Temp:Turb,
    names_to = "Analyte",
    values_to = "Conc"
  )

df_WQ_qual <- df_WQ_qual %>%
  pivot_longer(
    cols = c(Temp:Turb),
    names_to = "Analyte",
    values_to = "Qual.Score"
  )

df_WQ <- left_join(df_WQ_data,df_WQ_qual)

rm(df_WQ_data)
rm(df_WQ_qual)

# Quality score table for NCRO data
# 1 = Good quality data
# 2 = Good quality edited data
# 40 = Fair measurement
# 151 = Data missing
# 161 = Below Rating, no flow calculated
# 170 = Unreliable data
# 255 = no data exists

# Evaluate how many samples were below quality thresholds
table(df_WQ$Qual.Score)

# Filter out low-quality or missing data (keep good and fair)

df_WQ <- df_WQ %>% filter(Qual.Score <= 40)

# Remove Qual Score column
df_WQ <- df_WQ %>% select(StationCode:Conc)

# Import NCRO data from MDM ----------------------------------------------------
df_MDM <- read_csv("NCRO/MDM_2015_2022.csv",
                skip = 1,
                col_names = c("DateTime","Chl.a","Flow","SpCond","Temp","Turb"),
                id = "StationCode")

df_MDM$StationCode <- gsub("NCRO/MDM_2015_2022.csv", "MDM", df_MDM$StationCode)

## Pivot
df_MDM <- df_MDM %>%
  pivot_longer(cols = Chl.a:Turb,
               names_to = "Analyte",
               values_to = "Conc")

df_WQ <- bind_rows(df_WQ, df_MDM)

# Remove individual dfs
rm(df_FAL)
rm(df_HLT)
rm(df_HOL)
rm(df_MDM)
rm(df_ORQ)
rm(df_OSJ)

# Import EMP data from Franks Tract (FRK) --------------------------------------
# QA'd historical data from Andrew Tran

# Import EMP data files
files_EMP <- dir(path = "EMP/FRK_data/", pattern = "\\.csv", full.names = T)

df_EMP <- map_dfr(files_EMP, ~read_csv(.x))

# Import EMP data files from 2021
files_EMP_2021 <- dir(path = "EMP/2021/", pattern = "\\.csv", full.names = T)

df_EMP_2021 <- map_dfr(files_EMP_2021, ~read_csv(.x))

# Combine historical and 2021 data
df_EMP <- bind_rows(df_EMP,df_EMP_2021)
rm(df_EMP_2021)

# Remove duplicates
df_EMP <- df_EMP %>% distinct()

# Remove X-flagged data
df_EMP <- df_EMP %>% filter(df_EMP$qaqc_flag_id != "X")

# Remove unit and QA/QC column
df_EMP <- df_EMP %>% select(station:value)

# Rename headers
df_EMP <- df_EMP %>%
  rename("DateTime" = "time") %>%
  rename("Analyte" = "parameter") %>%
  rename("Conc" = "value") %>%
  rename("StationCode" = "station")

# Add column with StationCode for FRK
df_EMP <- df_EMP %>% mutate("StationCode" = "FRK")

# Rename Analytes to match those in NCRO
unique(df_EMP$Analyte)
unique(df_WQ$Analyte)

df_EMP$Analyte <- gsub("DissolvedOxygen","DO.Conc", df_EMP$Analyte)
df_EMP$Analyte <- gsub("Fluorescence","Chl.a", df_EMP$Analyte)
df_EMP$Analyte <- gsub("SpC","SpCond", df_EMP$Analyte)
df_EMP$Analyte <- gsub("Turbidity","Turb", df_EMP$Analyte)
df_EMP$Analyte <- gsub("WaterTemperature","Temp", df_EMP$Analyte)

# Bind all tibbles together into a single data frame
# Use .id to give a new column with original site name

df_WQ <- bind_rows(df_WQ,df_EMP)

# Add column of just the date for grouping
df_WQ <- df_WQ %>% mutate(Date = date(df_WQ$DateTime))

# Remove 2022 data
df_WQ <- df_WQ %>% filter(Date <= "2021-12-31")

# Calculate daily mean
df_WQ_daily <- df_WQ %>%
  group_by(StationCode, Date, Analyte) %>%
  summarise(Daily.Mean = mean(Conc, na.rm = TRUE)) %>%
  ungroup

# Add column of just the year for highlighting yearly data
# Add Julian date for plotting
df_WQ_daily <- df_WQ_daily %>%
  mutate(Year = year(df_WQ_daily$Date)) %>%
  mutate(Julian = yday(df_WQ_daily$Date)) %>%
  mutate(Date = date(df_WQ_daily$Date)) %>%
  mutate(Month = month(df_WQ_daily$Date), label = TRUE)

# Order month 3 in calendar order rather than (default) alphabetical
df_WQ_daily$Month = factor(df_WQ_daily$Month, levels = month.abb)

# Save RData files
save(df_WQ, file = "df_WQ.RData")
save(df_WQ_daily, file = "df_WQ_daily.RData")

# Plot pH at FRK over course of record -----------------------------------------
plot_WQ_mean <- ggplot(df_WQ_daily) +
  geom_line(data = subset(df_WQ_daily, Analyte == "pH"),
             aes(x = Julian, y = Daily.Mean, color = as.factor(Year)),
             size = 1) +
  scale_x_continuous(breaks = c(1,60,121,182,244,305, 366),
                     labels = c("Jan","Mar","May","Jul","Sep","Nov","Jan")) +
  labs(x = NULL,
       y = "pH",
       fill = "Year")

plot_WQ_mean +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = "Set2", name = "Year")

ggsave(path="plots",
       filename = "pH_FRK_by_year.png",
       device = "png",
       scale=1.0,
       units="in",
       height=5,
       width=6.5,
       dpi="print")

# Plot Temp at all stations over course of record faceted by year --------------
analytes <- unique(df_WQ$Analyte)

for (analyte in analytes) {

plot_WQ_mean <- ggplot(df_WQ_daily) +
  geom_line(data = subset(df_WQ_daily, Analyte == analyte),
            aes(x = Julian, y = Daily.Mean, color = as.factor(StationCode)),
            size = 1) +
  scale_x_continuous(breaks = c(1,60,121,182,244,305, 366),
                     labels = c("J","M","M","J","S","N","J")) +
  labs(x = NULL,
       y = paste0(analyte))

plot_WQ_mean +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = "Set1", name = "Year") +
  facet_wrap(Year ~ ., ncol = 4, scale = "free_y")

ggsave(path="plots",
       filename = paste0(analyte,"_by_year_free_y.pdf"),
       device = "pdf",
       scale=1.0,
       units="in",
       height=4,
       width=6.5,
       dpi="print")

}
