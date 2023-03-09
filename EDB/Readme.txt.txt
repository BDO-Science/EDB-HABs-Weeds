This folder contains files used to re-create the 10/14/2021 HABS report figures 2-14 and 2-15 using 2022 data for the 2022 HABS report.

Steps:
1) Use "~/EDBdata/data-raw/hab_satellite.R" to obtain a single year of data (download data manually from https://fhab.sfei.org as described in the code, saving to "~/EDBdata/data-raw/HAB_satellite_data").
2) Use "~/EDBdata/data-raw/Fig2_14_15_2022.R" to combine 2022 with prior (2019-2021) dataset & obtain figures using 2019-2022 data. Note that I added a figure 2-15B because of the HAB in Clifton Court Forebay. I substituted an alternative set of dates b/c sentinel-3 images were compromised on 7/10/2022 by cloud or smoke cover of some sort.
3) Manually save figures to the EDB folder.
