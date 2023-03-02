#Drought Barrier Report
#Franks Tract Study
#plots

#To do list
#there seem to be NAs present but seems like there shouldn't be; look closer at those

#metadata
#drought barriers where in place during 2015 and 2021
#Kimmerer et al 2019: The Barrier was constructed in May 2015 and the channel was
#closed on 28 May; the Barrier was breached on 1 October 2015, and fully removed by 15 November 2015

#packages
library(tidyverse) # wrangling tabular data and plotting
library(plotrix) #calcuate standard error
library(RColorBrewer) #color palette for plot

#read in the sample data
cleandat <- read_csv("./weeds/data_output/FranksTractManagement_2014-2021_formatted.csv")
#glimpse(cleandat) #looks good

#create subset for Eli
#last <- cleandat %>%
#  filter(date=="2021-10-06")
#write_csv(last,"Sepro_SAV_2021.csv")

#read in the data showing species origin
origin <- read_csv("./weeds/data_input/ucd_rake_samples/FranksTractManagement_SpeciesOrigin.csv")

#combine sample and spp origin dfs
dato <- left_join(cleandat,origin)

#summarize number of samples by year
ssize <- dato %>%
  distinct(date,station) %>%
  group_by(date) %>%
  summarize(samples = n(), .groups = 'drop')
range(ssize$samples) #45 200

#summarize data set to show number of occurrences of each spp by year
#and the mean and SE score for each spp and year
occur <- dato  %>%
  #drop the visual only observations because they don't have rake scores
  #there aren't many of these anyway
  filter(survey_method!="visual") %>%
  group_by(date,species) %>%
  summarize(sp_freq = sum(species_incidence)
            ,avg_score = mean(rake_coverage_ordinal)
            ,se_score = std.error(rake_coverage_ordinal)
            , .groups = 'drop')

#add sample number to spp occurrence df
occurs <- left_join(occur,ssize)

#calculate proportion of occurrences for each spp by year
poccurs <- occurs %>%
  rowwise() %>%
  mutate(sp_freq_prop = sp_freq/samples)

#add spp origin to this data set
pcsn <- left_join(poccurs,origin)

#calculate proportion of occurrences for each spp
#mostly want this to determine how best to order spp in bar graphs
occura <- dato  %>%
  group_by(species) %>%
  summarize(sp_freq = sum(species_incidence), .groups = 'drop') %>%
  arrange(-sp_freq)
#rare: "Nitella_sp", "Potamogeton_pusillus","Potamogeton_zosteriformis", "Potamogeton_nodosus", "Heteranthera_dubia"

#use spp column from this df as list to reorder bars in plot below
ssp_order <- occura$species
pcsn$species <- factor(pcsn$species
                          , levels= c("Potamogeton_richardsonii",  "Ceratophyllum_demersum",    "Egeria_densa"
                          ,"Najas_guadalupensis"       ,"Elodea_canadensis"        , "Stuckenia_filiformis"
                           ,"Stuckenia_pectinata"      , "Potamogeton_crispus"       ,"Potamogeton_foliosus"
                           ,"Myriophyllum_spicatum"   ,  "Nitella_sp"               , "Potamogeton_pusillus"
                           ,"Potamogeton_zosteriformis", "Potamogeton_nodosus"      , "Heteranthera_dubia"
                            ))

#bar plot of proportion of occurrences of each species faceted by year
#using proportion instead of number because sample sizes vary among years
(plot_spp_occur_prop_bar <-ggplot(pcsn
                              , aes(x=species, y= sp_freq_prop, fill=native))+
    geom_bar(stat = "identity") +
    ylab("Proportional Occurrence") + xlab("Species") +
    facet_wrap(~date, nrow=8)
      )
#to do
#turn date into year
#angle spp names so they are readable
#could lump rare taxa into "other" category (eg, any with ten or fewer occurrences)

#line plot of proportion of occurrences of each species by year
(plot_spp_occur_prop_line <-ggplot(pcsn
                              , aes(x=date, y= sp_freq_prop, group=species, color=species))+
    geom_line()+
    geom_point()+
    ylab("Proportional Occurrence") + xlab("Year")+
    facet_wrap(~native)
)
#plot is pretty busy, even with native and non-native spp separated
#could also cut some of the rare taxa which would help a tiny bit

#line plot of proportion of occurrences of each species by year (no rare species)

#filter out 5 taxa with ten or fewer occurrences across entire time series
norare <- pcsn %>%
  filter(species != "Nitella_sp" & species != "Potamogeton_pusillus"& species != "Potamogeton_zosteriformis"& species !=  "Potamogeton_nodosus"& species !=  "Heteranthera_dubia")
#should drop 28 rows but instead drops 40 rows; figure this out; is the related to potential NAs issue?

#create df that categories the ten most common species into three groups
#non-native, native pondweeds, other natives
spp_groups <- data.frame("species" = c("Egeria_densa","Myriophyllum_spicatum", "Potamogeton_crispus"
                                      ,"Najas_guadalupensis","Ceratophyllum_demersum","Elodea_canadensis"
                                      , "Stuckenia_filiformis" ,"Stuckenia_pectinata","Potamogeton_foliosus","Potamogeton_richardsonii"
                                      )
                        ,"group" = c(rep("Non-native Species",3),rep("Other Native Species",3),rep("Native Pondweeds",4))
)

#add groups as column in main df
norareg <- left_join(norare,spp_groups)

#reorder spp for plotting
#norareg$species <- factor(norareg$species
#                       , levels= c("Potamogeton_richardsonii",  "Ceratophyllum_demersum",    "Egeria_densa"
#                                   ,"Najas_guadalupensis"       ,"Elodea_canadensis"        , "Stuckenia_filiformis"
#                                   ,"Stuckenia_pectinata"      , "Potamogeton_crispus"       ,"Potamogeton_foliosus"
#                                   ,"Myriophyllum_spicatum"
#                       ))

#reorder spp groups for facetting
norareg$group = factor(norareg$group, levels=c("Non-native Species","Native Pondweeds", "Other Native Species"))

(plot_spp_occur_prop_line <-ggplot(norareg
                                   , aes(x=date, y= sp_freq_prop, group=species, color=species))+
    #2014 fluridone treatment
    geom_rect(aes(xmin = as.Date("2014-03-01", format = '%Y-%m-%d'),
                   xmax = as.Date("2014-11-01", format = '%Y-%m-%d'),
                   ymin = -Inf,
                   ymax = Inf),
               fill= 'palegoldenrod', color = 'palegoldenrod', alpha =0.9)+
    #2015 drought barrier
    geom_rect(aes(xmin = as.Date("2015-05-28", format = '%Y-%m-%d'),
                  xmax = as.Date("2015-10-01", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'gray75', color = 'gray75', alpha =0.9)+
    #2016 fluridone treatment
    geom_rect(aes(xmin = as.Date("2016-03-01", format = '%Y-%m-%d'),
                  xmax = as.Date("2016-11-01", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'palegoldenrod', color = 'palegoldenrod', alpha =0.9)+
    #2017 fluridone treatment
    geom_rect(aes(xmin = as.Date("2017-03-01", format = '%Y-%m-%d'),
                  xmax = as.Date("2017-11-01", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'palegoldenrod', color = 'palegoldenrod', alpha =0.9)+
    #2018 fluridone treatment
    geom_rect(aes(xmin = as.Date("2018-03-01", format = '%Y-%m-%d'),
                  xmax = as.Date("2018-11-01", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'palegoldenrod', color = 'palegoldenrod', alpha =0.9)+
    #2021 drought barrier
    geom_rect(aes(xmin = as.Date("2021-06-20", format = '%Y-%m-%d'),
                  xmax = as.Date("2022-01-07", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'gray75', color = 'gray75', alpha =0.9)+
    geom_line()+
    geom_point()+
    ylab("Proportional Occurrence") + xlab("Date")+
    facet_wrap(~group,nrow=3)
)
#28 May; the Barrier was breached on 1 October 2015
#2021 installed in May, completed in late June, probably Notched in Nov
#no overlap between fluridone treatment periods and drought barrier presence periods
#so could show both as shaded regions behind time series

#plot of mean score by species and year----------------

#calculate mean score for each spp
#mostly want this to determine how best to order spp in bar graphs
mn_sp <- dato  %>%
  group_by(species) %>%
  summarize(sp_avg_score = mean(rake_coverage_ordinal, na.rm=T), .groups = 'drop') %>%
  arrange(-sp_avg_score)
#looks like same order as occurrences

#use spp column from this df as list to specify order of bars in plot below
ssp_order2 <- mn_sp$species

#plot mean scores of each species by year
(plot_spp_score_avg <-ggplot(norareg
                              , aes(x=species, y= avg_score, fill=native))+
    geom_bar(stat = "identity") +
    ylab("Mean Ordinal Score") + xlab("Species") +
    facet_wrap(~date, nrow=8)
)
#add standard error bars

#line plot of mean ranks of each species by year (no rare species)

#filter out 5 taxa with ten or fewer occurrences across entire time series
norare <- pcsn %>%
  filter(species != "Nitella_sp" & species != "Potamogeton_pusillus"& species != "Potamogeton_zosteriformis"& species !=  "Potamogeton_nodosus"& species !=  "Heteranthera_dubia")

#create distinct shape, color, and line combos for species so they are distinguishable
#scol <- c(
#  "Ceratophyllum_demersum"= "red"
#  ,"Egeria_densa"=            "orange"
#  ,"Elodea_canadensis"=       "yellow"
#  ,"Myriophyllum_spicatum"=    "green"
#  ,"Najas_guadalupensis" =     "blue"
#  ,"Potamogeton_crispus" =    "purple"
#  ,"Potamogeton_foliosus" =    "pink"
#  ,"Potamogeton_richardsonii" ="brown"
#  ,"Stuckenia_filiformis"=    "black"
#  ,"Stuckenia_pectinata" = "tan"
#          )

(plot_spp_avg_score_line <-ggplot(norareg
          , aes(x=date, y= avg_score, group=species, color=species,shape=species, fill=species))+
    #2014 fluridone treatment
    geom_rect(aes(xmin = as.Date("2014-03-01", format = '%Y-%m-%d'),
                  xmax = as.Date("2014-11-01", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'palegoldenrod', color = 'palegoldenrod', alpha =0.9)+
    #2015 drought barrier
    geom_rect(aes(xmin = as.Date("2015-05-28", format = '%Y-%m-%d'),
                  xmax = as.Date("2015-10-01", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'gray75', color = 'gray75', alpha =0.9)+
    #2016 fluridone treatment
    geom_rect(aes(xmin = as.Date("2016-03-01", format = '%Y-%m-%d'),
                  xmax = as.Date("2016-11-01", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'palegoldenrod', color = 'palegoldenrod', alpha =0.9)+
    #2017 fluridone treatment
    geom_rect(aes(xmin = as.Date("2017-03-01", format = '%Y-%m-%d'),
                  xmax = as.Date("2017-11-01", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'palegoldenrod', color = 'palegoldenrod', alpha =0.9)+
    #2018 fluridone treatment
    geom_rect(aes(xmin = as.Date("2018-03-01", format = '%Y-%m-%d'),
                  xmax = as.Date("2018-11-01", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'palegoldenrod', color = 'palegoldenrod', alpha =0.9)+
    #2021 drought barrier
    geom_rect(aes(xmin = as.Date("2021-06-20", format = '%Y-%m-%d'),
                  xmax = as.Date("2022-01-07", format = '%Y-%m-%d'),
                  ymin = -Inf,
                  ymax = Inf),
              fill= 'gray75', color = 'gray75', alpha =0.9)+
    geom_errorbar(aes(ymin=avg_score-se_score, ymax=avg_score+se_score), width=30)+
    geom_line(
      #linetype = rep(c(2, 1), each = 40)
      )+
    geom_point()+
    scale_shape_manual(values=c(25,21,23,22,24,25,21,23,22,24))+
    #scale_color_manual(values=scol, aesthetics = c("colour", "fill"))+
    #scale_color_brewer(palette = "Set3")+ #colors are fairly  distinctive but many too light to show up well
    ylab("Mean Abundance Score") + xlab("Date")+
    facet_wrap(~group,nrow=3)+
    theme_bw()
)
#ggsave(plot=plot_spp_avg_score_line, "./weeds/plots/Sepro_SAV_TimeSeries.png",type ="cairo-png",width=8, height=7,units="in",dpi=300)

#28 May; the Barrier was breached on 1 October 2015
#2021 installed in May, completed in late June, probably Notched in Nov
#no overlap between fluridone treatment periods and drought barrier presence periods
#so could show both as shaded regions behind time series



#could try to make a plot that has scores of all spp within samples summed and then calculate mean scores by year
score_sum <- dato  %>%
  group_by(date,station) %>%
  summarize(tot_score = sum(rake_coverage_ordinal)
            , .groups = 'drop')  %>%
  group_by(date) %>%
  summarize(tot_score_avg = mean(tot_score, na.rm=T)
            ,tot_score_se = std.error(tot_score, na.rm=T)
            , .groups = 'drop')

#plot mean scores by year
(plot_score_avg <-ggplot(score_sum
                             , aes(x=date, y= tot_score_avg))+
    geom_bar(stat = "identity", fill="dark green") +
    geom_errorbar(aes(ymin=tot_score_avg-tot_score_se, ymax=tot_score_avg+tot_score_se)
                  #, width=.1
                  ) +
    ylab("Mean Ordinal Score") + xlab("Year")
)





