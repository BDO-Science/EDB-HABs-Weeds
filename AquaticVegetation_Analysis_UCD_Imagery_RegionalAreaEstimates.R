#Drought Barrier
#Aquatic vegetation
#Hyperspectral imagery coverage data
#Franks Tract, Big Break, and Clifton Court, Delta Common Area

#Script performs analysis and plotting of veg imagery data
#as well as relationships with water quality, flow, and herbicides

#load packages
library(tidyverse) #variety of data science tools
library(lubridate) #format date/time
library(janitor) #clean up column names
library(PerformanceAnalytics) #plotting correlations
library(DEGreport) #adds corr and p to plots
library(ggcorrplot) #plotting correlation matrix
library(ggpubr) #combining plots into panel
library(ggpmisc) #add equations and R^2 to plots

#1 read in data ------------------

#area estimates for the three regions
#note that these three raw data files were used to derive the ucd_imagery_regional_area_estimates.csv file below

#2004-2008, 2014-2015, 2019-2020
cc <- read_csv("./weeds/data_input/ucd_hyperspectral_regional_area_estimates/CliftonCourt_wgs84_area_ha.csv")%>%
  add_column("site" = as.factor("Clifton Court"))

#2004-2008, 2014-2020
ft <- read_csv("./weeds/data_input/ucd_hyperspectral_regional_area_estimates/FranksTract_wgs84_area_ha.csv")%>%
  add_column("site" = as.factor("Franks Tract"))

#2004-2008, 2014-2020
bb <- read_csv("./weeds/data_input/ucd_hyperspectral_regional_area_estimates/BigBreak_wgs84_area_ha.csv")%>%
  add_column("site" = as.factor("Big Break"))

#read in veg area estimates
vegarea <- read_csv("./weeds/data_output/ucd_imagery_regional_area_estimates.csv")

#read in df that matches regions and discrete water quality stations
stm <- read_csv("./weeds/data_output/water_quality_discrete_stations.csv")

#read in Franks Tract continuous water quality data (2015-2021)
wqf <- read_csv("./weeds/data_output/frk_sonde_data_summary.csv")

#read in discrete wq data and delta outflow (2004-2021)
wqd <- read_csv("./weeds/data_output/discrete_wq&outflow_data_summary.csv") %>%
  clean_names() %>%
  rename(ph=p_h)

#read in Franks Tract fluridone application data (2006-2021)
herb <- read_csv("./weeds/data_output/franks_tract_fluridone_2006-2021_summary.csv")

#2 focal sites: stacked bar plots------------------------------

#drop the delta common area for this figure panel
vegarea_sub <- vegarea %>%
  filter(site!="Delta Common Area")

#add symbol for missing data
#use better colors
(foc_ha <- ggplot(vegarea_sub, aes(x=year, y=area_ha,  fill = veg_type))+
   geom_bar(position = "stack", stat = "identity", colour="grey25") +
   ylab("Vegetation Coverage (ha)") + xlab("Year") +
   #scale_fill_discrete(labels=c("FAV","SAV")) +
   scale_fill_manual(name= NULL
                     ,labels=c("Floating","Submersed")
                     ,values=c("#88BA33","#556B2F")
                     ,guide=guide_legend(keyheight=0.5)
   )  +
   #customizes names in legend key, specifies the custom color palette, and sets height of elements in legend
   theme(
     legend.box.spacing=unit(0, units="cm"),
     legend.margin=margin(t=0,r=0,b=2,l=0, unit="pt")) +
   theme_bw()+
   facet_grid(site~.)
)
#ggsave(plot=foc_ha, "./weeds/plots/Hyperspectral_Veg_Area_TimeSeries_FocalSiteArea.png",type ="cairo-png",width=8, scale=0.8, height=7,units="in",dpi=300)


#proportion area based on hyperspectral imagery: stacked bar plots
ggplot(vegarea_sub, aes(x=year, y=area_prop_h,  fill = veg_type))+
  geom_bar(position = "stack", stat = "identity") +
  ylab("Vegetation Coverage Proportion") + xlab("Year") +
  scale_fill_discrete(labels=c("FAV","SAV")) +
  facet_grid(site~.)

#proportion area based on DBW waterway areas: stacked bar plots
ggplot(vegarea_sub, aes(x=year, y=area_prop_w,  fill = veg_type))+
  geom_bar(position = "stack", stat = "identity") +
  ylab("Vegetation Coverage Proportion") + xlab("Year") +
  scale_fill_discrete(labels=c("FAV","SAV")) +
  facet_grid(site~.)


#3 barplot of common area of delta through time--------------------

#only keep delta common area for this figure
vegarea_dca <- vegarea %>%
  filter(site=="Delta Common Area")

#make time series barplot
(veg_perc <- ggplot(data=vegarea_dca,aes(x=year, y=area_prop_h, fill=veg_type)) +
    #specifies the independent and dependent variables as well as groupings
    geom_bar(position="stack", stat="identity", colour="grey25")+
    #specifies that this is a stacked bar plot
    ylab("Water area occupied") + xlab("Year")+
    #x- and y-axis labels
    scale_fill_manual(name= NULL
                      ,labels=c("Floating","Submerged")
                      ,values=c("#88BA33","#556B2F")
                      ,guide=guide_legend(keyheight=0.5)
    )  +
    #customizes names in legend key, specifies the custom color palette, and sets height of elements in legend
    theme(
      legend.box.spacing=unit(0, units="cm"),
      legend.margin=margin(t=0,r=0,b=2,l=0, unit="pt")) +
    theme_bw()
)
#ggsave(plot=veg_perc, "./weeds/plots/Hyperspectral_Veg_Area_TimeSeries_DeltaCommonArea.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)

#4 summary stats--------------

#2004-2006: moderately low SAV
#2007-2008: DBW does intensive fluridone treatments
#2009-2013: no veg imagery
#2014: fairly low SAV
#2105 sudden increase in SAV
#2015-2020: sustained high SAV

erg <- vegarea_sub %>%
  #just FT
  filter(site=="Franks Tract")

bbeff <- vegarea_sub %>%
  filter(site=="Big Break")

cceff <- vegarea_sub %>%
  filter(site=="Clifton Court")
range(cceff$fav_prop)


#5 make correlation plots within sites among land types------------
#NOTE: the within site correlations don't yet include 2021

#Clifton Court: correlations among all land surface types

#need to drop the years with missing data first
ccz <- cc %>%
  filter(sav!=0)

chart.Correlation(ccz[2:10])
#significant correlations, highest to lowest
#water vs SAV (-1.00)
#primrose vs shadow
#npv vs soil
#npv vs sav
#npv vs water
#primrose vs riparian
#riparian vs emergent
#fav vs soil

#Franks Tract: correlations among all land surface types
chart.Correlation(ft[2:10])
#significant correlations, highest to lowest
#water vs SAV (-0.99)
#riparian vs emergent
#pennywort vs primrose
#primrose vs water
#sav vs primrose
#soil vs pennywort
#emergent vs hyacinth
#riparian vs hyacinth

#Big Bend: correlations among all land surface types
chart.Correlation(bb[2:10])

#6 make comparisons among sites within land types-----------

#need to format the data wider
veg_corr <- vegarea_sub %>%
  select(site:month,veg_type,area_prop_h) %>%
  pivot_wider(id_cols = c(site,year,month), names_from = veg_type, values_from = area_prop_h) %>%
  pivot_wider(id_cols = c(year,month), names_from = site, values_from = c(sav,fav)) %>%
  #rename columns
  rename(
    sav_prop_ft = "sav_Franks Tract"
    ,sav_prop_bb = "sav_Big Break"
    ,sav_prop_cc = "sav_Clifton Court"
    ,fav_prop_ft = "fav_Franks Tract"
    ,fav_prop_bb = "fav_Big Break"
    ,fav_prop_cc = "fav_Clifton Court"
  ) %>%
  glimpse()


#SAV: Franks Tract vs Big Break
#the 1:1 line represents how the correlation would look if proportion of SAV
#coverage were the same at both sites across years
fb<-ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_bb))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  xlab("Franks Tract SAV")+
  ylab("Big Break SAV ") +
  coord_cartesian(xlim = c(0,0.7), ylim = c(0, 0.7))

fb_mod <- cor.test(veg_corr$sav_prop_ft,veg_corr$sav_prop_bb)
#correlation is significant
#p-value = 0.009311; cor=0.6881768

#SAV: Franks Tract vs Clifton Court

fc<-ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_cc))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  xlab("Franks Tract SAV")+
  ylab("Clifton Court SAV ")+
  coord_cartesian(xlim = c(0,0.7), ylim = c(0, 0.7))

fc_mod <- cor.test(veg_corr$sav_prop_ft,veg_corr$sav_prop_cc)
#correlation is significant
#p-value = 0.01538; cor=0.7353165

#FAV: Franks Tract vs Big Break
#next make this same plot but with % coverage instead of hectares
#the 1:1 line represents how the correlation would look if proportion of SAV
#coverage were the same at both sites across years
fbf<-ggplot(veg_corr, aes(x=fav_prop_ft, y= fav_prop_bb))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  xlab("Franks Tract FAV")+
  ylab("Big Break FAV ")
#coord_cartesian(xlim = c(0,0.04), ylim = c(0, 0.125))

fbf_mod <- cor.test(veg_corr$fav_prop_ft,veg_corr$fav_prop_bb)
#correlation is significant
#p-value = 4.738e-07; cor=0.9531752

#FAV: Franks Tract vs Clifton Court

fcf<-ggplot(veg_corr, aes(x=fav_prop_ft, y= fav_prop_cc))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  xlab("Franks Tract FAV")+
  ylab("Clifton Court FAV ")
#coord_cartesian(xlim = c(0,0.04), ylim = c(0, 0.10))

fcf_mod <- cor.test(veg_corr$fav_prop_ft,veg_corr$fav_prop_cc)
#correlation is significant
#p-value = 0.04372; cor=0.6457414

sfigure <- ggarrange(fb,fc,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1)
#ggsave(plot=sfigure, "./weeds/plots/Hyperspectral_SAV_AreaCorrPanel_FT_CC_BB.png",type ="cairo-png",width=8, height=4.5,units="in",dpi=300)

ffigure <- ggarrange(fbf,fcf,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1)
#ggsave(plot=ffigure, "./weeds/plots/Hyperspectral_FAV_AreaCorrPanel_FT_CC_BB.png",type ="cairo-png",width=8, height=4.5,units="in",dpi=300)

#7 correlations between SAV acreage and fluridone applications in Franks Tract----------

#join veg acreages and fluridone data sets by year
frankf <- left_join(vegarea,herb) %>%
  filter(site=="Franks Tract" & veg_type=="sav") %>%
  glimpse()

#plot correlation between SAV acreage and fluridone quantities
#can only compare 2014-2021
#could request older treatment data from DBW
#DBW website doesn't have reports for years I need (2004-2008)
ggplot(frankf, aes(area_ha,fl_quantity_kg))+
  geom_smooth(method = "lm")  +
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Area of SAV (ha)")+
  ylab("Quantity of fluridone applied (kg))")
#correlation not significant

#plot correlation between SAV acreage and treatment acreage
(flur<-ggplot(frankf, aes(fl_area_ha,area_ha))+
    geom_smooth(method = "lm")  +
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "pearson")+
    xlab("Area treated with fluridone (ha)")+
    ylab("Area of SAV (ha)")+
    ylim(-250,1600)+
    theme_bw()
)
#ggsave(plot=flur, "./weeds/plots/Hyperspectral_FT_SAV_Area_v_Fluridone.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)
#set y-axis range so it doesn't go below zero
#significant correlation
#hard to know if increase is due to lack of treatment or DBW giving up because of veg intensity

#plot correlation between SAV acreage and treatment acreage
#2008 removed to see if relationship still significant

frankf8 <- frankf %>%
  filter(year!=2008)

ggplot(frankf8, aes(fl_area_ha,area_ha))+
  geom_smooth(method = "lm")  +
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Area treated with fluridone (ha)")+
  ylab("Area of SAV (ha)")
#no longer sign. corr.




#8 correlations among sonde water quality parameters and SAV acreage----------------
#sonde data starts 2015 while EMP discrete likely goes back much farther
#use discrete data instead (see below)

#reduce acreage data set to just year and sav acreage
frankfs <- frankf %>%
  select(year,area_ha)

#make wq data wider
wqfw <-wqf %>%
  #drop SE column
  select(-value_se) %>%
  pivot_wider(id_cols=c(year),names_from = parameter,values_from = value_mean)


#join sav acreage and wq data
frankw <- left_join(frankfs,wqfw) %>%
  #drop years with no WQ data
  filter(year>2014)

#create correlation matrix
corr_matrix <- round(cor(frankw[2:8]),2)

# Computing correlation matrix with p-values
corrp_matrix <- cor_pmat(frankw[2:8])

#grid of correlations
ggcorrplot(corr_matrix, method ="square", type="lower", p.mat = corrp_matrix, lab=T)
#some WQ parameters are strongly correlated but not with SAV

#9 compare veg with discrete water quality and outflow----------

#clifton court only has discrete WQ data for 2016-2020
#and from that period only has imagery for 2019-2020
#so can't do a meaningful analysis

#add region info to WQ data set
wr <- full_join(wqd,stm) %>%
  select(-agency)

#format veg data for combining with wq data
vaw <- vegarea_sub %>%
  select(-area_prop_h:area_prop_w) %>%
  pivot_wider(id_cols = c(site,year,month), names_from = veg_type, values_from = area_ha)

#then add veg data
wrv <-left_join(vaw,wr) %>%
  select(-station) %>%
  arrange(site,year)

#closer look at Big Break veg and conductivity/outflow
bbwr <- wrv %>%
  filter(site=="Big Break") %>%
  select(site,year,month,out_mean,conductivity,sav,fav)

dfta <- wrv %>%
  filter(site == "Franks Tract")

#add herbicide data
dft <- left_join(dfta,herb) %>%
  #but only keep area treated
  select(-c(fl_rate_ppb,fl_quantity_kg)) %>%
  glimpse()
#note we are missing treatment data for 2004-2005

dbb <- wrv %>%
  filter(site == "Big Break")

#create correlation matrices
#needed use="pairwise.complete.obs" because of NAs
f_corr_matrix <- round(cor(dft[4:13],use="pairwise.complete.obs"),3)
b_corr_matrix <- round(cor(dbb[c(4:9,12)],use="pairwise.complete.obs"),3)

# Computing correlation matrix with p-values
f_corrp_matrix <- round(cor_pmat(dft[4:13],use="pairwise.complete.obs"),3)
b_corrp_matrix <- round(cor_pmat(dbb[c(4:9,12)],use="pairwise.complete.obs"),3)

#grid of correlations
ggcorrplot(f_corr_matrix, method ="square", type="lower", p.mat = f_corrp_matrix, lab=T)
ggcorrplot(b_corr_matrix, method ="square", type="lower", p.mat = b_corrp_matrix, lab=T)
#few WQ parameters are strongly correlated to SAV area and none have sign. p-values
#BB SAV and salinity have corr = -0.52

#SAV: Extract the corr and pval from the comparisons between sav and wq parameters
fout <- as.data.frame(cbind("f_corr" = f_corr_matrix[,1],"f_pval" = f_corrp_matrix[,1]))
fout$parameter <- row.names(fout)

bout <- as.data.frame(cbind("b_corr" = b_corr_matrix[,1],"b_pval" = b_corrp_matrix[,1]))
bout$parameter <- row.names(bout)

#combine output df by parameter
aout <- full_join(fout,bout)

#SAV: clean up output df
vstat <- aout %>%
  #no need to have corr of SAV with itself
  #also conductivity and salinity are perfectly correlated with each other
  filter(parameter!="sav" & parameter!="salinity") %>%
  select(parameter,f_corr:f_pval,b_corr:b_pval)
#write_csv(vstat,"./weeds/data_output/sav_area_corr_discrete_wq.csv")

#FAV: Extract the corr and pval from the comparisons between fav and wq parameters
fout2 <- as.data.frame(cbind("f_corr" = f_corr_matrix[,2],"f_pval" = f_corrp_matrix[,2]))
fout2$parameter <- row.names(fout2)


bout2 <- as.data.frame(cbind("b_corr" = b_corr_matrix[,2],"b_pval" = b_corrp_matrix[,2]))
bout2$parameter <- row.names(bout2)

#combine output df by parameter
aout2 <- full_join(fout2,bout2)

#FAV: clean up output df
vstat2 <- aout2 %>%
  #no need to have corr of SAV with itself
  #also conductivity and salinity are perfectly correlated with each other
  filter(parameter!="fav" & parameter!="salinity") %>%
  select(parameter,f_corr:f_pval,b_corr:b_pval)
#write_csv(vstat2,"./weeds/data_output/fav_area_corr_discrete_wq.csv")

#plot correlation between SAV acreage and EC at Big Break
#note that EC and delta outflow are correlated so stick with outflow plot for report
(ec<-ggplot(dbb, aes(conductivity,sav))+
    geom_smooth(method = "lm")  +
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "pearson")+
    xlab("Annual mean conductivity (uS per cm)")+
    ylab("Area of SAV (ha)")+
    theme_bw()
)
#ggsave(plot=ec, "./weeds/plots/Hyperspectral_BB_SAV_Area_v_EC.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)


#plot correlation between SAV acreage and Delta outflow at Big Break
(oflw<-ggplot(dbb, aes(out_mean,sav))+
    geom_smooth(method = "lm")  +
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "pearson")+
    xlab("Annual mean delta outflow (cubic ft per sec)")+
    ylab("Area of SAV (ha)")+
    theme_bw()
)
#ggsave(plot=oflw, "./weeds/plots/Hyperspectral_BB_SAV_Area_v_Outflow.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)

#plot correlation between outflow and EC at Big Break
(outec<-ggplot(dbb, aes(conductivity,out_mean))+
    geom_smooth(method = "lm")  +
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "pearson")+
    xlab("Annual mean conductivity (uS per cm)")+
    ylab("Annual mean delta outflow (cubic ft per sec)")+
    theme_bw()
)
#ggsave(plot=outec, "./weeds/plots/Hyperspectral_BB_Outflow_v_EC.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)


#plot correlation between FAV acreage and temperature at Big Break
(wtemp<-ggplot(dbb, aes(temperature,fav))+
    geom_smooth(method = "lm")  +
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "pearson")+
    xlab("Annual mean water temperature (C)")+
    ylab("Area of FAV (ha)")+
    theme_bw()
)
#ggsave(plot=wtemp, "./weeds/plots/Hyperspectral_BB_FAV_Area_v_Temp.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)

#10 try to build some multiple regression models--------------------
#might not work because of small sample size

#Franks Tract
#originally considered the following predictors
#temperature, EC, secchi, outflow, herbicides
#but EC is significantly correlated with temp and outflow
#so just use temp, secchi, ouflow, herbicides
#note that secchi is tricky because remote sensing's ability to detect SAV is affected
#by turbidity in addition to veg both affecting and being affected by turbidity
#did also look at correlations between veg and DO and pH but these are responses to veg
#rather than predictors

#SAV
ft_sav_mod <- lm(sav ~ fl_area_ha + out_mean + temperature + secchi + fav, data = dft)
summary(ft_sav_mod)
anova(ft_sav_mod)
#only herbicide is significant
#same result we got with the correlations

#FAV
#didn't include EC because correlated with temp and flow
#didn't include secchi because shouldn't affect FAV much
#didn't include pH and DO because those are responses
#didn't include herbicides because I don't have data handy
ft_fav_mod <- lm(fav ~ out_mean + temperature, data = dft)
summary(ft_fav_mod)
anova(ft_fav_mod)
#neither is significant

#Big Break

#SAV
#didn't include DO or pH because responses and also not available from Bay Study station
#didn't include EC because correlated with outflow
#don't have herbicide data handy
bb_sav_mod <- lm(sav ~ out_mean + temperature + secchi + fav, data = dbb)
summary(bb_sav_mod)
anova(bb_sav_mod)
#only outflow significant

#SAV
#didn't include DO or pH because responses and also not available from Bay Study station
#didn't include EC because correlated with outflow
#didn't include secchi because shouldn't affect FAV
#don't have herbicide data handy
bb_fav_mod <- lm(fav ~ out_mean + temperature + secchi, data = dbb)
summary(bb_fav_mod)
anova(bb_fav_mod)
#only temperature significant

#11 correlations among discrete water quality parameters and SAV acreage across site----------------
#trying this because too little replication within individual sites
#mostly data from Big Break and Franks Tract

#start by removing unneeded columns
wrvs2 <- wrv %>%
  select(year,site,sav,temperature:out_mean) %>%
  #drop clifton court because only two years of paired data
  #filter(site!="Clifton Court") %>%
  glimpse()


#plot correlation between SAV acreage and conductance
ggplot(wrvs2, aes(sav,conductivity))+
  geom_smooth(method = "lm")  +
  geom_point() +
  geom_text(aes(label=year,col=site)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Proportion Area of SAV")+
  ylab("Conductance")

#plot correlation between SAV acreage and temperature
ggplot(wrvs2, aes(sav,temperature))+
  geom_smooth(method = "lm")  +
  geom_point() +
  geom_text(aes(label=year,col=site)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Proportion Area of SAV")+
  ylab("Temperature")
#no sign. corr

#plot correlation between SAV acreage and secchi
ggplot(wrvs2, aes(sav,secchi))+
  geom_smooth(method = "lm")  +
  geom_point() +
  geom_text(aes(label=year,col=site)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Proportion Area of SAV")+
  ylab("Secchi")
#significant corr; p=0.03
#is this because sav gets more light when the water is clearer (higher secchi) or
#because it's easier to image sav when water is clear or both?























