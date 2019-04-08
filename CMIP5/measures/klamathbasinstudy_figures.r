
rm(list=ls())

#LOADS PACKAGES
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tools)
library(gridExtra)

#USER FUNCTIONS
pctdiff = function(fut, hist){
	diff = (fut - hist) / hist *100
	return(diff)
}
magdiff = function(fut, hist){
	diff = (fut - hist)
	return(diff)
}
wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=30),collapse=" \n ")
  return(wtext)
}

#READ IN DATA
#wd = 'T:/WaterResources/PlanningOperations/Staff/MElsner/measures/'
wd = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures"
#wd_sav = 'T:/WaterResources/PlanningOperations/Staff/DBROMAN/'
wd_sav = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures/"

file_list = list.files(wd, pattern = '*.txt')[1:25]
file_list = file_list[!grepl("Hist", file_list)]
file_list = file_list[!grepl("DryYearFishTargets", file_list)]

#measures_hist = fread(file.path(wd, file_list[2]))
measures_hist = fread(file.path(wd, "Hist_KRBS_measures_CMIP3_2030.txt"),sep="\t")
#measures_hist = read.table(paste(wd,"/Hist_KRBS_measures_CMIP3_2030.txt",sep=""),sep="\t",skip=2)
measures_hist = measures_hist %>% dplyr::rename(measure = V1, value_hist = V2, Units = V3) %>% dplyr::select(-Units)
measures_dt = NULL


for(i in 1:length(file_list)) {
	info_temp = unlist(strsplit(file_path_sans_ext(file_list[i]), '_'))
	scenario_temp = info_temp[1]
	climset_temp = info_temp[4]
	period_temp = as.numeric(info_temp[5])
	measures_temp = fread(file.path(wd, file_list[i]))
	measures_temp = measures_temp %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp)
	measures_dt = rbind_list(measures_dt, measures_temp)
}

#MERGES FUTURE SCENARIO DATA AND HISTORIC DATA
measures_dt = left_join(measures_dt, measures_hist)
measures_dtmag = left_join(measures_dt, measures_hist)

#CALCULATES PERCENT DIFFERENCE BETWEEN HISTORIC (S0) AND FUTURE PERIODS
measures_dt = measures_dt %>% dplyr::mutate(pctchange = pctdiff(value, value_hist))
#CALCULATES MAGNITUDE DIFFERENCE BETWEEN HISTORIC (S0) AND FUTURE PERIODS
measures_dtmag = measures_dtmag %>% dplyr::mutate(magchange = magdiff(value, value_hist))

#SAVES DATA AS CSV
write.csv(measures_dt, file.path(wd_sav, 'kb_measures.csv'), row.names = F)
write.csv(measures_dtmag, file.path(wd_sav, 'kb_measures_mag.csv'), row.names = F)

#CREATES COLUMN WITH MULTILINE TEXT
measures_dt$measure_mline = llply(measures_dt$measure, wrapit)
measures_dt$measure_mline <- unlist(measures_dt$measure_mline)
measures_dtmag$measure_mline = llply(measures_dtmag$measure, wrapit)
measures_dtmag$measure_mline <- unlist(measures_dtmag$measure_mline)

#FILTERS DATA BY CATEGORY - ECOLOGICAL RESOURCES
measures_eco = measures_dt %>% dplyr::filter(category == 'Ecological Resources')

#CREATES MERGED COLUMNS FOR PLOTTING
measures_eco = measures_eco %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))

#PLOTS BAR PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_eco$test=c(paste(measures_eco$scenario,measures_eco$climset,sep="_"))
ggplot(measures_eco, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_eco,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_eco$pctchange>=0,0,1), size=3) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-80, 20) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_eco.png", width=5, height=6, dpi=300)

measures_eco = measures_dt %>% dplyr::filter(measure == 'Mean Annual Water Delivery to LKNWR')
measures_eco = measures_eco %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
measures_eco$test=c(paste(measures_eco$scenario,measures_eco$climset,sep="_"))
ggplot(measures_eco, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_eco,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_eco$pctchange>=0,0,1), size=3) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-90, 20) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_eco_LKNWR.png", width=5, height=3, dpi=300)

temp<-select(measures_eco,measure,scenario,period,climset,value,value_hist)
temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset)
write.csv(tt, file.path(wd_sav, 'kb_measures_eco.csv'), row.names = F)

#PLOTS BAR PLOT - MEASURES IN ROWS
#ggplot() + geom_bar(data = measures_eco, aes(x = scenario, y = pctchange, fill = climsetperiod), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", guide = F) + facet_wrap(~measure_mline, ncol = 1) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 6, angle = 0))

#FILTERS DATA BY CATEGORY - FLOOD CONTROL
 measures_flood = measures_dt %>% dplyr::filter(category == 'Flood Control')
 measures_flood = measures_flood %>% dplyr::mutate(climsetperiod = paste(climset, period))
 # ggplot() + geom_bar(data = measures_flood, aes(x = scenario, y = pctchange, fill = climset), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0))
#ggplot() + geom_bar(data = measures_flood, aes(x = scenario, y = pctchange, fill = climsetperiod), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_wrap(~measure_mline, ncol = 1) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 6, angle = 0))
measures_flood$test=c(paste(measures_flood$scenario,measures_flood$climset,sep="_"))
ggplot(measures_flood, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_flood,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_flood$pctchange>=0,0,1), size=3) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-55, 130) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_flood.png", width=5, height=6, dpi=300)

measures_flood = measures_dt %>% dplyr::filter(measure == 'Freq of UKL Flood Control Release')
measures_flood = measures_flood %>% dplyr::mutate(climsetperiod = paste(climset, period))
measures_flood$test=c(paste(measures_flood$scenario,measures_flood$climset,sep="_"))
ggplot(measures_flood, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_flood,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_flood$pctchange>=0,0,1), size=3) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-55, 10) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_flood_UKLrel.png", width=5, height=3, dpi=300)

#FILTERS DATA BY CATEGORY - HYDROPOWER
measures_hydro = measures_dt %>% dplyr::filter(category == 'Hydropower')
measures_hydro = measures_hydro %>% dplyr::mutate(climsetperiod = paste(climset, period))
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climset), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0))
measures_hydro$test=c(paste(measures_hydro$scenario,measures_hydro$climset,sep="_"))
ggplot(measures_hydro, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_hydro,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_hydro$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-50, 280) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_hydropower.png", width=5, height=6, dpi=300)
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climsetperiod), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_wrap(~measure_mline, ncol = 1) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 6, angle = 0))

measures_hydro = measures_dt %>% dplyr::filter(measure == 'Mean Annual Hydropower Generated (MW)')
measures_hydro = measures_hydro %>% dplyr::mutate(climsetperiod = paste(climset, period))
measures_hydro$test=c(paste(measures_hydro$scenario,measures_hydro$climset,sep="_"))
ggplot(measures_hydro, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_hydro,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_hydro$pctchange>=0,0,1), size=3) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-25, 10) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_hydropower_GEN.png", width=5, height=3, dpi=300)


temp<-select(measures_hydro,measure,scenario,period,climset,value,value_hist)
temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset)
write.csv(tt, file.path(wd_sav, 'kb_measures_hydro.csv'), row.names = F)

#FILTERS DATA BY CATEGORY - RECREATION FISHING
measures_rec = measures_dt %>% dplyr::filter(category == 'Recreation Fishing')
measures_rec = measures_rec %>% dplyr::mutate(climsetperiod = paste(climset, period))
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climset), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0))
measures_rec$measure_mline <- factor(measures_rec$measure_mline,levels = c("KenoReach Mean Ann Fishing \n Days",
																			"BoyleReach Mean Ann Fishing \n Days",
																			"HellsCornerReach Mean Ann \n Fishing Days",
																			"IGScottReach Mean Ann Fishing \n Days",
																			"ScottSalmonReach Mean Ann \n Fishing Days",
																			"SalmonTrinityReach Mean Ann \n Fishing Days",
																			"TrinityOceanReach Mean Ann \n Fishing Days"))
measures_rec$test=c(paste(measures_rec$scenario,measures_rec$climset,sep="_"))
ggplot(measures_rec, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_rec,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_rec$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-30, 50) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_recreation_fishing.png", width=5, height=6, dpi=300)
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climsetperiod), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_wrap(~measure_mline, ncol = 1) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 6, angle = 0))

temp<-select(measures_rec,measure,scenario,period,climset,value,value_hist)
temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset)
write.csv(tt, file.path(wd_sav, 'kb_measures_rec_fishing.csv'), row.names = F)

#FILTERS DATA BY CATEGORY - RECREATION BOATING
measures_rec = measures_dt %>% dplyr::filter(category == 'Recreation Boating')
measures_rec = measures_rec %>% dplyr::mutate(climsetperiod = paste(climset, period))
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climset), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0))
measures_rec$measure_mline <- factor(measures_rec$measure_mline,levels = c("KenoReach Mean Ann Boating \n Days",
																			"BoyleReach Mean Ann Boating \n Days",
																			"HellsCornerReach Mean Ann \n Boating Days",
																			"IGScottReach Mean Ann Boating \n Days",
																			"ScottSalmonReach Mean Ann \n Boating Days",
																			"SalmonTrinityReach Mean Ann \n Boating Days",
																			"TrinityOceanReach Mean Ann \n Boating Days"))
measures_rec$test=c(paste(measures_rec$scenario,measures_rec$climset,sep="_"))
ggplot(measures_rec, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_rec,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_rec$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_recreation_boating.png", width=5, height=6, dpi=300)
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climsetperiod), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_wrap(~measure_mline, ncol = 1) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 6, angle = 0))

temp<-select(measures_rec,measure,scenario,period,climset,value,value_hist)
temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset)
write.csv(tt, file.path(wd_sav, 'kb_measures_rec_boating.csv'), row.names = F)

#FILTERS DATA BY CATEGORY - WATER DELIVERY
measures_wd = measures_dt %>% dplyr::filter(category == 'Water Delivery')
measures_wd = measures_wd %>% dplyr::mutate(climsetperiod = paste(climset, period))
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climset), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0))
measures_wd$test=c(paste(measures_wd$scenario,measures_wd$climset,sep="_"))
ggplot(measures_wd, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wd,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wd$pctchange>=0,0,1), size=3) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-20, 35) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_water_delivery.png", width=5, height=6, dpi=300)
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climsetperiod), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_wrap(~measure_mline, ncol = 1) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 6, angle = 0))

measures_wd = measures_dt %>% dplyr::filter(measure == 'Mean Project Supply (Apr-Sep)')
measures_wd = measures_wd %>% dplyr::mutate(climsetperiod = paste(climset, period))
measures_wd$test=c(paste(measures_wd$scenario,measures_wd$climset,sep="_"))
ggplot(measures_wd, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wd,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wd$pctchange>=0,0,1), size=3) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-15, 10) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_water_delivery_KPsupply.png", width=5, height=3, dpi=300)

temp<-select(measures_wd,measure,scenario,period,climset,value,value_hist)
temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset)
write.csv(tt, file.path(wd_sav, 'kb_measures_wd.csv'), row.names = F)

#FILTERS DATA BY CATEGORY - WATER QUALITY
measures_wq = measures_dtmag %>% dplyr::filter(category == 'Water Quality', measure == 'Mean Annual MWAT')
measures_wq = measures_wq %>% dplyr::mutate(climsetperiod = paste(climset, period))
#measures_wq$measure <- factor(measures_wq$measure,levels = c("Mean Annual MWAT", "Mean exceedence of MWAT - Poor"))
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climset), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_grid(measure_mline~period) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0))
measures_wq$test=c(paste(measures_wq$scenario,measures_wq$climset,sep="_"))
ggplot(measures_wq, aes(x = scenario, y = magchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wq,aes(x=scenario,y=magchange,label = paste(round(magchange)," (F)",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wq$magchange>=0,0,1), size=3) + scale_fill_brewer(palette = "Paired", name = '', guide = F)  + facet_grid(measure~period) + coord_flip() + theme_bw() + ylim(-5, 15) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_water_quality.png", width=5, height=3, dpi=300)
#ggplot() + geom_bar(data = measures_hydro, aes(x = scenario, y = pctchange, fill = climsetperiod), stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = "Paired", name = '') + facet_wrap(~measure_mline, ncol = 1) + coord_flip() + theme_bw() + ylim(-50, 50) + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), strip.text.y = element_text(size = 6, angle = 0))

temp<-select(measures_wq,measure,scenario,period,climset,value,value_hist)
temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset)
write.csv(tt, file.path(wd_sav, 'kb_measures_wq.csv'), row.names = F)
