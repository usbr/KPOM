
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
  wtext <- paste(strwrap(text,width=20),collapse=" \n ")
  return(wtext)
}

#READ IN DATA
wd = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures"
wd_S1 = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures_ET70"
wd_S2 = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures_ET50"
wd_S3 = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures_30KAF"
wd_sav = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures/"

file_list = list.files(wd, pattern = '*.txt')[1:25]
file_list = file_list[!grepl("Hist", file_list)]
file_list = file_list[!grepl("DryYearFishTargets", file_list)]

# measures_hist = fread(file.path(wd, "Hist_KRBS_measures_CMIP3_2030.txt"),sep="\t")
# measures_hist = measures_hist %>% dplyr::rename(measure = V1, value = V2, Units = V3)
# measures_hist = measures_hist %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Historical'))

measures_dt = NULL

for(i in 1:length(file_list)) {
	info_temp = unlist(strsplit(file_path_sans_ext(file_list[i]), '_'))
	scenario_temp = info_temp[1]
	climset_temp = info_temp[4]
	period_temp = as.numeric(info_temp[5])
	
	measures_hist = fread(file.path(wd, "Hist_KRBS_measures_CMIP3_2030.txt"),sep="\t")
	measures_hist = measures_hist %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Historical'))
	
	measures_temp = fread(file.path(wd, file_list[i]))
	measures_temp = measures_temp %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Baseline'))
	measures_temp = fread(file.path(wd, file_list[i]))
	measures_temp = measures_temp %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Baseline'))
	measures_tempS1 = fread(file.path(wd_S1, file_list[i]))
	measures_tempS1 = measures_tempS1 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Reduce\nET 30%'))
	measures_tempS2 = fread(file.path(wd_S2, file_list[i]))
	measures_tempS2 = measures_tempS2 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Reduce\nET 50%'))
	measures_tempS3 = fread(file.path(wd_S3, file_list[i]))
	measures_tempS3 = measures_tempS3 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Add\n30KAF'))
	measures_dt = rbind_list(measures_dt, measures_hist, measures_temp, measures_tempS1, measures_tempS2, measures_tempS3)
}

#MERGES FUTURE SCENARIO DATA AND HISTORIC DATA
# measures_dt = left_join(measures_dt, measures_hist)
# measures_dtmag = left_join(measures_dt, measures_hist)

#CALCULATES PERCENT DIFFERENCE BETWEEN HISTORIC (S0) AND FUTURE PERIODS
# measures_dt = measures_dt %>% dplyr::mutate(pctchange = pctdiff(value, value_hist))
#CALCULATES MAGNITUDE DIFFERENCE BETWEEN HISTORIC (S0) AND FUTURE PERIODS
# measures_dtmag = measures_dtmag %>% dplyr::mutate(magchange = magdiff(value, value_hist))

#SAVES DATA AS CSV
write.csv(measures_dt, file.path(wd_sav, 'kb_measures_STRATEGIES.csv'), row.names = F)
# write.csv(measures_dtmag, file.path(wd_sav, 'kb_measures_mag_STRATEGIES.csv'), row.names = F)

#CREATES COLUMN WITH MULTILINE TEXT
measures_dt$measure_mline = llply(measures_dt$measure, wrapit)
measures_dt$measure_mline <- unlist(measures_dt$measure_mline)
# measures_dtmag$measure_mline = llply(measures_dtmag$measure, wrapit)
# measures_dtmag$measure_mline <- unlist(measures_dtmag$measure_mline)

#==========================
# ECOLOGICAL RESOURCES
#==========================
#FILTERS DATA BY CATEGORY - ECOLOGICAL RESOURCES
measures_eco = measures_dt %>% dplyr::filter(category == 'Ecological Resources')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_eco = measures_eco %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS BAR PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_eco$test=c(paste(measures_eco$scenario,measures_eco$climset,sep="_"))
#WRITE TO FILE BEFORE SUBSETTING TABLE FOR PLOTTING
temp<-select(measures_eco,measure,scenario,period,climset,strategy,value)
temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
# write.csv(tt, file.path(wd_sav, 'kb_measures_eco_STRATEGIES.csv'), row.names = F)

#PLOTS FOR 2030s
# measures_eco2030 = measures_eco %>% dplyr::filter(period == 2030)
# measures_eco2030$strategy <- factor(measures_eco2030$strategy,levels = c("Historical", "Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_eco2030, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_eco2030,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_eco2030$value>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 100) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_eco_STRATEGIES_2030.png", width=6, height=6, dpi=300)

measures_eco2030b = measures_eco %>% dplyr::filter(period == 2030)
measures_eco2030b = measures_eco2030b %>% dplyr::filter(measure == 'Mean Annual Water Delivery to LKNWR')
measures_eco2030b = measures_eco2030b %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
measures_eco2030b$test=c(paste(measures_eco2030b$scenario,measures_eco2030b$climset,sep="_"))
measures_eco2030b$strategy <- factor(measures_eco2030b$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
ggplot(measures_eco2030b, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_eco2030b,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_eco2030b$value>=0,0,1), size=2) + geom_hline(yintercept = 95.4, colour="#990000", size=1.0) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 100) + theme(axis.text.x=element_text(angle=90), axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_eco_LKNWRgaps_STRATEGIES_2030.png", width=6, height=1.8, dpi=300)

#PLOT FOR 2070s
# measures_eco2070 = measures_eco %>% dplyr::filter(period == 2070)
# measures_eco2070$strategy <- factor(measures_eco2070$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_eco2070, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_eco2070,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_eco2070$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-90, 20) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_eco_STRATEGIES_2070.png", width=6, height=6, dpi=300)

measures_eco2070b = measures_eco %>% dplyr::filter(period == 2070)
measures_eco2070b = measures_eco2070b %>% dplyr::filter(measure == 'Mean Annual Water Delivery to LKNWR')
measures_eco2070b = measures_eco2070b %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
measures_eco2070b$test=c(paste(measures_eco2070b$scenario,measures_eco2070b$climset,sep="_"))
measures_eco2070b$strategy <- factor(measures_eco2070b$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
ggplot(measures_eco2070b, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_eco2070b,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_eco2070b$value>=0,0,1), size=2) + geom_hline(yintercept = 95.4, colour="#990000", size=1.0) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 100) + theme(axis.text.x=element_text(angle=90), axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_eco_LKNWRgaps_STRATEGIES_2070.png", width=6, height=1.8, dpi=300)

#==========================
# FLOOD CONTROL
#==========================

#FILTERS DATA BY CATEGORY - FLOOD CONTROL
measures_flood = measures_dt %>% dplyr::filter(category == 'Flood Control')
measures_flood = measures_flood %>% dplyr::mutate(climsetperiod = paste(climset, period))

#PLOTS BAR PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_flood$test=c(paste(measures_flood$scenario,measures_flood$climset,sep="_"))

#WRITE TO FILE BEFORE SUBSETTING TABLE FOR PLOTTING
# temp<-select(measures_flood,measure,scenario,period,climset,strategy,value,value_hist)
# temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
# tt <- arrange(temp,measure,period,scenario,climset,strategy)
# write.csv(tt, file.path(wd_sav, 'kb_measures_flood_STRATEGIES.csv'), row.names = F)

#PLOTS FOR 2030s
# measures_flood2030 = measures_flood %>% dplyr::filter(period == 2030)
# measures_flood2030$strategy <- factor(measures_flood2030$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_flood2030, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_flood2030,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_flood2030$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-50, 130) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_flood_STRATEGIES_2030.png", width=6, height=4, dpi=300)

measures_flood2030b = measures_flood %>% dplyr::filter(period == 2030)
measures_flood2030b = measures_flood2030b %>% dplyr::filter(measure == 'Freq of UKL Flood Control Release')
measures_flood2030b = measures_flood2030b %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
measures_flood2030b$test=c(paste(measures_flood2030b$scenario,measures_flood2030b$climset,sep="_"))
measures_flood2030b$strategy <- factor(measures_flood2030b$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
ggplot(measures_flood2030b, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_flood2030b,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_flood2030b$value>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 50) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_flood_UKLrel_gaps_STRATEGIES_2030.png", width=6, height=1.8, dpi=300)

#PLOTS FOR 2070s
# measures_flood2070 = measures_flood %>% dplyr::filter(period == 2070)
# measures_flood2070$strategy <- factor(measures_flood2070$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_flood2070, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_flood2070,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_flood2070$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-50, 130) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_flood_STRATEGIES_2070.png", width=6, height=4, dpi=300)

measures_flood2070b = measures_flood %>% dplyr::filter(period == 2070)
measures_flood2070b = measures_flood2070b %>% dplyr::filter(measure == 'Freq of UKL Flood Control Release')
measures_flood2070b = measures_flood2070b %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
measures_flood2070b$test=c(paste(measures_flood2070b$scenario,measures_flood2070b$climset,sep="_"))
measures_flood2070b$strategy <- factor(measures_flood2070b$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
ggplot(measures_flood2070b, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_flood2070b,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_flood2070b$value>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 50) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_flood_UKLrel_gaps_STRATEGIES_2070.png", width=6, height=1.8, dpi=300)

#==========================
# HYDROPOWER RESOURCES
#==========================

#FILTERS DATA BY CATEGORY - HYDROPOWER
measures_hydro = measures_dt %>% dplyr::filter(category == 'Hydropower')
measures_hydro = measures_hydro %>% dplyr::mutate(climsetperiod = paste(climset, period))
measures_hydro$test=c(paste(measures_hydro$scenario,measures_hydro$climset,sep="_"))

#WRITE TO FILE BEFORE SUBSETTING TABLE FOR PLOTTING
# temp<-select(measures_hydro,measure,scenario,period,climset,strategy,value,value_hist)
# temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
# tt <- arrange(temp,measure,period,scenario,climset,strategy)
# write.csv(tt, file.path(wd_sav, 'kb_measures_hydro_STRATEGIES.csv'), row.names = F)

#PLOTS FOR 2030s
# measures_hydro2030 = measures_hydro %>% dplyr::filter(period == 2030)
# measures_hydro2030$strategy <- factor(measures_hydro2030$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_hydro2030, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_hydro2030,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_hydro2030$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-100, 280) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_hydro_STRATEGIES_2030.png", width=6, height=7.5, dpi=300)

measures_hydro2030b = measures_hydro %>% dplyr::filter(period == 2030)
measures_hydro2030b = measures_hydro2030b %>% dplyr::filter(measure == 'Mean Annual Hydropower Generated (MW)')
measures_hydro2030b = measures_hydro2030b %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
measures_hydro2030b$test=c(paste(measures_hydro2030b$scenario,measures_hydro2030b$climset,sep="_"))
measures_hydro2030b$strategy <- factor(measures_hydro2030b$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
ggplot(measures_hydro2030b, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_hydro2030b,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_hydro2030b$value>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 50000) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_hydropower_GEN_gaps_STRATEGIES_2030.png", width=6, height=1.8, dpi=300)

#PLOTS FOR 2070s
# measures_hydro2070 = measures_hydro %>% dplyr::filter(period == 2070)
# measures_hydro2070$strategy <- factor(measures_hydro2070$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_hydro2070, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_hydro2070,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_hydro2070$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-100, 280) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_hydro_STRATEGIES_2070.png", width=6, height=7.5, dpi=300)

measures_hydro2070b = measures_hydro %>% dplyr::filter(period == 2070)
measures_hydro2070b = measures_hydro2070b %>% dplyr::filter(measure == 'Mean Annual Hydropower Generated (MW)')
measures_hydro2070b = measures_hydro2070b %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
measures_hydro2070b$test=c(paste(measures_hydro2070b$scenario,measures_hydro2070b$climset,sep="_"))
measures_hydro2070b$strategy <- factor(measures_hydro2070b$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
ggplot(measures_hydro2070b, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_hydro2070b,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_hydro2070b$value>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 50000) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_hydropower_GEN_gaps_STRATEGIES_2070.png", width=6, height=1.8, dpi=300)

#==========================
# RECREATION FISHING
#==========================

#FILTERS DATA BY CATEGORY - RECREATION FISHING
# measures_rec = measures_dt %>% dplyr::filter(category == 'Recreation Fishing')
# measures_rec = measures_rec %>% dplyr::mutate(climsetperiod = paste(climset, period))
# measures_rec$measure_mline <- factor(measures_rec$measure_mline,levels = c("KenoReach Mean Ann \n Fishing Days",
																			# "BoyleReach Mean Ann \n Fishing Days",
																			# "HellsCornerReach \n Mean Ann Fishing \n Days",
																			# "IGScottReach Mean \n Ann Fishing Days",
																			# "ScottSalmonReach \n Mean Ann Fishing \n Days",
																			# "SalmonTrinityReach \n Mean Ann Fishing \n Days",
																			# "TrinityOceanReach \n Mean Ann Fishing \n Days"))
# measures_rec$test=c(paste(measures_rec$scenario,measures_rec$climset,sep="_"))

#WRITE TO FILE BEFORE SUBSETTING TABLE FOR PLOTTING
# temp<-select(measures_rec,measure,scenario,period,climset,strategy,value,value_hist)
# temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
# tt <- arrange(temp,measure,period,scenario,climset,strategy)
# write.csv(tt, file.path(wd_sav, 'kb_measures_rec_fishing_STRATEGIES.csv'), row.names = F)

#PLOTS FOR 2030s
# measures_rec2030 = measures_rec %>% dplyr::filter(period == 2030)
# measures_rec2030$strategy <- factor(measures_rec2030$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_rec2030, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_rec2030,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_rec2030$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-40, 50) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_rec_fishing_STRATEGIES_2030.png", width=6, height=7.5, dpi=300)

#PLOTS FOR 2070s
# measures_rec2070 = measures_rec %>% dplyr::filter(period == 2070)
# measures_rec2070$strategy <- factor(measures_rec2070$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_rec2070, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_rec2070,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_rec2070$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-40, 50) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_rec_fishing_STRATEGIES_2070.png", width=6, height=7.5, dpi=300)

#==========================
# RECREATION BOATING
#==========================

#FILTERS DATA BY CATEGORY - RECREATION BOATING
# measures_rec = measures_dt %>% dplyr::filter(category == 'Recreation Boating')
# measures_rec = measures_rec %>% dplyr::mutate(climsetperiod = paste(climset, period))
# measures_rec$measure_mline <- factor(measures_rec$measure_mline,levels = c("KenoReach Mean Ann \n Boating Days",
																			# "BoyleReach Mean Ann \n Boating Days",
																			# "HellsCornerReach \n Mean Ann Boating \n Days",
																			# "IGScottReach Mean \n Ann Boating Days",
																			# "ScottSalmonReach \n Mean Ann Boating \n Days",
																			# "SalmonTrinityReach \n Mean Ann Boating \n Days",
																			# "TrinityOceanReach \n Mean Ann Boating \n Days"))
# measures_rec$test=c(paste(measures_rec$scenario,measures_rec$climset,sep="_"))

#WRITE TO FILE BEFORE SUBSETTING TABLE FOR PLOTTING
# temp<-select(measures_rec,measure,scenario,period,climset,strategy,value,value_hist)
# temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
# tt <- arrange(temp,measure,period,scenario,climset,strategy)
# write.csv(tt, file.path(wd_sav, 'kb_measures_rec_Boating_STRATEGIES.csv'), row.names = F)

#PLOTS FOR 2030s
# measures_rec2030 = measures_rec %>% dplyr::filter(period == 2030)
# measures_rec2030$strategy <- factor(measures_rec2030$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_rec2030, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_rec2030,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_rec2030$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-40, 25) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_rec_Boating_STRATEGIES_2030.png", width=6, height=7.5, dpi=300)

#PLOTS FOR 2070s
# measures_rec2070 = measures_rec %>% dplyr::filter(period == 2070)
# measures_rec2070$strategy <- factor(measures_rec2070$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_rec2070, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_rec2070,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_rec2070$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-40, 25) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_rec_Boating_STRATEGIES_2070.png", width=6, height=7.5, dpi=300)

#==========================
# WATER DELIVERY
#==========================

#FILTERS DATA BY CATEGORY - WATER DELIVERY
measures_wd = measures_dt %>% dplyr::filter(category == 'Water Delivery')
measures_wd = measures_wd %>% dplyr::mutate(climsetperiod = paste(climset, period))
measures_wd$test=c(paste(measures_wd$scenario,measures_wd$climset,sep="_"))

#WRITE TO FILE BEFORE SUBSETTING TABLE FOR PLOTTING
# temp<-select(measures_wd,measure,scenario,period,climset,strategy,value,value_hist)
# temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
# tt <- arrange(temp,measure,period,scenario,climset,strategy)
# write.csv(tt, file.path(wd_sav, 'kb_measures_wd_STRATEGIES.csv'), row.names = F)

#PLOTS FOR 2030s
# measures_wd2030 = measures_wd %>% dplyr::filter(period == 2030)
# measures_wd2030$strategy <- factor(measures_wd2030$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_wd2030, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wd2030,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wd2030$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-25, 35) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_wd_STRATEGIES_2030.png", width=6, height=6, dpi=300)

measures_wd2030b = measures_wd %>% dplyr::filter(period == 2030)
measures_wd2030b$strategy <- factor(measures_wd2030b$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
measures_wd2030b = measures_wd2030b %>% dplyr::filter(measure == 'Mean Project Supply (Apr-Sep)')
measures_wd2030b = measures_wd2030b %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
measures_wd2030b$test=c(paste(measures_wd2030b$scenario,measures_wd2030b$climset,sep="_"))
ggplot(measures_wd2030b, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wd2030b,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wd2030b$value>=0,-0.9,1), size=2) + geom_hline(yintercept = 390, colour="#990000", size=1.0) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 600) + theme(axis.text.x=element_text(angle=90), axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_wd_KPsupply_gaps_STRATEGIES_2030.png", width=6, height=1.8, dpi=300)

#PLOTS FOR 2070s
# measures_wd2070 = measures_wd %>% dplyr::filter(period == 2070)
# measures_wd2070$strategy <- factor(measures_wd2070$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
# ggplot(measures_wd2070, aes(x = scenario, y = pctchange, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wd2070,aes(x=scenario,y=pctchange,label = paste(round(pctchange),"%",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wd2070$pctchange>=0,0,1), size=2) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(-25, 35) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
# ggsave("measures_wd_STRATEGIES_2070.png", width=6, height=6, dpi=300)

measures_wd2070b = measures_wd %>% dplyr::filter(period == 2070)
measures_wd2070b$strategy <- factor(measures_wd2070b$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
measures_wd2070b = measures_wd2070b %>% dplyr::filter(measure == 'Mean Project Supply (Apr-Sep)')
measures_wd2070b = measures_wd2070b %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
measures_wd2070b$test=c(paste(measures_wd2070b$scenario,measures_wd2070b$climset,sep="_"))
ggplot(measures_wd2070b, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wd2070b,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wd2070b$value>=0,-0.9,1), size=2) + geom_hline(yintercept = 390, colour="#990000", size=1.0) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 600) + theme(axis.text.x=element_text(angle=90), axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_wd_KPsupply_gaps_STRATEGIES_2070.png", width=6, height=1.8, dpi=300)

#==========================
# WATER QUALITY
#==========================

#FILTERS DATA BY CATEGORY - WATER QUALITY
measures_wq = measures_dt %>% dplyr::filter(category == 'Water Quality', measure == 'Mean Annual MWAT')
measures_wq = measures_wq %>% dplyr::mutate(climsetperiod = paste(climset, period))
measures_wq$test=c(paste(measures_wq$scenario,measures_wq$climset,sep="_"))

#WRITE TO FILE BEFORE SUBSETTING TABLE FOR PLOTTING
# temp<-select(measures_wq,measure,scenario,period,climset,strategy,value,value_hist)
# temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
# tt <- arrange(temp,measure,period,scenario,climset,strategy)
# write.csv(tt, file.path(wd_sav, 'kb_measures_wq_STRATEGIES.csv'), row.names = F)

#PLOTS FOR 2030s
measures_wq2030 = measures_wq %>% dplyr::filter(period == 2030)
measures_wq2030$strategy <- factor(measures_wq2030$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
ggplot(measures_wq2030, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wq2030,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wq2030$value>=0,0,1), size=2) + geom_hline(yintercept = 63.68, colour="#990000", size=1.0) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 100) + theme(axis.text.x=element_text(angle=90), axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_wq_STRATEGIES_gaps_2030.png", width=6, height=1.8, dpi=300)

#PLOTS FOR 2070s
measures_wq2070 = measures_wq %>% dplyr::filter(period == 2070)
measures_wq2070$strategy <- factor(measures_wq2070$strategy,levels = c("Historical", "Baseline", "Reduce\nET 30%", "Reduce\nET 50%", "Add\n30KAF"))
ggplot(measures_wq2070, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wq2070,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wq2070$value>=0,0,1), size=2) + geom_hline(yintercept = 63.68, colour="#990000", size=1.0) + scale_fill_brewer(palette = "Paired", name = '', guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 100) + theme(axis.text.x=element_text(angle=90), axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_wq_STRATEGIES_gaps_2070.png", width=6, height=1.8, dpi=300)

