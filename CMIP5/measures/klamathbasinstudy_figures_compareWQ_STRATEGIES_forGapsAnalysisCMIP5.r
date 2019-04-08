
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
  wtext <- paste(strwrap(text,width=15),collapse=" \n ")
  return(wtext)
}

#READ IN DATA
wd = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures"
wd_S1 = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures_STRATEGY5_add10%toflow"
wd_S2 = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures_STRATEGY5_add20%toflow"
wd_S3 = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures_STRATEGY5_red4degCalltribs"
wd_S4 = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures_STRATEGY5_red4degCdams"
wd_S5 = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures_STRATEGY4"
wd_sav = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures/"

file_list = list.files(wd, pattern = '*.txt')[1:25]
file_list = file_list[!grepl("Hist", file_list)]
file_list = file_list[!grepl("DryYearFishTargets", file_list)]

# measures_hist = fread(file.path(wd, "Hist_KRBS_measures_CMIP3_2030.txt"),sep="\t")
# measures_hist = measures_hist %>% dplyr::rename(measure = V1, value_hist = V2, Units = V3) %>% dplyr::select(-Units)
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
	measures_tempS1 = fread(file.path(wd_S1, file_list[i]))
	measures_tempS1 = measures_tempS1 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Add Flow\n10%'))
	measures_tempS2 = fread(file.path(wd_S2, file_list[i]))
	measures_tempS2 = measures_tempS2 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Add Flow\n20%'))
	measures_tempS3 = fread(file.path(wd_S3, file_list[i]))
	measures_tempS3 = measures_tempS3 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Reduce\nTribs\n4degC'))
	measures_tempS4 = fread(file.path(wd_S4, file_list[i]))
	measures_tempS4 = measures_tempS4 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Reduce\nDam\nOutflow\n4degC'))
	measures_tempS5 = fread(file.path(wd_S5, file_list[i]))
	measures_tempS5 = measures_tempS5 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Reduce\nShasta\nScott\n4degC'))
	measures_dt = rbind_list(measures_dt, measures_hist, measures_temp, measures_tempS1, measures_tempS2, measures_tempS3, measures_tempS4, measures_tempS5)
}

#MERGES FUTURE SCENARIO DATA AND HISTORIC DATA
# measures_dt = left_join(measures_dt, measures_hist)
# measures_dtmag = left_join(measures_dt, measures_hist)

#CALCULATES PERCENT DIFFERENCE BETWEEN HISTORIC (S0) AND FUTURE PERIODS
# measures_dt = measures_dt %>% dplyr::mutate(pctchange = pctdiff(value, value_hist))
#CALCULATES MAGNITUDE DIFFERENCE BETWEEN HISTORIC (S0) AND FUTURE PERIODS
# measures_dtmag = measures_dtmag %>% dplyr::mutate(magchange = magdiff(value, value_hist))

#SAVES DATA AS CSV
write.csv(measures_dt, file.path(wd_sav, 'kb_measures_STRATEGY5all_forgaps.csv'), row.names = F)
# write.csv(measures_dtmag, file.path(wd_sav, 'kb_measures_mag_STRATEGY5all.csv'), row.names = F)

#CREATES COLUMN WITH MULTILINE TEXT
measures_dt$measure_mline = llply(measures_dt$measure, wrapit)
measures_dt$measure_mline <- unlist(measures_dt$measure_mline)
# measures_dtmag$measure_mline = llply(measures_dtmag$measure, wrapit)
# measures_dtmag$measure_mline <- unlist(measures_dtmag$measure_mline)

#==========================
# WATER QUALITY
#==========================

#FILTERS DATA BY CATEGORY - WATER QUALITY
measures_wq = measures_dt %>% dplyr::filter(category == 'Water Quality', measure == 'Mean Annual MWAT')
# measures_wq = measures_dtmag %>% dplyr::filter(category == 'Water Quality', measure == 'Mean Annual MWAT')
measures_wq = measures_wq %>% dplyr::mutate(climsetperiod = paste(climset, period))
measures_wq$test=c(paste(measures_wq$scenario,measures_wq$climset,sep="_"))

#WRITE TO FILE BEFORE SUBSETTING TABLE FOR PLOTTING
temp<-select(measures_wq,measure,scenario,period,climset,value)
temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset)
# write.csv(tt, file.path(wd_sav, 'kb_measures_wq_STRATEGYall.csv'), row.names = F)

#PLOTS FOR 2030s
measures_wq2030 = measures_wq %>% dplyr::filter(period == 2030, climset == 'CMIP5')
measures_wq2030$strategy <- factor(measures_wq2030$strategy,levels = c("Historical", "Baseline", "Reduce\nShasta\nScott\n4degC", "Add Flow\n10%", "Add Flow\n20%", "Reduce\nTribs\n4degC", "Reduce\nDam\nOutflow\n4degC"))
ggplot(measures_wq2030, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wq2030,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wq2030$value>=0,0,1), size=2) + geom_hline(yintercept = 63.68, colour="#990000", size=1.0) + scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c","#ff7f00","#6a3d9a"), guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 100) + theme(axis.text.x=element_text(angle=90), axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_wq_STRATEGYall_gapsCMIP5_2030.pdf", device = "pdf", width=6, height=2, dpi=300)
ggsave("measures_wq_STRATEGYall_gapsCMIP5_2030.png", width=6, height=2, dpi=300)

#PLOTS FOR 2070s
measures_wq2070 = measures_wq %>% dplyr::filter(period == 2070, climset == 'CMIP5')
measures_wq2070$strategy <- factor(measures_wq2070$strategy,levels = c("Historical", "Baseline", "Reduce\nShasta\nScott\n4degC", "Add Flow\n10%", "Add Flow\n20%", "Reduce\nTribs\n4degC", "Reduce\nDam\nOutflow\n4degC"))
ggplot(measures_wq2070, aes(x = scenario, y = value, fill = test)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(data=measures_wq2070,aes(x=scenario,y=value,label = paste(round(value),"",sep="")),position=position_dodge(width=0.9), hjust=ifelse(measures_wq2070$value>=0,0,1), size=2) + geom_hline(yintercept = 63.68, colour="#990000", size=1.0) + scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c","#ff7f00","#6a3d9a"), guide = F) + facet_grid(measure_mline~strategy) + coord_flip() + theme_bw() + ylim(0, 100) + theme(axis.text.x=element_text(angle=90), axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))
ggsave("measures_wq_STRATEGYall_gapsCMIP5_2070.pdf", device = "pdf", width=6, height=2, dpi=300)
ggsave("measures_wq_STRATEGYall_gapsCMIP5_2070.png", width=6, height=2, dpi=300)

