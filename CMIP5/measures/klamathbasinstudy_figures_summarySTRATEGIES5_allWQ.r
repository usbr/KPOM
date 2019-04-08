
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

measures_hist = fread(file.path(wd, "Hist_KRBS_measures_CMIP3_2030.txt"),sep="\t")
measures_hist = measures_hist %>% dplyr::rename(measure = V1, value_hist = V2, Units = V3) %>% dplyr::select(-Units)
measures_dt = NULL

for(i in 1:length(file_list)) {
	info_temp = unlist(strsplit(file_path_sans_ext(file_list[i]), '_'))
	scenario_temp = info_temp[1]
	climset_temp = info_temp[4]
	period_temp = as.numeric(info_temp[5])
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
	measures_dt = rbind_list(measures_dt, measures_temp, measures_tempS1, measures_tempS2, measures_tempS3, measures_tempS4, measures_tempS5)
}

#MERGES FUTURE SCENARIO DATA AND HISTORIC DATA
measures_dt = left_join(measures_dt, measures_hist)
measures_dtmag = left_join(measures_dt, measures_hist)

#CALCULATES PERCENT DIFFERENCE BETWEEN HISTORIC (S0) AND FUTURE PERIODS
measures_dt = measures_dt %>% dplyr::mutate(pctchange = pctdiff(value, value_hist))
#CALCULATES MAGNITUDE DIFFERENCE BETWEEN HISTORIC (S0) AND FUTURE PERIODS
measures_dtmag = measures_dtmag %>% dplyr::mutate(magchange = magdiff(value, value_hist))

#CREATES COLUMN WITH MULTILINE TEXT
measures_dt$measure_mline = llply(measures_dt$measure, wrapit)
measures_dt$measure_mline <- unlist(measures_dt$measure_mline)
measures_dtmag$measure_mline = llply(measures_dtmag$measure, wrapit)
measures_dtmag$measure_mline <- unlist(measures_dtmag$measure_mline)

#===============================
# 2070s WATER QUALITY
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - WATER QUALITY & 2070s
#measures_temp = measures_dt %>% dplyr::filter(category == 'Water Quality', period == '2070')
measures_temp = measures_dt %>% dplyr::filter(category == 'Water Quality', period == '2070',measure == 'Mean exceedence of MWAT - Poor')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce\nShasta\nScott\n4degC", "Add Flow\n10%", "Add Flow\n20%", "Reduce\nTribs\n4degC", "Reduce\nDam\nOutflow\n4degC"))
measures_temp$scenchar = measures_temp$scenario
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WW', 1))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WD', 2))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HW', 3))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HD', 4))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='CT', 5))
# bin changes
ymin = floor(min(measures_temp$pctchange))
ymax = ceiling(max(measures_temp$pctchange))
magmax = max(abs(ymin),abs(ymax))
bins = seq(from = -magmax, to = magmax, length.out = 5)
measures_temp$colournm = ifelse(measures_temp$pctchange>=bins[1] & measures_temp$pctchange<bins[2],"1",
								ifelse(measures_temp$pctchange>=bins[2] & measures_temp$pctchange<bins[3],"2",
								ifelse(measures_temp$pctchange>=bins[3] & measures_temp$pctchange<bins[4],"3",
								ifelse(measures_temp$pctchange>=bins[4] & measures_temp$pctchange<bins[5],"4","NA"))))
measures_temp$test=c(paste(measures_temp$measure,measures_temp$climset,sep="_"))
tmp <- arrange(measures_temp,measure_mline,strategy,climset,scenario,pctchange,colournm)
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#006600","2"="#66FF66","3"="#FF9999","4"="#CC0000"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_wq_ALLsummary_2070.png", width=6, height=1.4, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_wq_ALLsummary_2070.csv'), row.names = F)

#===============================
# 2030s WATER QUALITY
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - WATER QUALITY & 2070s
measures_temp = measures_dt %>% dplyr::filter(category == 'Water Quality', period == '2030')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce\nShasta\nScott\n4degC", "Add Flow\n10%", "Add Flow\n20%", "Reduce\nTribs\n4degC", "Reduce\nDam\nOutflow\n4degC"))
measures_temp$scenchar = measures_temp$scenario
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WW', 1))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WD', 2))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HW', 3))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HD', 4))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='CT', 5))
# bin changes
# USE SAME BINS FROM 2070s CALCS......
measures_temp$colournm = ifelse(measures_temp$pctchange>=bins[1] & measures_temp$pctchange<bins[2],"1",
								ifelse(measures_temp$pctchange>=bins[2] & measures_temp$pctchange<bins[3],"2",
								ifelse(measures_temp$pctchange>=bins[3] & measures_temp$pctchange<bins[4],"3",
								ifelse(measures_temp$pctchange>=bins[4] & measures_temp$pctchange<bins[5],"4","NA"))))
measures_temp$test=c(paste(measures_temp$measure,measures_temp$climset,sep="_"))
tmp <- arrange(measures_temp,measure_mline,strategy,climset,scenario,pctchange,colournm)
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#006600","2"="#66FF66","3"="#FF9999","4"="#CC0000"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_wq_ALLsummary_2030.png", width=6, height=1.4, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_wq_ALLsummary_2030.csv'), row.names = F)

