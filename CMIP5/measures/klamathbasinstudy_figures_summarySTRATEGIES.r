
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
	measures_tempS1 = measures_tempS1 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Reduce ET 30%'))
	measures_tempS2 = fread(file.path(wd_S2, file_list[i]))
	measures_tempS2 = measures_tempS2 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Reduce ET 50%'))
	measures_tempS3 = fread(file.path(wd_S3, file_list[i]))
	measures_tempS3 = measures_tempS3 %>% dplyr::rename(measure = V1, value = V2, units = V3) %>% dplyr::mutate(category = c(rep('Ecological Resources', 3), rep('Hydropower', 7), rep('Flood Control', 2), rep('Recreation Fishing', 7), rep('Recreation Boating', 7), rep('Water Delivery', 4), rep('Water Quality', 2)), scenario = scenario_temp, climset = climset_temp, period = period_temp, strategy = c('Add 30KAF'))
	measures_dt = rbind_list(measures_dt, measures_temp, measures_tempS1, measures_tempS2, measures_tempS3)
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
# 2070s Ecological Resources
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - ECOLOGICAL RESOURCES & 2070s
measures_temp = measures_dt %>% dplyr::filter(category == 'Ecological Resources', period == '2070')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
measures_temp$scenchar = measures_temp$scenario
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WW', 1))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WD', 2))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HW', 3))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HD', 4))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='CT', 5))
# assume positive change is green (improvement), negative change is red (worsening)
# bin changes
ymin = floor(min(measures_temp$pctchange))
ymax = ceiling(max(measures_temp$pctchange))
magmax = max(abs(ymin),abs(ymax))
bins = seq(from = -magmax, to = magmax, length.out = 5)
measures_temp$colournm = ifelse(measures_temp$pctchange>=bins[1] & measures_temp$pctchange<bins[2],"1",
								ifelse(measures_temp$pctchange>=bins[2] & measures_temp$pctchange<bins[3],"2",
								ifelse(measures_temp$pctchange>=bins[3] & measures_temp$pctchange<bins[4],"3",
								ifelse(measures_temp$pctchange>=bins[4] & measures_temp$pctchange<bins[5],"4","NA"))))
#"1"="#990000", dk red
#"2"="#FF9999", salmon
#"3"="#33FF33", lt green
#"4"="#006600", dk green
#c("1"="#006600","2"="#66FF66","3"="#FF9999","4"="#CC0000") -- use if negative change is "good"
#c("1"="#CC0000","2"="#FF9999","3"="#66FF66","4"="#006600") -- use if positive change is "good"
measures_temp$test=c(paste(measures_temp$measure,measures_temp$climset,sep="_"))
tmp <- arrange(measures_temp,measure_mline,strategy,climset,scenario,pctchange,colournm)
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#CC0000","2"="#FF9999","3"="#66FF66","4"="#006600"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_eco_summary_2070.png", width=6, height=1.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_eco_summary_2070.csv'), row.names = F)

#===============================
# 2030s Ecological Resources
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - ECOLOGICAL RESOURCES & 2030s
measures_temp = measures_dt %>% dplyr::filter(category == 'Ecological Resources', period == '2030')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
measures_temp$scenchar = measures_temp$scenario
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WW', 1))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WD', 2))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HW', 3))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HD', 4))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='CT', 5))
# assume positive change is green (improvement), negative change is red (worsening)
# bin changes
# USE SAME BINS FROM 2070s CALCS......
measures_temp$colournm = ifelse(measures_temp$pctchange>=bins[1] & measures_temp$pctchange<bins[2],"1",
								ifelse(measures_temp$pctchange>=bins[2] & measures_temp$pctchange<bins[3],"2",
								ifelse(measures_temp$pctchange>=bins[3] & measures_temp$pctchange<bins[4],"3",
								ifelse(measures_temp$pctchange>=bins[4] & measures_temp$pctchange<bins[5],"4","NA"))))
measures_temp$test=c(paste(measures_temp$measure,measures_temp$climset,sep="_"))
tmp <- arrange(measures_temp,measure_mline,strategy,climset,scenario,pctchange,colournm)
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#CC0000","2"="#FF9999","3"="#66FF66","4"="#006600"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_eco_summary_2030.png", width=6, height=1.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_eco_summary_2030.csv'), row.names = F)

#=====================================================================================================================

#===============================
# 2070s Flood Control
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - FLOOD CONTROL & 2070s
measures_temp = measures_dt %>% dplyr::filter(category == 'Flood Control', period == '2070')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
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
								ifelse(measures_temp$pctchange>=bins[4] & measures_temp$pctchange<bins[5],"4",
								ifelse(measures_temp$pctchange>=bins[5] & measures_temp$pctchange<bins[6],"5",
								ifelse(measures_temp$pctchange>=bins[6] & measures_temp$pctchange<bins[7],"6","NA"))))))
measures_temp$test=c(paste(measures_temp$measure,measures_temp$climset,sep="_"))
tmp <- arrange(measures_temp,measure_mline,strategy,climset,scenario,pctchange,colournm)
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#006600","2"="#66FF66","3"="#FF9999","4"="#CC0000"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_flood_summary_2070.png", width=6, height=1.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_flood_summary_2070.csv'), row.names = F)

#===============================
# 2030s Flood Control
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - FLOOD CONTROL & 2030s
measures_temp = measures_dt %>% dplyr::filter(category == 'Flood Control', period == '2030')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
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
ggsave("measures_flood_summary_2030.png", width=6, height=1.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_flood_summary_2030.csv'), row.names = F)

#=====================================================================================================================

#===============================
# 2070s Water Delivery
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - WATER DELIVERY & 2030s
measures_temp = measures_dt %>% dplyr::filter(category == 'Water Delivery', period == '2070')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
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
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#CC0000","2"="#FF9999","3"="#66FF66","4"="#006600"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_wd_summary_2070.png", width=6, height=3.0, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_wd_summary_2070.csv'), row.names = F)

#===============================
# 2030s Water Delivery
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - WATER DELIVERY & 2030s
measures_temp = measures_dt %>% dplyr::filter(category == 'Water Delivery', period == '2030')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
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
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#CC0000","2"="#FF9999","3"="#66FF66","4"="#006600"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_wd_summary_2030.png", width=6, height=3.0, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_wd_summary_2030.csv'), row.names = F)

#=====================================================================================================================

#===============================
# 2070s HYDROPOWER
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - HYDROPWOER & 2070s
measures_temp = measures_dt %>% dplyr::filter(category == 'Hydropower', period == '2070')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
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
measures_temp$colournm = ifelse(measures_temp$measure=='Mean Annual Hydropower Generated (MW)' & measures_temp$colournm==1,4,
								ifelse(measures_temp$measure=='Mean Annual Hydropower Generated (MW)' & measures_temp$colournm==2,3,
								ifelse(measures_temp$measure=='Mean Annual Hydropower Generated (MW)' & measures_temp$colournm==3,2,
								ifelse(measures_temp$measure=='Mean Annual Hydropower Generated (MW)' & measures_temp$colournm==4,1,
								measures_temp$colournm))))
measures_temp$test=c(paste(measures_temp$measure,measures_temp$climset,sep="_"))
tmp <- arrange(measures_temp,measure_mline,strategy,climset,scenario,pctchange,colournm)
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#006600","2"="#66FF66","3"="#FF9999","4"="#CC0000"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_hydro_summary_2070.png", width=6, height=3.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_hydro_summary_2070.csv'), row.names = F)

#===============================
# 2030s HYDROPOWER
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - HYDROPOWER & 2030s
measures_temp = measures_dt %>% dplyr::filter(category == 'Hydropower', period == '2030')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
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
measures_temp$colournm = ifelse(measures_temp$measure=='Mean Annual Hydropower Generated (MW)' & measures_temp$colournm==1,4,
								ifelse(measures_temp$measure=='Mean Annual Hydropower Generated (MW)' & measures_temp$colournm==2,3,
								ifelse(measures_temp$measure=='Mean Annual Hydropower Generated (MW)' & measures_temp$colournm==3,2,
								ifelse(measures_temp$measure=='Mean Annual Hydropower Generated (MW)' & measures_temp$colournm==4,1,
								measures_temp$colournm))))
measures_temp$test=c(paste(measures_temp$measure,measures_temp$climset,sep="_"))
tmp <- arrange(measures_temp,measure_mline,strategy,climset,scenario,pctchange,colournm)
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#006600","2"="#66FF66","3"="#FF9999","4"="#CC0000"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_hydro_summary_2030.png", width=6, height=3.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_hydro_summary_2030.csv'), row.names = F)

#=====================================================================================================================

#===============================
# 2070s RECREATION - BOATING
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - HYDROPOWER & 2030s
measures_temp = measures_dt %>% dplyr::filter(category == 'Recreation Boating', period == '2070')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
measures_temp$scenchar = measures_temp$scenario
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WW', 1))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WD', 2))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HW', 3))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HD', 4))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='CT', 5))
measures_temp$measure_mline <- factor(measures_temp$measure_mline,levels = c("KenoReach Mean Ann \n Boating Days",
																			"BoyleReach Mean Ann \n Boating Days",
																			"HellsCornerReach \n Mean Ann Boating \n Days",
																			"IGScottReach Mean \n Ann Boating Days",
																			"ScottSalmonReach \n Mean Ann Boating \n Days",
																			"SalmonTrinityReach \n Mean Ann Boating \n Days",
																			"TrinityOceanReach \n Mean Ann Boating \n Days"))
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
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#CC0000","2"="#FF9999","3"="#66FF66","4"="#006600"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_recboating_summary_2070.png", width=6, height=3.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_recboating_summary_2070.csv'), row.names = F)

#===============================
# 2030s RECREATION - BOATING
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - REC BOATING & 2030s
measures_temp = measures_dt %>% dplyr::filter(category == 'Recreation Boating', period == '2030')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
measures_temp$scenchar = measures_temp$scenario
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WW', 1))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WD', 2))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HW', 3))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HD', 4))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='CT', 5))
measures_temp$measure_mline <- factor(measures_temp$measure_mline,levels = c("KenoReach Mean Ann \n Boating Days",
																			"BoyleReach Mean Ann \n Boating Days",
																			"HellsCornerReach \n Mean Ann Boating \n Days",
																			"IGScottReach Mean \n Ann Boating Days",
																			"ScottSalmonReach \n Mean Ann Boating \n Days",
																			"SalmonTrinityReach \n Mean Ann Boating \n Days",
																			"TrinityOceanReach \n Mean Ann Boating \n Days"))
# bin changes
# USE SAME BINS FROM 2070s CALCS......
measures_temp$colournm = ifelse(measures_temp$pctchange>=bins[1] & measures_temp$pctchange<bins[2],"1",
								ifelse(measures_temp$pctchange>=bins[2] & measures_temp$pctchange<bins[3],"2",
								ifelse(measures_temp$pctchange>=bins[3] & measures_temp$pctchange<bins[4],"3",
								ifelse(measures_temp$pctchange>=bins[4] & measures_temp$pctchange<bins[5],"4","NA"))))
measures_temp$test=c(paste(measures_temp$measure,measures_temp$climset,sep="_"))
tmp <- arrange(measures_temp,measure_mline,strategy,climset,scenario,pctchange,colournm)
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#CC0000","2"="#FF9999","3"="#66FF66","4"="#006600"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_recboating_summary_2030.png", width=6, height=3.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_recboating_summary_2030.csv'), row.names = F)

#=====================================================================================================================

#===============================
# 2070s RECREATION - FISHING
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - REC FISHING & 2070s
measures_temp = measures_dt %>% dplyr::filter(category == 'Recreation Fishing', period == '2070')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
measures_temp$scenchar = measures_temp$scenario
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WW', 1))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WD', 2))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HW', 3))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HD', 4))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='CT', 5))
measures_temp$measure_mline <- factor(measures_temp$measure_mline,levels = c("KenoReach Mean Ann \n Fishing Days",
																			"BoyleReach Mean Ann \n Fishing Days",
																			"HellsCornerReach \n Mean Ann Fishing \n Days",
																			"IGScottReach Mean \n Ann Fishing Days",
																			"ScottSalmonReach \n Mean Ann Fishing \n Days",
																			"SalmonTrinityReach \n Mean Ann Fishing \n Days",
																			"TrinityOceanReach \n Mean Ann Fishing \n Days"))
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
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#CC0000","2"="#FF9999","3"="#66FF66","4"="#006600"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_recfishing_summary_2070.png", width=6, height=3.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_recfishing_summary_2070.csv'), row.names = F)

#===============================
# 2030s RECREATION - FISHING
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - REC FISHING & 2030s
measures_temp = measures_dt %>% dplyr::filter(category == 'Recreation Fishing', period == '2030')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
measures_temp$scenchar = measures_temp$scenario
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WW', 1))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='WD', 2))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HW', 3))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='HD', 4))
measures_temp = measures_temp %>% mutate(scenario=replace(scenario, scenario=='CT', 5))
measures_temp$measure_mline <- factor(measures_temp$measure_mline,levels = c("KenoReach Mean Ann \n Fishing Days",
																			"BoyleReach Mean Ann \n Fishing Days",
																			"HellsCornerReach \n Mean Ann Fishing \n Days",
																			"IGScottReach Mean \n Ann Fishing Days",
																			"ScottSalmonReach \n Mean Ann Fishing \n Days",
																			"SalmonTrinityReach \n Mean Ann Fishing \n Days",
																			"TrinityOceanReach \n Mean Ann Fishing \n Days"))
# bin changes
# USE SAME BINS FROM 2070s CALCS......
measures_temp$colournm = ifelse(measures_temp$pctchange>=bins[1] & measures_temp$pctchange<bins[2],"1",
								ifelse(measures_temp$pctchange>=bins[2] & measures_temp$pctchange<bins[3],"2",
								ifelse(measures_temp$pctchange>=bins[3] & measures_temp$pctchange<bins[4],"3",
								ifelse(measures_temp$pctchange>=bins[4] & measures_temp$pctchange<bins[5],"4","NA"))))
measures_temp$test=c(paste(measures_temp$measure,measures_temp$climset,sep="_"))
tmp <- arrange(measures_temp,measure_mline,strategy,climset,scenario,pctchange,colournm)
ggplot(tmp, aes(x = climset, y = scenario,colour=tmp$colournm)) + geom_point(stat = 'identity', position = 'dodge',size=4) + scale_colour_manual(values = c("1"="#CC0000","2"="#FF9999","3"="#66FF66","4"="#006600"),guide=FALSE) + coord_flip() + facet_grid(measure_mline~strategy) + theme_bw() + theme(title=element_text('Ecological Resources'), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=8), strip.text.x = element_text(size = 8), strip.text.y = element_text(size = 8, angle = 0), strip.background = element_rect(fill = NA))+scale_y_discrete(labels=c("1"="WW","2"="WD","3"="HW","4"="HD","5"="CT"))
ggsave("measures_recfishing_summary_2030.png", width=6, height=3.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_recfishing_summary_2030.csv'), row.names = F)

#=====================================================================================================================

#===============================
# 2070s WATER QUALITY
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - WATER QUALITY & 2070s
measures_temp = measures_dt %>% dplyr::filter(category == 'Water Quality', period == '2070')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
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
ggsave("measures_wq_summary_2070.png", width=6, height=1.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_wq_summary_2070.csv'), row.names = F)

#===============================
# 2030s WATER QUALITY
#===============================
#FILTERS DATA BY CATEGORY & PERIOD - WATER QUALITY & 2070s
measures_temp = measures_dt %>% dplyr::filter(category == 'Water Quality', period == '2030')
#CREATES MERGED COLUMNS FOR PLOTTING
measures_temp = measures_temp %>% dplyr::mutate(climsetperiod = paste(climset, period), climssetscenario = paste(scenario, climset))
#PLOTS Scatter PLOT - PERIODS IN COLUMNS, MEASURES IN ROWS
measures_temp$test=c(paste(measures_temp$scenario,measures_temp$climset,sep="_"))
measures_temp$strategy <- factor(measures_temp$strategy,levels = c("Baseline", "Reduce ET 30%", "Reduce ET 50%", "Add 30KAF"))
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
ggsave("measures_wq_summary_2030.png", width=6, height=1.8, dpi=300)
temp<-select(measures_temp,measure,scenario,period,climset,strategy,value_hist,pctchange,colournm)
#temp$scenario <- factor(temp$scenario,levels = c("WD", "WW", "HD", "HW", "CT"))
tt <- arrange(temp,measure,period,scenario,climset,strategy)
write.csv(tt, file.path(wd_sav, 'kb_measures_wq_summary_2030.csv'), row.names = F)

