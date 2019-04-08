#
#Main program to:
#	compute ecological resources measures
#	for Klamath River Basin Study
#
#U.S. Bureau of Reclamation: October 2015.
#
#######################################################################

# default - clear namespace
rm(list=ls())

# base directory for dependencies and user input file for run setup
dirpath="C:/PROJECTS/A044F KlamathBasinStudy/data/simulated"
basepath="C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures"
# historical
fname="KBSModelOutputData.xlsx"
# S0
#fname="KBSModelOutputData_Scenario.xlsx"

# libraries
library(readxl)

# user input
fin=fname

# Specify sheet with a number or name
tmp <- read_excel(paste(dirpath,"/",fin,sep=""), sheet = "Daily")
tmp <- tmp[complete.cases(tmp),]

# setup output file
scenario="Historical"
#scenario="S0"
fout=paste(basepath,"/",scenario,"_KRBS_measures.txt",sep="")
header <- sprintf("%12s\t%12s\t%12s","Measure","Value","Units")
write(header,fout,append=F)
write("----------------------------------------",fout,append=T)

#======================================================
# ECOLOGICAL MEASURE 1: compute frequency of meeting flow targets
#======================================================

# Step 1: read in Scott and Shasta flow data in cfs
shasta.daily <- data.frame(Date=tmp$Date,Flow=tmp$"Shasta Near Yreka.Gage Outflow")
scott.daily <- data.frame(Date=tmp$Date,Flow=tmp$"Scott Near Ft Jones.Gage Outflow")
shasta.daily$Month	<- as.numeric(format(shasta.daily$Date,"%m"))
scott.daily$Month	<- as.numeric(format(scott.daily$Date,"%m"))
shasta.daily$Day	<- as.numeric(format(shasta.daily$Date,"%d"))
scott.daily$Day		<- as.numeric(format(scott.daily$Date,"%d"))

# Step 2: read in DryYearFishTargets.txt (from McBain and Trush (2014)
fin="DryYearFishTargets.txt"
fish.targets <- read.table(paste(basepath,"/",fin,sep=""),header=TRUE)

# Step 3: compute measures
#         frequency of meeting flow targets
shasta.w.targets		<- merge(x=fish.targets,y=shasta.daily, by.x=c("Month", "Day"),by.y=c("Month", "Day"))
shasta.w.targets		<- shasta.w.targets[order(shasta.w.targets$Date),] 
shasta.w.targets$Meet	<- ifelse(shasta.w.targets$Flow>=shasta.w.targets$DryYearTarget,1,0)
t1 						<- shasta.w.targets[which(shasta.w.targets$Meet==1),]
shasta.freq.meet		<- (length(t1$Meet)/length(shasta.w.targets$Flow))*100
print(shasta.freq.meet)

scott.w.targets			<- merge(x=fish.targets,y=scott.daily, by.x=c("Month", "Day"),by.y=c("Month", "Day"))
scott.w.targets			<- scott.w.targets[order(scott.w.targets$Date),] 
scott.w.targets$Meet	<- ifelse(scott.w.targets$Flow>=scott.w.targets$DryYearTarget,1,0)
t1 						<- scott.w.targets[which(scott.w.targets$Meet==1),]
scott.freq.meet			<- (length(t1$Meet)/length(scott.w.targets$Flow))*100
print(scott.freq.meet)

# write output
xx <- cbind(sprintf("%25s","Frequency Meeting Dry Year Fish Targets Scott:\t"),
				sprintf("%12.4f\t",scott.freq.meet),
				sprintf("%12s","% of Days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","Frequency Meeting Dry Year Fish Targets Shasta:\t"),
				sprintf("%12.4f\t",shasta.freq.meet),
				sprintf("%12s","% of Days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)


#======================================================
# ECOLOGICAL MEASURE 2: compute frequency of meeting annual refuge demand (95,400 AF)
#======================================================

# Step 1: read in refuge daily diversions in cfs
refuge.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Refuge.Total Diversion")
refuge.daily$Year		<- as.numeric(format(refuge.daily$Date,"%Y"))
refuge.daily$Flow_af	<- refuge.daily$Flow*1.9835
t1						<- refuge.daily[,2:4]

# Step 2: convert daily diversions to annual totals in AF
aggdata					<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata					<- cbind(aggdata,unique(t1$Year))
refuge.mean.ann.supply	<- apply(aggdata,2,mean)[4]

# Step 3: compute measure 
#            mean percent of desired supply (since full supply is never met)
refuge.mean.percent.desired.supply <- refuge.mean.ann.supply/95400*100
names(refuge.mean.percent.desired.supply) <- "Perc_tot_supply"
print(refuge.mean.percent.desired.supply)

# write output
xx <- cbind(sprintf("%25s","Frequency Meeting Annual Refuge Demand:\t"),
				sprintf("%12.4f\t",refuge.mean.percent.desired.supply),
				sprintf("%12s","% of Years"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

#======================================================
# ECOLOGICAL MEASURE 3: compute frequency of falling below target pool elevations
#======================================================

clearlake.elev			<- data.frame(Date=tmp$Date,Elev=tmp$"Clear Lake.Pool Elevation")
gerber.elev				<- data.frame(Date=tmp$Date,Elev=tmp$"Gerber Reservoir.Pool Elevation")
min.clpoolelev=4521
min.grpoolelev=4799.6
clearlake.elev$Meet		<- ifelse(clearlake.elev$Elev>=min.clpoolelev,1,0)
gerber.elev$Meet		<- ifelse(gerber.elev$Elev>=min.grpoolelev,1,0)
tcl 					<- clearlake.elev[which(clearlake.elev$Meet==1),]
tgr 					<- gerber.elev[which(gerber.elev$Meet==1),]
clearlake.freq.meet		<- (length(tcl$Meet)/length(clearlake.elev$Elev))*100
gerber.freq.meet		<- (length(tgr$Meet)/length(gerber.elev$Elev))*100
print(clearlake.freq.meet)
print(gerber.freq.meet)

# write output
xx <- cbind(sprintf("%25s","Frequency of Meeting Target Elev ClearLk:\t"),
				sprintf("%12.4f\t",clearlake.freq.meet),
				sprintf("%12s","% of Days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","Frequency of Meeting Target Elev Gerber:\t"),
				sprintf("%12.4f\t",gerber.freq.meet),
				sprintf("%12s","% of Days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

#======================================================
# POWER MEASURE 1: compute mean annual hydropower generated in megawatts
#======================================================

hydropower				<- data.frame(Date=tmp$Date,BoyleHydro=tmp$"JC Boyle Power Plant.Power")
hydropower$Copco1Hydro	<- tmp$"Copco 1 Reservoir.Power"
hydropower$Copco2Hydro	<- tmp$"Copco 2 Power Plant.Power"
hydropower$IGHydro		<- tmp$"Iron Gate Reservoir.Power"
hydropower$Year			<- as.numeric(format(hydropower$Date,"%Y"))
t1						<- data.frame(SumHydro=rowSums(hydropower[,2:5]))
t1$Year					<- hydropower$Year
aggdata					<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata					<- cbind(aggdata,Yr=unique(t1$Year))
hydropower.ann			<- apply(aggdata,2,mean)[2]
print(hydropower.ann)  # megawatts units

# write output
xx <- cbind(sprintf("%25s","Mean Annual Hydropower Generated MW:\t"),
				sprintf("%12.4f\t",hydropower.ann),
				sprintf("%12s","MW"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

#======================================================
# POWER MEASURE 2: compute volume of spill at select facilities 
#                  per year (for now) BY CALENDAR YEAR FOR NOW
#======================================================

vspill				<- data.frame(Date=tmp$Date,BoyleSpill=tmp$"JC Boyle Reservoir.Spill")
vspill$Copco1		<- tmp$"Copco 1 Reservoir.Spill"
vspill$IG			<- tmp$"Iron Gate Reservoir.Spill"
vspill$Year			<- as.numeric(format(hydropower$Date,"%Y"))
t1					<- vspill[,2:4]*1.9835
t1$Year				<- vspill$Year
aggdata				<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata				<- cbind(aggdata,Yr=unique(t1$Year))
vspill.ann			<- apply(aggdata,2,mean)[2:4]
print(vspill.ann)  # mean annual spill in AF

# write output
xx <- cbind(sprintf("%25s","Boyle Mean Spill per year:\t"),
				sprintf("%12.4f\t",vspill.ann[1]),
				sprintf("%12s","AF"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","Copco1 Mean Spill per year:\t"),
				sprintf("%12.4f\t",vspill.ann[2]),
				sprintf("%12s","AF"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","Iron Gate Mean Spill per year:\t"),
				sprintf("%12.4f\t",vspill.ann[3]),
				sprintf("%12s","AF"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

#======================================================
# POWER MEASURE 3: compute number of spill days at select facilities 
#                  per year (for now) BY CALENDAR YEAR FOR NOW
#======================================================

dspill				<- data.frame(Date=tmp$Date,BoyleSpill=tmp$"JC Boyle Reservoir.Spill")
dspill$Copco1		<- tmp$"Copco 1 Reservoir.Spill"
dspill$IG			<- tmp$"Iron Gate Reservoir.Spill"
dspill$Year			<- as.numeric(format(hydropower$Date,"%Y"))
t1					<- dspill[,2:4]
t1$Year				<- dspill$Year
t1$BoyleDays		<- ifelse(t1$BoyleSpill>0.01,1,0)
t1$Copco1Days		<- ifelse(t1$Copco1>0.01,1,0)
t1$IGDays			<- ifelse(t1$IG>0.01,1,0)
aggdata				<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata				<- cbind(aggdata,Yr=unique(t1$Year))
dspill.ann			<- apply(aggdata,2,mean)[6:8]
print(dspill.ann)  # mean number of spill days per year

# write output
xx <- cbind(sprintf("%25s","Boyle Mean Days Spill per year:\t"),
				sprintf("%12.4f\t",dspill.ann[1]),
				sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","Copco1 Mean Days Spill per year:\t"),
				sprintf("%12.4f\t",dspill.ann[2]),
				sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","IronGate Mean Days Spill per year:\t"),
				sprintf("%12.4f\t",dspill.ann[3]),
				sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

#======================================================
# POWER MEASURE 4: compute timing (date) of seasonal peak flow
#                  at Iron Gate
#======================================================
# perhaps compute timing of center of mass...

#======================================================
# POWER MEASURE 5: compute fraction of seasonal volume as spill
#======================================================

#======================================================
# POWER MEASURE 6: change in storage at UKL
#                  on what timescale??
#======================================================

#======================================================
# POWER MEASURE 6: change in storage at UKL
#                  on what timescale??
#======================================================

#======================================================
# FLOOD CONTROL MEASURE 1: frequency of flood control releases
#                          from UKL (use C1_EXC for this calc for now...
#======================================================

UKLexcessRel.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"ReservoirOperations.UKL Excess Water Release")
UKLexcessRel.daily$Month	<- as.numeric(format(UKLexcessRel.daily$Date,"%m"))
UKLexcessRel.daily$Day		<- as.numeric(format(UKLexcessRel.daily$Date,"%d"))

#### this quantity is always > 0
#### what to do?

# write output
# xx <- cbind(sprintf("%25s","Freq of UKL Flood Control Release: "),
				# sprintf("%12.4f",dspill.ann[1]),
				# sprintf("%12s","days"))
# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)


#======================================================
# RECREATION MEASURE 1 & 2: days within acceptable flow ranges for fishing & boating
#                           evaluated at identified reaches, based on mean days per calendar year
#======================================================

# Keno Reach (Link River to Keno Dam)
kenoreach.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Keno Dam.Inflow")
kenoreach.daily$Month	<- as.numeric(format(kenoreach.daily$Date,"%m"))
kenoreach.daily$Day		<- as.numeric(format(kenoreach.daily$Date,"%d"))
kenoreach.daily$Year	<- as.numeric(format(kenoreach.daily$Date,"%Y"))
t1						<- kenoreach.daily[,2:5]
t1$KenoReachFishingDays	<- ifelse(t1$Flow>=200 & t1$Flow<=1500,1,0)
t1$KenoReachBoatingDays	<- ifelse(t1$Flow>=1000 & t1$Flow<=4000,1,0)
aggdata					<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata					<- cbind(aggdata,Yr=unique(t1$Year))
kenoreach.mean.ann				<- apply(aggdata,2,mean)[6:8]
print(kenoreach.mean.ann)  # mean number of fishing and boating days per year

# JCBoyle Reach (upstream of dam)
boylereach.daily		<- data.frame(Date=tmp$Date,Flow=tmp$"JC Boyle Reservoir.Inflow")
boylereach.daily$Month	<- as.numeric(format(boylereach.daily$Date,"%m"))
boylereach.daily$Day	<- as.numeric(format(boylereach.daily$Date,"%d"))
boylereach.daily$Year	<- as.numeric(format(boylereach.daily$Date,"%Y"))
t1						<- boylereach.daily[,2:5]
t1$BoyleReachFishingDays	<- ifelse(t1$Flow>=200 & t1$Flow<=1000,1,0)
t1$BoyleReachBoatingDays	<- ifelse(t1$Flow>=1300 & t1$Flow<=1800,1,0)
aggdata					<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata					<- cbind(aggdata,Yr=unique(t1$Year))
boylereach.mean.ann				<- apply(aggdata,2,mean)[6:8]
print(boylereach.mean.ann)  # mean number of fishing and boating days per year

# Hells Corner Reach (Boyle to Copco)
hellscornerreach.daily		<- data.frame(Date=tmp$Date,Flow=tmp$"Copco 1 Reservoir.Inflow")
hellscornerreach.daily$Month<- as.numeric(format(hellscornerreach.daily$Date,"%m"))
hellscornerreach.daily$Day	<- as.numeric(format(hellscornerreach.daily$Date,"%d"))
hellscornerreach.daily$Year	<- as.numeric(format(hellscornerreach.daily$Date,"%Y"))
t1							<- hellscornerreach.daily[,2:5]
t1$HCReachFishingDays	<- ifelse(t1$Flow>=200 & t1$Flow<=1500,1,0)
t1$HCReachBoatingDays	<- ifelse(t1$Flow>=1000 & t1$Flow<=3500,1,0)
aggdata					<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata					<- cbind(aggdata,Yr=unique(t1$Year))
hcreach.mean.ann				<- apply(aggdata,2,mean)[6:8]
print(hcreach.mean.ann)  # mean number of fishing and boating days per year

# Copco2 Bypass Reach ()

# Iron Gate to Scott Reach
IGScottreach.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Klamath Scott.Inflow1")
IGScottreach.daily$Month	<- as.numeric(format(IGScottreach.daily$Date,"%m"))
IGScottreach.daily$Day		<- as.numeric(format(IGScottreach.daily$Date,"%d"))
IGScottreach.daily$Year		<- as.numeric(format(IGScottreach.daily$Date,"%Y"))
t1							<- IGScottreach.daily[,2:5]
t1$IGScottReachFishingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=4000,1,0)
t1$IGScottReachBoatingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=4000,1,0)
aggdata						<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata						<- cbind(aggdata,Yr=unique(t1$Year))
IGScottReach.mean.ann		<- apply(aggdata,2,mean)[6:8]
print(IGScottReach.mean.ann)  # mean number of fishing and boating days per year

# Scott to Salmon Reach
ScottSalmonreach.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Klamath Salmon.Inflow1")
ScottSalmonreach.daily$Month	<- as.numeric(format(ScottSalmonreach.daily$Date,"%m"))
ScottSalmonreach.daily$Day		<- as.numeric(format(ScottSalmonreach.daily$Date,"%d"))
ScottSalmonreach.daily$Year		<- as.numeric(format(ScottSalmonreach.daily$Date,"%Y"))
t1								<- ScottSalmonreach.daily[,2:5]
t1$ScottSalmonReachFishingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=4000,1,0)
t1$ScottSalmonReachBoatingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=7000,1,0)
aggdata							<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata							<- cbind(aggdata,Yr=unique(t1$Year))
ScottSalmonReach.mean.ann			<- apply(aggdata,2,mean)[6:8]
print(ScottSalmonReach.mean.ann)  # mean number of fishing and boating days per year

# Salmon to Trinity Reach
SalmonTrinityreach.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Klamath Trinity.Inflow1")
SalmonTrinityreach.daily$Month		<- as.numeric(format(SalmonTrinityreach.daily$Date,"%m"))
SalmonTrinityreach.daily$Day		<- as.numeric(format(SalmonTrinityreach.daily$Date,"%d"))
SalmonTrinityreach.daily$Year		<- as.numeric(format(SalmonTrinityreach.daily$Date,"%Y"))
t1									<- SalmonTrinityreach.daily[,2:5]
t1$SalmonTrinityReachFishingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=10000,1,0)
t1$SalmonTrinityReachBoatingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=10000,1,0)
aggdata								<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata								<- cbind(aggdata,Yr=unique(t1$Year))
SalmonTrinityReach.mean.ann			<- apply(aggdata,2,mean)[6:8]
print(SalmonTrinityReach.mean.ann)  # mean number of fishing and boating days per year

# Trinity to Ocean Reach
TrinityOceanreach.daily				<- data.frame(Date=tmp$Date,Flow=tmp$"Klamath Near Klamath.Gage Outflow")
TrinityOceanreach.daily$Month		<- as.numeric(format(TrinityOceanreach.daily$Date,"%m"))
TrinityOceanreach.daily$Day			<- as.numeric(format(TrinityOceanreach.daily$Date,"%d"))
TrinityOceanreach.daily$Year		<- as.numeric(format(TrinityOceanreach.daily$Date,"%Y"))
t1									<- TrinityOceanreach.daily[,2:5]
t1$TrinityOceanReachFishingDays		<- ifelse(t1$Flow>=1000 & t1$Flow<=18000,1,0)
t1$TrinityOceanReachBoatingDays		<- ifelse(t1$Flow>=1000 & t1$Flow<=18000,1,0)
aggdata								<- aggregate(t1, by=list(t1$Year), FUN=sum, na.rm=TRUE)
aggdata								<- cbind(aggdata,Yr=unique(t1$Year))
TrinityOceanReach.mean.ann			<- apply(aggdata,2,mean)[6:8]
print(TrinityOceanReach.mean.ann)  # mean number of fishing and boating days per year

# write output
# fishing days
xx <- cbind(sprintf("%25s","KenoReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",kenoreach.mean.ann[1]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","BoyleReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",boylereach.mean.ann[1]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","HellsCornerReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",hcreach.mean.ann[1]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","IGScottReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",IGScottReach.mean.ann[1]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","ScottSalmonReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",ScottSalmonReach.mean.ann[1]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","SalmonTrinityReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",SalmonTrinityReach.mean.ann[1]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","TrinityOceanReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",TrinityOceanReach.mean.ann[1]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
# boating days
xx <- cbind(sprintf("%25s","KenoReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",kenoreach.mean.ann[2]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","BoyleReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",boylereach.mean.ann[2]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","HellsCornerReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",hcreach.mean.ann[2]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","IGScottReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",IGScottReach.mean.ann[2]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","ScottSalmonReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",ScottSalmonReach.mean.ann[2]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","SalmonTrinityReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",SalmonTrinityReach.mean.ann[2]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
xx <- cbind(sprintf("%25s","TrinityOceanReach Mean Ann Fishing Days:\t"),sprintf("%12.4f\t",TrinityOceanReach.mean.ann[2]),sprintf("%12s","days"))
write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)


#======================================================
# WATER DELIVERY MEASURE 1: percent of full Klamath Project Supply
#                           take average monthly value from April-September
#                           to compute percentage
#======================================================
ProjectSupply.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"ProjectData.ProjectSupply")
ProjectSupply.daily$Month	<- as.numeric(format(ProjectSupply.daily$Date,"%m"))
ProjectSupply.daily$Day		<- as.numeric(format(ProjectSupply.daily$Date,"%d"))
ProjectSupply.daily$Year	<- as.numeric(format(ProjectSupply.daily$Date,"%Y"))
t1							<- ProjectSupply.daily[,2:5]
seldata						<- t1[which(t1$Month>=4 & t1$Month<=9),]
aggdata						<- aggregate(seldata, by=list(seldata$Year), FUN=mean, na.rm=TRUE)
fullsupply=390000 #AF
ProjectPercentFullSupply.mean	<- apply(aggdata,2,function(x) {x/fullsupply*100})[2]
print(ProjectPercentFullSupply.mean)

#======================================================
# WATER DELIVERY MEASURE 2: end of February storage plus actual March-September inflow
#======================================================


#======================================================
# WATER DELIVERY MEASURE 3: Klamath Project delivery
#                           what would be the variable for this??
#======================================================

#======================================================
# WATER DELIVERY MEASURE 4: differene between total available flow and total basin demand
#                           Scott and Shasta River basins
#                           Need to define variables......
#======================================================

