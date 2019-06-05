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
#dirpath="C:/PROJECTS/A044F KlamathBasinStudy/data/simulated"
futperiod="2030"
bcsd="CMIP3"

dirpath=paste("C:/PROJECTS/A044F KlamathBasinStudy/data/climatechange/",bcsd,sep="")
basepath="C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures"
#dirpath=paste("T:/WaterResources/PlanningOperations/8210_Projects/BasinStudies/KlamathBasin/data/climatechange/",bcsd,sep="")
#basepath="T:/WaterResources/PlanningOperations/8210_Projects/BasinStudies/KlamathBasin/Analysis/ManagementModels/Measures"
fname=paste("KBS",futperiod,"DailyCCRWOutput.xlsx",sep="")

# libraries
library(readxl)
library(xts)

# user input
fin=fname

#scenario="Historical"
scenario=c("S0","S1","S2","S3","S4","S5")
scenario2=c("hist","warmdry","warmwet","hotdry","hotwet","middle")
new_scen=c("Hist","WD","WW","HD","HW","CT")
nscen=length(scenario)

fishin="DryYearFishTargets.txt"
fish.targets <- read.table(paste(basepath,"/",fishin,sep=""),header=TRUE)

for(scen in 1:nscen){

	print(scenario[scen])
	# Specify sheet with a number or name
	tmp <- read_excel(paste(dirpath,"/",fin,sep=""), sheet = scenario[scen])
	tmp <- tmp[complete.cases(tmp),]
	
	# setup output file
	fout=paste(basepath,"/",new_scen[scen],"_KRBS_measures_",bcsd,"_",futperiod,".txt",sep="")
	header <- sprintf("%12s\t%12s\t%12s","Measure","Value","Units")
	write(header,fout,append=F)
	write("----------------------------------------",fout,append=T)

	#======================================================
	# ECOLOGICAL MEASURE 1: compute frequency of meeting flow targets
	#======================================================

	# Step 1: read in Scott and Shasta flow data in cfs
	shasta.daily		<- data.frame(Date=tmp$Date,Flow=tmp$"Shasta Near Yreka.Gage Outflow")
	scott.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Scott Near Ft Jones.Gage Outflow")
	shasta.daily$Month	<- as.numeric(format(shasta.daily$Date,"%m"))
	scott.daily$Month	<- as.numeric(format(scott.daily$Date,"%m"))
	shasta.daily$Day	<- as.numeric(format(shasta.daily$Date,"%d"))
	scott.daily$Day		<- as.numeric(format(scott.daily$Date,"%d"))

	# Step 2: read in DryYearFishTargets.txt (from McBain and Trush (2014)
	#  already done outside of loop

	# Step 3: compute measures
	#         frequency of meeting flow targets
	shasta.w.targets		<- merge(x=fish.targets,y=shasta.daily, by.x=c("Month", "Day"),by.y=c("Month", "Day"))
	shasta.w.targets		<- shasta.w.targets[order(shasta.w.targets$Date),] 
	shasta.w.targets$Meet	<- ifelse(shasta.w.targets$Flow>=shasta.w.targets$DryYearTarget,1,0)
	t1 						<- shasta.w.targets[which(shasta.w.targets$Meet==1),]
	shasta.freq.meet		<- (length(t1$Meet)/length(shasta.w.targets$Flow))*100
	#print(shasta.freq.meet)

	scott.w.targets			<- merge(x=fish.targets,y=scott.daily, by.x=c("Month", "Day"),by.y=c("Month", "Day"))
	scott.w.targets			<- scott.w.targets[order(scott.w.targets$Date),] 
	scott.w.targets$Meet	<- ifelse(scott.w.targets$Flow>=scott.w.targets$DryYearTarget,1,0)
	t1 						<- scott.w.targets[which(scott.w.targets$Meet==1),]
	scott.freq.meet			<- (length(t1$Meet)/length(scott.w.targets$Flow))*100
	#print(scott.freq.meet)

	# write output
	xx <- cbind(sprintf("%25s","Frequency Meeting Dry Year Fish Targets Scott\t"),
					sprintf("%12.4f\t",scott.freq.meet),
					sprintf("%12s","% of Days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","Frequency Meeting Dry Year Fish Targets Shasta\t"),
					sprintf("%12.4f\t",shasta.freq.meet),
					sprintf("%12s","% of Days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)


	#======================================================
	# WATER QUALITY MEASURE 1: Max Weekly Average Temperature (degrees C)
	#                           
	#======================================================
	begdate = "1969-10-01"
	enddate = "1999-09-30"
	temppath = "C:/PROJECTS/A044F KlamathBasinStudy/Analysis/StreamTemperatureModel/RBM10_KRBS_runs"
	fintemp = paste("krbs_",scenario2[scen],"_",bcsd,"_",futperiod,".out",sep="")
	tmpt			<- read.table(paste(temppath,"/",fintemp,sep=""))
	dts				<- seq(as.Date(begdate),as.Date(enddate),by="day")
	tmpt			<- cbind(dts,tmpt)
	colnames(tmpt)	<- c("Date","Yeardec","Seg","Temp","Stdev")
	tmpt2			<- as.xts(tmpt[,2:5], order.by=tmpt$Date, frequency=1,.RECLASS=FALSE)
	tmptzoo			<- zoo(tmpt2)
	tmpt.rollavg	<- rollmean(tmptzoo, 7)
	tmpt.rollavg$WYear	<- floor(tmpt.rollavg$Yeardec)+1
	mwat			<- aggregate(tmpt.rollavg, by=list(tmpt.rollavg$WYear), FUN=max, na.rm=TRUE)
	wyearcount		<- length(unique(tmpt.rollavg$WYear))
	mwat.class		<- cut(mwat$Temp, c(0,15, 16, 17.6,30), include.lowest = TRUE)
	mwat.table		<- table(mwat.class)
	mwat.poor.dist	<- as.data.frame(mwat)
	mwat.poor.dist$Temp	<- (mwat.poor.dist$Temp)*9/5+32
	mwat.poor.dist$dis	<- mwat.poor.dist$Temp-((17.6*9/5)+32)
	poor.mwat.avg	<- apply(mwat.poor.dist,2,mean)

	xx <- cbind(sprintf("%25s","Mean Annual MWAT\t"),
					sprintf("%12.4f\t",poor.mwat.avg[3]),
					sprintf("%12s","degF"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
					xx <- cbind(sprintf("%25s","Mean exceedence of MWAT - Poor\t"),
					sprintf("%12.4f\t",poor.mwat.avg[6]),
					sprintf("%12s","degF"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	
} #end scenario loop
