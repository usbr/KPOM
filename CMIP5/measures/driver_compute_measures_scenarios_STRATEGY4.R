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
bcsd="CMIP5"

dirpath=paste("C:/PROJECTS/A044F KlamathBasinStudy/data/climatechange/",bcsd,sep="")
basepath="C:/PROJECTS/A044F KlamathBasinStudy/Analysis/ManagementModels/measures"
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
	# ECOLOGICAL MEASURE 2: compute frequency of meeting annual refuge demand (95,400 AF)
	#						by WATER YEAR
	#======================================================

	# Step 1: read in refuge daily diversions in cfs
	refuge.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Refuge.Total Diversion")
	refuge.daily$Year		<- as.numeric(format(refuge.daily$Date,"%Y"))
	refuge.daily$Month		<- as.numeric(format(refuge.daily$Date,"%m"))
	refuge.daily$WYear		<- ifelse(refuge.daily$Month>=10,refuge.daily$Year+1,refuge.daily$Year)
	refuge.daily$Flow_af	<- refuge.daily$Flow*1.9835
	t1						<- refuge.daily[,2:6]

	# Step 2: convert daily diversions to annual totals in AF
	aggdata					<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata					<- cbind(aggdata,unique(t1$WYear))
	refuge.mean.ann.supply	<- apply(aggdata,2,mean)[6]

	# Step 3: compute measure 
	#            mean percent of desired supply (since full supply is never met)
	refuge.mean.percent.desired.supply <- refuge.mean.ann.supply/95400*100
	names(refuge.mean.percent.desired.supply) <- "Perc_tot_supply"
	#print(refuge.mean.percent.desired.supply)

	# write output
	xx <- cbind(sprintf("%25s","Mean Percent of Annual Refuge Demand\t"),
					sprintf("%12.4f\t",refuge.mean.percent.desired.supply),
					sprintf("%12s","% of Years"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	#======================================================
	# ECOLOGICAL MEASURE 3: compute frequency of falling below target pool elevations
	#======================================================

	clearlake.elev			<- data.frame(Date=tmp$Date,Elev=tmp$"Clear Lake.Pool Elevation")
	gerber.elev				<- data.frame(Date=tmp$Date,Elev=tmp$"Gerber Reservoir.Pool Elevation")
	min.clpoolelev=4520.6
	min.grpoolelev=4798.1
	clearlake.elev$Meet		<- ifelse(clearlake.elev$Elev>=min.clpoolelev,1,0)
	gerber.elev$Meet		<- ifelse(gerber.elev$Elev>=min.grpoolelev,1,0)
	tcl 					<- clearlake.elev[which(clearlake.elev$Meet==1),]
	tgr 					<- gerber.elev[which(gerber.elev$Meet==1),]
	clearlake.freq.meet		<- (length(tcl$Meet)/length(clearlake.elev$Elev))*100
	gerber.freq.meet		<- (length(tgr$Meet)/length(gerber.elev$Elev))*100
	#print(clearlake.freq.meet)
	#print(gerber.freq.meet)

	# write output
	# COMMENTED OUT BECAUSE ELEVATIONS ARE ALWAYS MET FOR ALL SCENARIOS...INCLUDE IN DISCUSSION...12/10/2015
	# xx <- cbind(sprintf("%25s","Frequency of Meeting Target Elev ClearLk\t"),
					# sprintf("%12.4f\t",clearlake.freq.meet),
					# sprintf("%12s","% of Days"))
	# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	# xx <- cbind(sprintf("%25s","Frequency of Meeting Target Elev Gerber\t"),
					# sprintf("%12.4f\t",gerber.freq.meet),
					# sprintf("%12s","% of Days"))
	# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	#======================================================
	# POWER MEASURE 1: compute mean annual hydropower generated in megawatts
	#					by WATER YEAR
	#======================================================

	hydropower				<- data.frame(Date=tmp$Date,BoyleHydro=tmp$"JC Boyle Power Plant.Power")
	hydropower$Copco1Hydro	<- tmp$"Copco 1 Reservoir.Power"
	hydropower$Copco2Hydro	<- tmp$"Copco 2 Power Plant.Power"
	hydropower$IGHydro		<- tmp$"Iron Gate Reservoir.Power"
	hydropower$Year			<- as.numeric(format(hydropower$Date,"%Y"))
	hydropower$Month		<- as.numeric(format(hydropower$Date,"%m"))
	hydropower$WYear		<- ifelse(hydropower$Month>=10,hydropower$Year+1,hydropower$Year)
	t1						<- data.frame(SumHydro=rowSums(hydropower[,2:5]))
	t1$WYear				<- hydropower$WYear
	aggdata					<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata					<- cbind(aggdata,Yr=unique(t1$WYear))
	hydropower.ann			<- apply(aggdata,2,mean)[2]
	#print(hydropower.ann)  # megawatts units

	# write output
	xx <- cbind(sprintf("%25s","Mean Annual Hydropower Generated (MW)\t"),
					sprintf("%12.4f\t",hydropower.ann),
					sprintf("%12s","MW"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	#======================================================
	# POWER MEASURE 2: compute volume of spill at select facilities 
	#                  per year BY WATER YEAR
	#======================================================

	vspill				<- data.frame(Date=tmp$Date,BoyleSpill=tmp$"JC Boyle Reservoir.Spill")
	vspill$Copco1		<- tmp$"Copco 1 Reservoir.Spill"
	vspill$IG			<- tmp$"Iron Gate Reservoir.Spill"
	vspill$Year			<- as.numeric(format(hydropower$Date,"%Y"))
	vspill$Month		<- as.numeric(format(hydropower$Date,"%m"))
	vspill$WYear		<- ifelse(vspill$Month>=10,vspill$Year+1,vspill$Year)
	t1					<- vspill[,2:4]*1.9835
	t1$WYear			<- vspill$WYear
	aggdata				<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata				<- cbind(aggdata,Yr=unique(t1$WYear))
	vspill.ann			<- apply(aggdata,2,mean)[2:4]/1000
	#print(vspill.ann)  # mean annual spill in KAF

	# write output
	xx <- cbind(sprintf("%25s","Boyle Mean Spill Volume per year\t"),
					sprintf("%12.4f\t",vspill.ann[1]),
					sprintf("%12s","KAF"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","Copco1 Mean Spill Volume per year\t"),
					sprintf("%12.4f\t",vspill.ann[2]),
					sprintf("%12s","KAF"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","Iron Gate Mean Spill Volume per year\t"),
					sprintf("%12.4f\t",vspill.ann[3]),
					sprintf("%12s","KAF"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	#======================================================
	# POWER MEASURE 3: compute number of spill days at select facilities 
	#                  per year BY WATER YEAR
	#======================================================

	dspill				<- data.frame(Date=tmp$Date,BoyleSpill=tmp$"JC Boyle Reservoir.Spill")
	dspill$Copco1		<- tmp$"Copco 1 Reservoir.Spill"
	dspill$IG			<- tmp$"Iron Gate Reservoir.Spill"
	dspill$Year			<- as.numeric(format(dspill$Date,"%Y"))
	dspill$Month		<- as.numeric(format(dspill$Date,"%m"))
	dspill$WYear		<- ifelse(dspill$Month>=10,dspill$Year+1,dspill$Year)
	t1					<- dspill[,2:4]
	t1$WYear			<- dspill$WYear
	t1$BoyleDays		<- ifelse(t1$BoyleSpill>0.01,1,0)
	t1$Copco1Days		<- ifelse(t1$Copco1>0.01,1,0)
	t1$IGDays			<- ifelse(t1$IG>0.01,1,0)
	aggdata				<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata				<- cbind(aggdata,Yr=unique(t1$WYear))
	dspill.ann			<- apply(aggdata,2,mean)[6:8]
	#print(dspill.ann)  # mean number of spill days per year

	# write output
	xx <- cbind(sprintf("%25s","Boyle Mean Spill Days per year\t"),
					sprintf("%12.4f\t",dspill.ann[1]),
					sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","Copco1 Mean Spill Days per year\t"),
					sprintf("%12.4f\t",dspill.ann[2]),
					sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","IronGate Mean Spill Days per year\t"),
					sprintf("%12.4f\t",dspill.ann[3]),
					sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	#======================================================
	# POWER MEASURE 4: compute timing (date) of seasonal peak flow
	#                  at JCBoyle, Copco1, Iron Gate
	#======================================================
	# compute timing of center of mass...
	#determine averaging period
#	avgperiod <- as.Date(as.character(as.POSIXct(c(tmp[1,1], tmp[length(tmp[,1]),1]))))
	dir.create("./flowfiles",showWarnings = FALSE)
	sim			<- data.frame(Date=tmp$Date,BoyleInflow=tmp$"JC Boyle Reservoir.Inflow")
	sim$Copco1	<- tmp$"Copco 1 Reservoir.Inflow"
	sim$IG		<- tmp$"Iron Gate Reservoir.Inflow"

	#create array for mean flows by month
				   #data, nrow, ncol
	meansim=matrix(0,12,3)
	areasim=matrix(0,1,3)
	xbar=matrix(NA,nrow=1,ncol=3)
	pkdate=matrix(NA,nrow=1,ncol=3)
	
	for (isite in 1:3){

		#need to compute averages by month
		sim$month<- months(sim$Date)
		mean.sim<-rep(0,12)
		mean.sim[4]<-with(sim, mean(sim[,isite+1][sim$month=="January"]))
		mean.sim[5]<-with(sim, mean(sim[,isite+1][sim$month=="February"]))
		mean.sim[6]<-with(sim, mean(sim[,isite+1][sim$month=="March"]))
		mean.sim[7]<-with(sim, mean(sim[,isite+1][sim$month=="April"]))
		mean.sim[8]<-with(sim, mean(sim[,isite+1][sim$month=="May"]))
		mean.sim[9]<-with(sim, mean(sim[,isite+1][sim$month=="June"]))
		mean.sim[10]<-with(sim, mean(sim[,isite+1][sim$month=="July"]))
		mean.sim[11]<-with(sim, mean(sim[,isite+1][sim$month=="August"]))
		mean.sim[12]<-with(sim, mean(sim[,isite+1][sim$month=="September"]))
		mean.sim[1]<-with(sim, mean(sim[,isite+1][sim$month=="October"]))
		mean.sim[2]<-with(sim, mean(sim[,isite+1][sim$month=="November"]))
		mean.sim[3]<-with(sim, mean(sim[,isite+1][sim$month=="December"]))
		meansim[,isite]<-mean.sim
		#area under the hydrograph
		f=approxfun(1:12,meansim[,isite])
		area=integrate(f,1,12)
		areasim[,isite]=area$value

		#centre of flow/mass
		months=1:12 #representing Oct-Sep
		#loop through sites
		xbar[1,isite]=sum(meansim[,isite]*months)/areasim[1,isite]
		pkdate[1,isite]=xbar[1,isite]/12*365
	}
	#write output to separate files
	if(scen==1){
		fout1=paste("./flowfiles/ALLAREA_transpose_",futperiod,"_",bcsd,".txt",sep="")
		fout2=paste("./flowfiles/xbar_transpose_",futperiod,"_",bcsd,".txt",sep="")
		fout3=paste("./flowfiles/SeasonalPeakDates_transpose_",futperiod,"_",bcsd,".txt",sep="")
		write.table(areasim,file=fout1,append=F,row.names=F,col.names=F,sep="\t",quote=FALSE)
		write.table(xbar,file=fout2,append=F,row.names=F,col.names=F,sep="\t",quote=FALSE)
		write.table(pkdate,file=fout3,append=F,row.names=F,col.names=F,sep="\t",quote=FALSE)
	} else {
		write.table(areasim,file=fout1,append=T,row.names=F,col.names=F,sep="\t",quote=FALSE)
		write.table(xbar,file=fout2,append=T,row.names=F,col.names=F,sep="\t",,quote=FALSE)
		write.table(pkdate,file=fout3,append=T,row.names=F,col.names=F,sep="\t",quote=FALSE)
	}

	#======================================================
	# POWER MEASURE 5: compute fraction of seasonal volume as spill
	#======================================================

	library(xts)
	flowvol					<- data.frame(Date=tmp$Date,BoyleSpill=tmp$"JC Boyle Reservoir.Spill")
	flowvol$Copco1Spill		<- tmp$"Copco 1 Reservoir.Spill"
	flowvol$IGSpill			<- tmp$"Iron Gate Reservoir.Spill"
	flowvol$BoyleInflow		<- tmp$"JC Boyle Reservoir.Inflow"
	flowvol$Copco1Inflow	<- tmp$"Copco 1 Reservoir.Inflow"
	flowvol$IGInflow		<- tmp$"Iron Gate Reservoir.Inflow"
	flowvol$Year			<- as.numeric(format(flowvol$Date,"%Y"))
	flowvol$Month			<- as.numeric(format(flowvol$Date,"%m"))
	t1						<- flowvol[,2:7]*1.9835
	t1$Date					<- flowvol$Date
	t2						<- as.xts(t1[,1:6], order.by=t1$Date, dateFormat="POSIXct",frequency=NULL,.RECLASS=FALSE)
	t2mon					<- data.frame(BoyleSpill=apply.monthly(t2[,1],sum))
	t2mon$CopcoSpill		<- data.frame(apply.monthly(t2[,2],sum))
	t2mon$IGSpill			<- data.frame(apply.monthly(t2[,3],sum))
	t2mon$BoyleInflow		<- data.frame(apply.monthly(t2[,4],sum))
	t2mon$CopcoInflow		<- data.frame(apply.monthly(t2[,5],sum))
	t2mon$IGInflow			<- data.frame(apply.monthly(t2[,6],sum))
	t3mon					<- data.frame(BoyleSpillFract=t2mon$BoyleSpill/t2mon$BoyleInflow)
	t3mon$CopcoSpillFract	<- t2mon$CopcoSpill/t2mon$CopcoInflow
	t3mon$IGSpillFract		<- t2mon$IGSpill/t2mon$IGInflow
	t3mon$Month				<- as.numeric(format(as.Date(rownames(t3mon)),"%m"))
	colnames(t3mon)			<-c("BoyleSpillFract","CopcoSpillFract","IGSpillFract","Month")
	BoylemeanSpillFrac		<- aggregate(t3mon[,1], by=list(t3mon$Month), FUN=mean, na.rm=TRUE)
	CopcomeanSpillFrac		<- aggregate(t3mon[,2], by=list(t3mon$Month), FUN=mean, na.rm=TRUE)
	IGmeanSpillFrac			<- aggregate(t3mon[,3], by=list(t3mon$Month), FUN=mean, na.rm=TRUE)
	meanSpillFrac			<- cbind(BoylemeanSpillFrac[,2],CopcomeanSpillFrac[,2],IGmeanSpillFrac[,2])

	# write output
	# xx <- cbind(sprintf("%25s","Boyle Mean Spill per year:\t"),
					# sprintf("%12.4f\t",vspill.ann[1]),
					# sprintf("%12s","AF"))
	# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	# xx <- cbind(sprintf("%25s","Copco1 Mean Spill per year:\t"),
					# sprintf("%12.4f\t",vspill.ann[2]),
					# sprintf("%12s","AF"))
	# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	# xx <- cbind(sprintf("%25s","Iron Gate Mean Spill per year:\t"),
					# sprintf("%12.4f\t",vspill.ann[3]),
					# sprintf("%12s","AF"))
	# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	#======================================================
	# POWER MEASURE 6: change in storage at UKL
	#                  on what timescale??
	#======================================================

	#======================================================
	# FLOOD CONTROL MEASURE 1: frequency of flood control releases
	#                          from UKL
	#======================================================

	UKLexcessRel.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"ReservoirOperations.UKL Incremental Excess Water")
	UKLexcessRel.daily$Month	<- as.numeric(format(UKLexcessRel.daily$Date,"%m"))
	UKLexcessRel.daily$Day		<- as.numeric(format(UKLexcessRel.daily$Date,"%d"))

	# compute measure as mean percent of days per water year that flood control releases are made
	UKLexcessRel.daily$Flag		<- ifelse(UKLexcessRel.daily$Flow>=1,1,0)
	t1 							<- UKLexcessRel.daily[which(UKLexcessRel.daily$Flag==1),]
	UKLfc.freq					<- (length(t1$Flag)/length(UKLexcessRel.daily$Flow))*100

	# write output
	xx <- cbind(sprintf("%25s","Freq of UKL Flood Control Release\t"),
					sprintf("%12.4f\t",UKLfc.freq[1]),
					sprintf("%12s","Percent of Days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	#======================================================
	# FLOOD CONTROL MEASURE 2: mean annual flood control release volume
	#                          from UKL, by WATER YEAR
	#======================================================
	UKLexcessRel.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"ReservoirOperations.UKL Incremental Excess Water")
	UKLexcessRel.daily$Month	<- as.numeric(format(UKLexcessRel.daily$Date,"%m"))
	UKLexcessRel.daily$Day		<- as.numeric(format(UKLexcessRel.daily$Date,"%d"))
	UKLexcessRel.daily$Year		<- as.numeric(format(UKLexcessRel.daily$Date,"%Y"))
	UKLexcessRel.daily$WYear	<- ifelse(UKLexcessRel.daily$Month>=10,UKLexcessRel.daily$Year+1,UKLexcessRel.daily$Year)

	UKLexcessRel.daily$Flow_af	<- UKLexcessRel.daily$Flow*1.9835
	t1							<- UKLexcessRel.daily[,3:7]

	# Step 2: convert daily diversions to annual totals in KAF
	aggdata					<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata					<- cbind(aggdata,unique(t1$WYear))
	UKLexcess.mean.ann		<- apply(aggdata,2,mean)[6]/1000

	# write output
	xx <- cbind(sprintf("%25s","Mean Ann UKL Flood Control Release Volume\t"),
					sprintf("%12.4f\t",UKLexcess.mean.ann[1]),
					sprintf("%12s","KAF"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	#======================================================
	# RECREATION MEASURE 1 & 2: days within acceptable flow ranges for fishing & boating
	#                           evaluated at identified reaches, based on mean days per WATER YEAR
	#======================================================

	# Keno Reach (Link River to Keno Dam)
	kenoreach.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Keno Dam.Inflow")
	kenoreach.daily$Month	<- as.numeric(format(kenoreach.daily$Date,"%m"))
	kenoreach.daily$Day		<- as.numeric(format(kenoreach.daily$Date,"%d"))
	kenoreach.daily$Year	<- as.numeric(format(kenoreach.daily$Date,"%Y"))
	kenoreach.daily$WYear	<- ifelse(kenoreach.daily$Month>=10,kenoreach.daily$Year+1,kenoreach.daily$Year)
	t1						<- kenoreach.daily[,2:6]
	t1$KenoReachFishingDays	<- ifelse(t1$Flow>=200 & t1$Flow<=1500,1,0)
	t1$KenoReachBoatingDays	<- ifelse(t1$Flow>=1000 & t1$Flow<=4000,1,0)
	aggdata					<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata					<- cbind(aggdata,Yr=unique(t1$WYear))
	kenoreach.mean.ann		<- apply(aggdata,2,mean)[6:8]
	#print(kenoreach.mean.ann)  # mean number of fishing and boating days per year

	# JCBoyle Reach (upstream of dam)
	boylereach.daily		<- data.frame(Date=tmp$Date,Flow=tmp$"JC Boyle Reservoir.Inflow")
	boylereach.daily$Month	<- as.numeric(format(boylereach.daily$Date,"%m"))
	boylereach.daily$Day	<- as.numeric(format(boylereach.daily$Date,"%d"))
	boylereach.daily$Year	<- as.numeric(format(boylereach.daily$Date,"%Y"))
	boylereach.daily$WYear	<- ifelse(boylereach.daily$Month>=10,boylereach.daily$Year+1,boylereach.daily$Year)
	t1						<- boylereach.daily[,2:6]
	t1$BoyleReachFishingDays	<- ifelse(t1$Flow>=200 & t1$Flow<=1000,1,0)
	t1$BoyleReachBoatingDays	<- ifelse(t1$Flow>=1300 & t1$Flow<=1800,1,0)
	aggdata						<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata						<- cbind(aggdata,Yr=unique(t1$WYear))
	boylereach.mean.ann			<- apply(aggdata,2,mean)[6:8]
	#print(boylereach.mean.ann)  # mean number of fishing and boating days per year

	# Hells Corner Reach (Boyle to Copco)
	hellscornerreach.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Copco 1 Reservoir.Inflow")
	hellscornerreach.daily$Month	<- as.numeric(format(hellscornerreach.daily$Date,"%m"))
	hellscornerreach.daily$Day		<- as.numeric(format(hellscornerreach.daily$Date,"%d"))
	hellscornerreach.daily$Year		<- as.numeric(format(hellscornerreach.daily$Date,"%Y"))
	hellscornerreach.daily$WYear	<- ifelse(hellscornerreach.daily$Month>=10,hellscornerreach.daily$Year+1,hellscornerreach.daily$Year)
	t1								<- hellscornerreach.daily[,2:6]
	t1$HCReachFishingDays	<- ifelse(t1$Flow>=200 & t1$Flow<=1500,1,0)
	t1$HCReachBoatingDays	<- ifelse(t1$Flow>=1000 & t1$Flow<=3500,1,0)
	aggdata					<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata					<- cbind(aggdata,Yr=unique(t1$WYear))
	hcreach.mean.ann		<- apply(aggdata,2,mean)[6:8]
	#print(hcreach.mean.ann)  # mean number of fishing and boating days per year

	# Copco2 Bypass Reach ()

	# Iron Gate to Scott Reach
	IGScottreach.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Klamath Scott.Inflow1")
	IGScottreach.daily$Month	<- as.numeric(format(IGScottreach.daily$Date,"%m"))
	IGScottreach.daily$Day		<- as.numeric(format(IGScottreach.daily$Date,"%d"))
	IGScottreach.daily$Year		<- as.numeric(format(IGScottreach.daily$Date,"%Y"))
	IGScottreach.daily$WYear	<- ifelse(IGScottreach.daily$Month>=10,IGScottreach.daily$Year+1,IGScottreach.daily$Year)
	t1							<- IGScottreach.daily[,2:6]
	t1$IGScottReachFishingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=4000,1,0)
	t1$IGScottReachBoatingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=4000,1,0)
	aggdata						<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata						<- cbind(aggdata,Yr=unique(t1$WYear))
	IGScottReach.mean.ann		<- apply(aggdata,2,mean)[6:8]
	#print(IGScottReach.mean.ann)  # mean number of fishing and boating days per year

	# Scott to Salmon Reach
	ScottSalmonreach.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Klamath Salmon.Inflow1")
	ScottSalmonreach.daily$Month	<- as.numeric(format(ScottSalmonreach.daily$Date,"%m"))
	ScottSalmonreach.daily$Day		<- as.numeric(format(ScottSalmonreach.daily$Date,"%d"))
	ScottSalmonreach.daily$Year		<- as.numeric(format(ScottSalmonreach.daily$Date,"%Y"))
	ScottSalmonreach.daily$WYear	<- ifelse(ScottSalmonreach.daily$Month>=10,ScottSalmonreach.daily$Year+1,ScottSalmonreach.daily$Year)
	t1								<- ScottSalmonreach.daily[,2:6]
	t1$ScottSalmonReachFishingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=4000,1,0)
	t1$ScottSalmonReachBoatingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=7000,1,0)
	aggdata							<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata							<- cbind(aggdata,Yr=unique(t1$WYear))
	ScottSalmonReach.mean.ann		<- apply(aggdata,2,mean)[6:8]
	#print(ScottSalmonReach.mean.ann)  # mean number of fishing and boating days per year

	# Salmon to Trinity Reach
	SalmonTrinityreach.daily			<- data.frame(Date=tmp$Date,Flow=tmp$"Klamath Trinity.Inflow1")
	SalmonTrinityreach.daily$Month		<- as.numeric(format(SalmonTrinityreach.daily$Date,"%m"))
	SalmonTrinityreach.daily$Day		<- as.numeric(format(SalmonTrinityreach.daily$Date,"%d"))
	SalmonTrinityreach.daily$Year		<- as.numeric(format(SalmonTrinityreach.daily$Date,"%Y"))
	SalmonTrinityreach.daily$WYear		<- ifelse(SalmonTrinityreach.daily$Month>=10,SalmonTrinityreach.daily$Year+1,SalmonTrinityreach.daily$Year)
	t1									<- SalmonTrinityreach.daily[,2:6]
	t1$SalmonTrinityReachFishingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=10000,1,0)
	t1$SalmonTrinityReachBoatingDays	<- ifelse(t1$Flow>=800 & t1$Flow<=10000,1,0)
	aggdata								<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata								<- cbind(aggdata,Yr=unique(t1$WYear))
	SalmonTrinityReach.mean.ann			<- apply(aggdata,2,mean)[6:8]
	#print(SalmonTrinityReach.mean.ann)  # mean number of fishing and boating days per year

	# Trinity to Ocean Reach
	TrinityOceanreach.daily				<- data.frame(Date=tmp$Date,Flow=tmp$"Klamath Near Klamath.Gage Outflow")
	TrinityOceanreach.daily$Month		<- as.numeric(format(TrinityOceanreach.daily$Date,"%m"))
	TrinityOceanreach.daily$Day			<- as.numeric(format(TrinityOceanreach.daily$Date,"%d"))
	TrinityOceanreach.daily$Year		<- as.numeric(format(TrinityOceanreach.daily$Date,"%Y"))
	TrinityOceanreach.daily$WYear		<- ifelse(TrinityOceanreach.daily$Month>=10,TrinityOceanreach.daily$Year+1,TrinityOceanreach.daily$Year)
	t1									<- TrinityOceanreach.daily[,2:6]
	t1$TrinityOceanReachFishingDays		<- ifelse(t1$Flow>=1000 & t1$Flow<=18000,1,0)
	t1$TrinityOceanReachBoatingDays		<- ifelse(t1$Flow>=1000 & t1$Flow<=18000,1,0)
	aggdata								<- aggregate(t1, by=list(t1$WYear), FUN=sum, na.rm=TRUE)
	aggdata								<- cbind(aggdata,Yr=unique(t1$WYear))
	TrinityOceanReach.mean.ann			<- apply(aggdata,2,mean)[6:8]
	#print(TrinityOceanReach.mean.ann)  # mean number of fishing and boating days per year

	# write output
	# fishing days
	xx <- cbind(sprintf("%25s","KenoReach Mean Ann Fishing Days\t"),sprintf("%12.4f\t",kenoreach.mean.ann[2]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","BoyleReach Mean Ann Fishing Days\t"),sprintf("%12.4f\t",boylereach.mean.ann[2]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","HellsCornerReach Mean Ann Fishing Days\t"),sprintf("%12.4f\t",hcreach.mean.ann[2]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","IGScottReach Mean Ann Fishing Days\t"),sprintf("%12.4f\t",IGScottReach.mean.ann[2]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","ScottSalmonReach Mean Ann Fishing Days\t"),sprintf("%12.4f\t",ScottSalmonReach.mean.ann[2]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","SalmonTrinityReach Mean Ann Fishing Days\t"),sprintf("%12.4f\t",SalmonTrinityReach.mean.ann[2]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","TrinityOceanReach Mean Ann Fishing Days\t"),sprintf("%12.4f\t",TrinityOceanReach.mean.ann[2]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	# boating days
	xx <- cbind(sprintf("%25s","KenoReach Mean Ann Boating Days\t"),sprintf("%12.4f\t",kenoreach.mean.ann[3]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","BoyleReach Mean Ann Boating Days\t"),sprintf("%12.4f\t",boylereach.mean.ann[3]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","HellsCornerReach Mean Ann Boating Days\t"),sprintf("%12.4f\t",hcreach.mean.ann[3]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","IGScottReach Mean Ann Boating Days\t"),sprintf("%12.4f\t",IGScottReach.mean.ann[3]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","ScottSalmonReach Mean Ann Boating Days\t"),sprintf("%12.4f\t",ScottSalmonReach.mean.ann[3]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","SalmonTrinityReach Mean Ann Boating Days\t"),sprintf("%12.4f\t",SalmonTrinityReach.mean.ann[3]),sprintf("%12s","days"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","TrinityOceanReach Mean Ann Boating Days\t"),sprintf("%12.4f\t",TrinityOceanReach.mean.ann[3]),sprintf("%12s","days"))
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
	t2							<- aggdata[,2]/fullsupply*100
	ProjectPercentFullSupply.mean	<- mean(t2)  #can take mean of percent because all use same "fullsupply" as denominator
	#print(ProjectPercentFullSupply.mean)
	xx <- cbind(sprintf("%25s","Mean Percent Full Supply (Apr-Sep)\t"),sprintf("%12.4f\t",ProjectPercentFullSupply.mean),sprintf("%12s","%"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	#======================================================
	# WATER DELIVERY MEASURE 2: end of February storage plus actual March-September inflow
	#======================================================

	UKLstorage.daily			<- data.frame(Date=tmp$Date,Storage=tmp$"Upper Klamath Lake.Storage")
	UKLstorage.daily$Infl		<- tmp$"Upper Klamath Lake.Inflow"
	UKLstorage.daily$Tomorrow	<- as.Date(UKLstorage.daily$Date) + 1
	UKLstorage.daily$YestMonth	<- as.numeric(format(UKLstorage.daily$Tomorrow,"%m"))
	UKLstorage.daily$YestDay	<- as.numeric(format(UKLstorage.daily$Tomorrow,"%d"))
	EndofFebStorage				<- UKLstorage.daily[which(UKLstorage.daily$YestMonth==3 &
										UKLstorage.daily$YestDay==1),]
	UKLstorage.daily$Infl_af	<- UKLstorage.daily$Infl*1.9835 #convert cfs to AF

	Inflow						<- data.frame(Date=as.Date(UKLstorage.daily$Date),Infl_af=UKLstorage.daily$Infl_af)
	Inflow$Month				<- as.numeric(format(UKLstorage.daily$Date,"%m"))
	Inflow$Year					<- as.numeric(format(UKLstorage.daily$Date,"%Y"))
	seldata						<- Inflow[which(Inflow$Month>=3 & Inflow$Month<=9),]
	MarchSeptInflow				<- aggregate(seldata[,2:4], by=list(seldata$Year), FUN=sum, na.rm=TRUE)
	ProjectActualSupply			<- data.frame(Year=MarchSeptInflow[,1],
									EOFstg=EndofFebStorage$Storage,
									MarSepInfl=MarchSeptInflow$Infl_af,
									SumSupply=(EndofFebStorage$Storage+MarchSeptInflow$Infl_af))
	MeanProjectActualSupply		<- apply(ProjectActualSupply,2,mean)[4]/1000
	# write output
	xx <- cbind(sprintf("%25s","Mean Annual UKL Stg and Inflow\t"),
					sprintf("%12.4f\t",MeanProjectActualSupply),
					sprintf("%12s","KAF"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)


	#======================================================
	# WATER DELIVERY MEASURE 3: Klamath Project delivery
	#                           what would be the variable for this??
	#======================================================

	#======================================================
	# WATER DELIVERY MEASURE 4: difference between total available flow and total basin demand
	#                           Scott and Shasta River basins
	#                           Defined simply as mean annual flow in Scott and Shasta Rivers
	#======================================================
	# Step 1: read in Scott and Shasta flow data in cfs
	ScottShasta.daily				<- data.frame(Date=tmp$Date,ShastaFlow=tmp$"Shasta Near Yreka.Gage Outflow")
	ScottShasta.daily$ScottFlow		<- tmp$"Scott Near Ft Jones.Gage Outflow"
	ScottShasta.daily$Month			<- as.numeric(format(ScottShasta.daily$Date,"%m"))
	ScottShasta.daily$Day			<- as.numeric(format(ScottShasta.daily$Date,"%d"))
	MeanAnnualFlow					<- apply(ScottShasta.daily[,2:3],2,mean)
	# write output
	xx <- cbind(sprintf("%25s","Mean Annual Shasta Flow\t"),
					sprintf("%12.4f\t",MeanAnnualFlow[1]),
					sprintf("%12s","cfs"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	xx <- cbind(sprintf("%25s","Mean Annual Scott Flow\t"),
					sprintf("%12.4f\t",MeanAnnualFlow[2]),
					sprintf("%12s","cfs"))
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

	# write output
	
# In-stream temperature monitoring provided daily and seasonal maxima and minima, diurnal
# ranges, Maximum Weekly Average Temperature (MWAT) and Maximum Weekly Maximum
# Temperature (MWMT) at specific sites within the river. The MWAT and MWMT are the highest
# seven-day moving average of the daily mean or maximum temperature, respectively. Stream
# temperature statistics for 2003 are in Table 1.

	# xx <- cbind(sprintf("%25s","Count of MWAT - Poor\t"),
					# sprintf("%12.4f\t",mwat.table[4]),
					# sprintf("%12s","degC"))
	# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	# xx <- cbind(sprintf("%25s","Count of MWAT - Fair\t"),
					# sprintf("%12.4f\t",mwat.table[3]),
					# sprintf("%12s","degC"))
	# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	# xx <- cbind(sprintf("%25s","Count of MWAT - Good\t"),
					# sprintf("%12.4f\t",mwat.table[2]),
					# sprintf("%12s","degC"))
	# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	# xx <- cbind(sprintf("%25s","Count of MWAT - Very Good\t"),
					# sprintf("%12.4f\t",mwat.table[1]),
					# sprintf("%12s","degC"))
	# write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)

	xx <- cbind(sprintf("%25s","Mean Annual MWAT\t"),
					sprintf("%12.4f\t",poor.mwat.avg[3]),
					sprintf("%12s","degF"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
					xx <- cbind(sprintf("%25s","Mean exceedence of MWAT - Poor\t"),
					sprintf("%12.4f\t",poor.mwat.avg[6]),
					sprintf("%12s","degF"))
	write.table(xx,fout,row.names=F,col.names=F,sep=" ",append=TRUE,quote=FALSE)
	
} #end scenario loop
