#
#Main program to:
#	compute ecological resources measures
#	for Klamath River Basin Study
#
#U.S. Bureau of Reclamation: October 2015.
#
#######################################################################

#default - clear namespace
rm(list=ls())

#base directory for dependencies and user input file for run setup
dirpath="T:/WaterResources/PlanningOperations/8210_Projects/BasinStudies/KlamathBasin/data/simulated"

library(xlsx)

#load dependencies
# srclist=c("plot_meanhydrograph_3.R")
# src_dir = paste(dirpath,"/",sep="")
# nsrc=length(paste(src_dir,srclist,sep=""))
# for(i in 1:nsrc){
  # source(file.path(src_dir, srclist[i]))
# }

#user input
fin="KBSModelEcologicalOutputMeasures.xlsx"
tmp <- read.xlsx(paste(dirpath,"/",fin,sep=""), sheetName = "Daily")
tmp$Date <- tmp$NA.

# compute 

##parse user input file
# SIMDIR <- tmp[1,2]
# OBSDIR <- tmp[2,2]
# USGSDIR <- tmp[3,2]
# OUTDIR <- tmp[4,2]
# SIMFILE <- tmp[5,2]
# OBSFILE	<- tmp[6,2]
# USGSFILE <- tmp[7,2]
# OUTFILE <- tmp[8,2]
# STAVG <- tmp[9,2] #  histyyyymm
# ENDAVG <- tmp[10,2]	#  histyyyymm
# SITE <- tmp[11,2]

# print(STAVG)
##determine averaging period
# avgperiod <- as.Date(c(STAVG, ENDAVG))

##read simulated flow file
# simfn=paste(SIMDIR,"/",SIMFILE,sep="")
# simft=read.table(simfn,header=FALSE)  #reads file in as a data.frame
# sim<-data.frame(date=as.Date(paste(simft[,1],"-",simft[,2],"-",1,sep="")),simft[,3])

##read obs (nat) flow file
# obsfn=paste(OBSDIR,"/",OBSFILE,sep="")
# obsft=read.table(obsfn,header=FALSE)  #reads file in as a data.frame
# obs<-data.frame(date=as.Date(paste(obsft[,1],"-",obsft[,2],"-",1,sep="")),obsft[,3])

##read USGS measured flow file
# usgsfn=paste(USGSDIR,"/",USGSFILE,sep="")
# usgsft=read.table(usgsfn,header=FALSE)  #reads file in as a data.frame
# usgs<-data.frame(date=as.Date(paste(usgsft[,1],"-",usgsft[,2],"-",1,sep="")),usgsft[,3])

# #output directory
# outfl=paste(OUTDIR,"/",OUTFILE,sep="")
# dir.create(OUTDIR,showWarnings = FALSE)

##subset data over averaging period
# sub.sim <- sim[(sim$date >= avgperiod[1]) & (sim$dat <= avgperiod[2]), ]
# sub.obs <- obs[(obs$date >= avgperiod[1]) & (obs$dat <= avgperiod[2]), ]
# sub.usgs <- obs[(usgs$date >= avgperiod[1]) & (usgs$dat <= avgperiod[2]), ]
# sub.sim$month<-months(sub.sim$date)
# sub.obs$month<-months(sub.obs$date)
# sub.usgs$month<-months(sub.usgs$date)
 
##need to compute averages by month...possibly use this
# mean.sim<-rep(0,12)
# mean.obs<-rep(0,12)
# mean.usgs<-rep(0,12)
# mean.sim[4]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="January"]))
# mean.sim[5]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="February"]))
# mean.sim[6]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="March"]))
# mean.sim[7]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="April"]))
# mean.sim[8]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="May"]))
# mean.sim[9]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="June"]))
# mean.sim[10]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="July"]))
# mean.sim[11]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="August"]))
# mean.sim[12]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="September"]))
# mean.sim[1]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="October"]))
# mean.sim[2]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="November"]))
# mean.sim[3]<-with(sub.sim, mean(sub.sim[,2][sub.sim$month=="December"]))

# mean.obs[4]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="January"]))
# mean.obs[5]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="February"]))
# mean.obs[6]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="March"]))
# mean.obs[7]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="April"]))
# mean.obs[8]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="May"]))
# mean.obs[9]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="June"]))
# mean.obs[10]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="July"]))
# mean.obs[11]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="August"]))
# mean.obs[12]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="September"]))
# mean.obs[1]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="October"]))
# mean.obs[2]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="November"]))
# mean.obs[3]<-with(sub.obs, mean(sub.obs[,2][sub.obs$month=="December"]))

# mean.usgs[4]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="January"]))
# mean.usgs[5]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="February"]))
# mean.usgs[6]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="March"]))
# mean.usgs[7]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="April"]))
# mean.usgs[8]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="May"]))
# mean.usgs[9]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="June"]))
# mean.usgs[10]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="July"]))
# mean.usgs[11]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="August"]))
# mean.usgs[12]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="September"]))
# mean.usgs[1]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="October"]))
# mean.usgs[2]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="November"]))
# mean.usgs[3]<-with(sub.usgs, mean(sub.usgs[,2][sub.usgs$month=="December"]))

# xnames=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun",
             # "Jul","Aug","Sep")

# header=paste(SITE,"\n Mean ",
  		# avgperiod[1]," to ",avgperiod[2],collapse=NULL,sep="")
  # plot_meanhydrograph_3(mean.sim,mean.obs,mean.usgs,xnames,header)
  # dev.print(png,file=outfl,width = 5, height = 3,
           # units = "in", res=600)
  # dev.off()
