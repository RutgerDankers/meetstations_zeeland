# modified for HPC and R version 3.5.3
# module load R/3.5.3
# Rscript Zeeland_data.R >& /lustre/scratch/WUR/ESG/danke010/Zeeland/latest_run.Rout

# libraries
#.libPaths("D:/UserData/R/win-library/3.3")
library (RCurl)
library (ggplot2)
library (scales)
library (gtable)
library (grid)
library(caTools)

# get the functions
# this assumes these are located in the current working directory
source("function_tmrt.r")
source("function_utci.r")
source("functions_wbgt.r")

# working directory
#rm(list = ls())
wd <- "/lustre/scratch/WUR/ESG/danke010/Zeeland/"
# change this!
#setwd(wd)

graphsdir <- paste(wd,"graphs/",sep="")
if (!dir.exists(graphsdir)) {dir.create(graphsdir)}

# colours
#ec2c33
#128b4a
#165da7
#f37b2d
#65328f

# parameters for UTCI
pres <- 988.
lat <- 52.0
lon <- 10.0

# stations currently in maintenance
inmaintenance = vector() # c(108,109)

# =====================================================
# get 15 min data for all 5 dataloggers and make graphs

print("getting station data to create graphs...")

# rename current files
#file.rename(from=list.files(pattern="*_current.log"), to=gsub("current","old",list.files(pattern="*_current.log")))

for (i in 108:112){

  print(paste("station nr",i))
  datadir <- paste(wd,"i",i,"/",sep="")
  if (!dir.exists(datadir)) {dir.create(datadir)}
  
  # rename current files
  file.rename(from=list.files(path=datadir, pattern="*_current.log",full.names=TRUE), to=gsub("current","old",list.files(path=datadir,pattern="*_current.log",full.names=TRUE)))
  
  # get list of file names from FTP server
  curl <-  getCurlHandle()
  url <- paste("ftp://ftp.wur.nl/CALM/Zeeland/logger_ibox-",i,"/",sep="")
  userpwd <- "wur\\username:password" # replace with FTP account details
  filenames <- getURL(url, userpwd = userpwd, ftp.use.epsv = FALSE, dirlistonly = TRUE, curl=curl)
  destnames <- strsplit(filenames, "\r*\n")[[1]] # destfiles = origin file names
  if (length(destnames) == 0){
    next
  }
  # if file not exist in destination get it
  for (j in 1:length(destnames)){
    if(!file.exists( paste(datadir,destnames[j],sep="") )){
	print(paste("getting file", destnames[j]))
    a <- getBinaryURL(paste("ftp://ftp.wur.nl/CALM/Zeeland/logger_ibox-",i,"/",destnames[j],sep=""), curl = curl, dirlistonly = FALSE)
    #Sys.sleep(1)
    writeBin(a, paste(datadir,destnames[j],sep=""))
	}
  }
  reset(curl)

  # combine all .txt files into 1 dataframe
  pattern <- paste("^i",i,".*txt$",sep="")
  #files <- list.files(path=datadir, pattern = pattern,full.names=TRUE)
  # sort files by modification time
  details = file.info(list.files(path=datadir, pattern = pattern, full.names=TRUE))
  details = details[with(details, order(as.POSIXct(mtime))), ]
  files = rownames(details)
  lastfiles = tail(files,30)
  
  for (file in lastfiles){
    if (!exists("dataset")){
      dataset <- read.table(file, header=FALSE, sep=";")
    } else {
      temp_dataset <-read.table(file, header=FALSE, sep=";")
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }

  # column names and convert time to POSIX
  colnames(dataset) <- c("date","time","batt","temp","rh","rad","speed")
  dataset$ctime <- as.POSIXct(paste(dataset$date,dataset$time),format="%d-%m-%y %H:%M:%S", tz="Etc/GMT+1")

  # fix issues with windspeed before 28-3-2017
  for(j in c(1:length(dataset$speed))){
    if (dataset$speed[j]>5 & dataset$ctime[j]<"2017-03-28 12:00:00") dataset$speed[j] <- dataset$speed[j]/3
  }

  # fix issue with speed=0 spikes
  test<-runmean(dataset$speed,5)
  for(j in c(1:length(dataset$speed))){
    if (test[j]-dataset$speed[j]>0.6 & dataset$speed[j]<0.01) dataset$speed[j] <- test[j]
  }

  # save dataset to file and copy this file to FIELDDB\Zeeland\Meteo
  #write.csv(dataset[,c(9,3,4,5,6,7)],file=paste(i,"_alldata.csv",sep=''),row.names=FALSE)
  #file.copy(paste(i,"_alldata.csv",sep=''), "E:\\FIELDDB\\Zeeland\\Meteo\\",overwrite = TRUE)

  # cut out the last week
  dataset.sub <- subset(dataset, ctime >= as.POSIXct(Sys.Date()-7))
  rm(dataset)

  # locations
  if (i == 111) {loc<-"Vlissingen: Westerzicht"}
  if (i == 112) {loc<-"Vlissingen: Scheldestraat"}
  if (i == 108) {loc<-"Middelburg: Walplein"}
  if (i == 109) {loc<-"Middelburg: Eisenhowerlaan"}
  if (i == 110) {loc<-"Heinkenszand: Roofvogelstraat"}
  print(loc)

  # calc UTCI
  doy <- strptime(dataset.sub$date, format="%d-%m-%Y")$yday + 1
  yr <- strptime(dataset.sub$date, format="%d-%m-%Y")$year+1900
  yr <- yr+2000
  hour <- strptime(dataset.sub$time, format="%H:%M:%S")$hour
  
  if (nrow(dataset.sub) > 0) {
	  for(j in c(1:length(dataset.sub$temp))){  # calculate tglobe
		x <- calc_solar_parameters(yr[j], doy[j], hour[j], lat, lon, dataset.sub$rad[j])
		cza <- x[1]
		fdir <- x[2]
		dataset.sub$tglobe[j] <- calc_tglobe(dataset.sub$temp[j]+273.15,dataset.sub$rh[j]/100,pres,dataset.sub$speed[j],dataset.sub$rad[j],fdir,cza)
	  }
	  for(j in c(1:length(dataset.sub$temp))){   # calculate Tmrt following Thorsson 2007
		dataset.sub$tmrt[j] <- calc_tmrt(dataset.sub$tglobe[j],dataset.sub$temp[j],dataset.sub$speed[j])
	  }
	  for(j in c(1:length(dataset.sub$temp))){  # calculate UTCI following UTCI_a002.f90 (www.utci.org)
		dataset.sub$utci[j] <- calc_utci(dataset.sub$temp[j],dataset.sub$rh[j],dataset.sub$tmrt[j],max(dataset.sub$speed[j],0.5))
	  }
  }
  
  # make graphs
  png(paste(graphsdir,i,'_timeseries.png',sep=""), height=790, width=790)
  
  if (nrow(dataset.sub) == 0 || i %in% inmaintenance) {
	#dataset.sub$tglobe <- numeric()
	#dataset.sub$tmrt <- numeric()
	#dataset.sub$utci <- numeric()
	print("creating empty plots")
	if (i %in% inmaintenance ) {
	  nodatalabel = "In onderhoud"
	} else {
	  nodatalabel = "Geen gegevens ontvangen"
	}
	p1 <- ggplot() + 
		annotate("text", x = 4, y = 25, size=8, label = nodatalabel) + 
		theme_void()
	p2 <- ggplot() + 
		annotate("text", x = 4, y = 25, size=8, label = nodatalabel) + 
		theme_void()
	p3 <- ggplot() + 
		annotate("text", x = 4, y = 25, size=8, label = nodatalabel) + 
		theme_void()
	p4 <- ggplot() + 
		annotate("text", x = 4, y = 25, size=8, label = nodatalabel) + 
		theme_void()
	p5 <- ggplot() + 
		annotate("text", x = 4, y = 25, size=8, label = nodatalabel) + 
		theme_void()  
  } else {
  #if (nrow(dataset.sub) > 0) {
  p1 <- ggplot(dataset.sub, aes(x=ctime, y=as.numeric(temp))) +
        geom_point(colour="#ec2c33",size=1.5) +
        theme_bw() +
        theme(text=element_text(size=13,colour="black")) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
        theme(axis.ticks=element_line(size=1.5)) +
        ylab("Temperatuur (graden C)") +
        ggtitle(loc) 
  p2 <- ggplot(dataset.sub, aes(x=ctime, y=as.numeric(rh))) +
        geom_point(colour="#128b4a",size=1.5) +
        theme_bw() +
        theme(text=element_text(size=13,colour="black")) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
        theme(axis.ticks=element_line(size=1.5)) +
        ylab("Luchtvochtigheid (%)")
  p3 <- ggplot(dataset.sub, aes(x=ctime, y=as.numeric(rad))) +
        geom_point(colour="#165da7",size=1.5) +
        theme_bw() +
        theme(text=element_text(size=13,colour="black")) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
        theme(axis.ticks=element_line(size=1.5)) +
        ylab("Straling (W/m2)")
  p4 <- ggplot(dataset.sub, aes(x=ctime, y=as.numeric(speed))) +
        geom_point(colour="#f37b2d",size=1.5) +
        theme_bw() +
        theme(text=element_text(size=13,colour="black")) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
        theme(axis.ticks=element_line(size=1.5)) +
        ylab("Windsnelheid (m/s)")
  p5 <- ggplot(dataset.sub, aes(x=ctime, y=as.numeric(utci))) +
        annotate("rect", xmin=as.POSIXct(min(dataset.sub$ctime)), xmax=as.POSIXct(max(dataset.sub$ctime)), ymin=-Inf, ymax=9, fill="#AAEEFF", alpha=0.6) +
        annotate("rect", xmin=as.POSIXct(min(dataset.sub$ctime)), xmax=as.POSIXct(max(dataset.sub$ctime)), ymin=9, ymax=26, fill="#87DEAA", alpha=0.6) +
        annotate("rect", xmin=as.POSIXct(min(dataset.sub$ctime)), xmax=as.POSIXct(max(dataset.sub$ctime)), ymin=26, ymax=32, fill="#F4D7D7", alpha=0.6) +
        annotate("rect", xmin=as.POSIXct(min(dataset.sub$ctime)), xmax=as.POSIXct(max(dataset.sub$ctime)), ymin=32, ymax=Inf, fill="#E9AFAF", alpha=0.6) +
        geom_point(colour='black',size=1.5) +
        scale_x_datetime(breaks=date_breaks(width="day")) +
        theme_bw() +
        theme(text=element_text(size=13,colour="black")) +
        theme(axis.ticks=element_line(size=1.5)) +
        xlab("Datum") +
        ylab("Gevoelstemperatuur (graden C)")
  } 
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g4 <- ggplotGrob(p4)
  g5 <- ggplotGrob(p5)
  g <- rbind(g1, g2, g3, g4, g5, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths, g5$widths) # use the largest widths
  grid.draw(g)

  # close and clean up
  dev.off()
  rm(dataset.sub)
  
  # upload to climatexchange web server
  pngname <- paste(i,'_timeseries.png',sep="")
  uploadname <- paste("ftp://username:password@ftpserver.details.nl/sites/graphs/", pngname, sep="")
  print(paste("uploading to", uploadname))
  ftpUpload(paste(graphsdir,pngname,sep=""), uploadname)

}

# =================================================================
# get current data for all 5 dataloggers and make comparison graphs

print("")
print("getting current data...")
# combine all .log files into 1 dataframe
for (i in 108:112){

  datadir <- paste(wd,"i",i,"/",sep="")
  # locations
  if (i == 111) {loc<-"Vlissingen: Westerzicht"}
  if (i == 112) {loc<-"Vlissingen: Scheldestraat"}
  if (i == 108) {loc<-"Middelburg: Walplein"}
  if (i == 109) {loc<-"Middelburg: Eisenhowerlaan"}
  if (i == 110) {loc<-"Heinkenszand: Roofvogelstraat"} 
  print(loc)  

  file <- paste(datadir,i,"_current.log",sep="")
  if (!file.exists(file)) {
    print(paste("file does not exist:", file))
    next
  }
  
  if (!exists("dataset")){
    dataset <- read.table(file, header=FALSE, sep=";", nrows = 1, blank.lines.skip = TRUE)
	dataset$loc <- loc
  } else {
    temp_dataset <-read.table(file, header=FALSE, sep=";", nrows = 1, blank.lines.skip = TRUE)
	temp_dataset$loc <- loc
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

# column names and POSIX date/time
colnames(dataset) <- c("date","time","batt","temp","rh","rad","speed", "unknown", "loc")
#dataset$loc <- c("Middelburg\nWalplein","Middelburg\nEisenhowerlaan","Heinkenszand\nRoofvogelstraat","Vlissingen\nWesterzicht","Vlissingen\nScheldestraat")
dataset$ctime <- as.POSIXct(paste(dataset$date,dataset$time),format="%d-%m-%y %H:%M:%S", tz="Etc/GMT+1")
dataset <- dataset[order(dataset$ctime,decreasing=TRUE),]

# locations
# make graph collection date/time
png(paste(graphsdir,"current_time.png",sep=""), height=300, width=790)
ggplot(dataset, aes(x=loc, y=ctime)) +
  geom_point(stat = "identity", fill="black", colour='black',,size=5) +
  coord_flip() +
  scale_y_datetime(limits=c(min(dataset$ctime),max(dataset$ctime)),labels = date_format("%d/%m %H:%M")) +
  ylab("Datum/tijd") +
  xlab("Laatste comms per locatie") +
  theme_bw() +
  theme(text=element_text(size=13,colour="black")) +
  theme(axis.ticks=element_line(size=2))
dev.off()

# make graph t
png(paste(graphsdir,"current_temp.png",sep=""), height=300, width=600)
ggplot(dataset, aes(x=loc, y=as.numeric(temp))) +
  geom_bar(stat = "identity", fill="#ec2c33", colour="#ec2c33") +
  geom_text(aes(label=strftime(dataset$ctime,format="%H:%M",tz="Etc/GMT+0"),y=as.numeric(min(dataset$temp,na.rm=TRUE))-0.5),color="black",size=4,position=position_dodge(.9) ) +
  theme_bw() +
  theme(text=element_text(size=13,colour="black")) +
  theme(axis.ticks=element_line(size=2)) +
  xlab("Locatie") +
  ylab("Luchttemperatuur (graden C)") +
  coord_cartesian(ylim=c(as.numeric(min(dataset$temp,na.rm=TRUE))-0.5,as.numeric(max(dataset$temp,na.rm=TRUE))+0.5))
dev.off()

# make graph rh
png(paste(graphsdir,"current_rh.png",sep=""), height=300, width=600)
ggplot(dataset, aes(x=loc, y=as.numeric(rh))) +
  geom_bar(stat = "identity", fill="#128b4a", colour="#128b4a") +
  geom_text(aes(label=strftime(dataset$ctime,format="%H:%M",tz="Etc/GMT+0"),y=as.numeric(min(dataset$rh,na.rm=TRUE))-3),color="black",size=4,position=position_dodge(.9) ) +
  theme_bw() +
  theme(text=element_text(size=13,colour="black")) +
  theme(axis.ticks=element_line(size=2)) +
  xlab("Locatie") +
  ylab("Luchtvochtigheid (%)") +
  coord_cartesian(ylim=c(as.numeric(min(dataset$rh,na.rm=TRUE))-3,as.numeric(max(dataset$rh,na.rm=TRUE))+3))
dev.off()

# make graph rad
png(paste(graphsdir,"current_rad.png",sep=""), height=300, width=600)
ggplot(dataset, aes(x=loc, y=as.numeric(rad))) +
  geom_bar(stat = "identity", fill="#165da7", colour="#165da7") +
  geom_text(aes(label=strftime(dataset$ctime,format="%H:%M",tz="Etc/GMT+0"),y=as.numeric(min(dataset$rad,na.rm=TRUE))-10),color="black",size=4,position=position_dodge(.9) ) +
  theme_bw() +
  theme(text=element_text(size=13,colour="black")) +
  theme(axis.ticks=element_line(size=2)) +
  xlab("Locatie") +
  ylab("Straling (W/m2)") +
  coord_cartesian(ylim=c(as.numeric(min(dataset$rad,na.rm=TRUE))-10,as.numeric(max(dataset$rad,na.rm=TRUE))+10))
dev.off()

# make graph u
png(paste(graphsdir,"current_wind.png",sep=""), height=300, width=600)
ggplot(dataset, aes(x=loc, y=as.numeric(speed))) +
  geom_bar(stat = "identity", fill="#f37b2d", colour="#f37b2d") +
  geom_text(aes(label=strftime(dataset$ctime,format="%H:%M",tz="Etc/GMT+0"),y=0.05),color="black",size=4,position=position_dodge(.9) ) +
  theme_bw() +
  theme(text=element_text(size=13,colour="black")) +
  theme(axis.ticks=element_line(size=2)) +
  xlab("Locatie") +
  ylab("Windsnelheid (m/s)") +
  coord_cartesian(ylim=c(0,as.numeric(max(dataset$speed,na.rm=TRUE))+0.5))
dev.off()

# make graph utci
doy <- strptime(dataset$date, format="%d-%m-%Y")$yday + 1
yr <- strptime(dataset$date, format="%d-%m-%Y")$year+1900
yr <- yr+2000
hour <- strptime(dataset$time, format="%H:%M:%S")$hour
for(i in c(1:length(dataset$temp))){  # calculate tglobe
  x <- calc_solar_parameters(yr[i], doy[i], hour[i], lat, lon, dataset$rad[i])
  cza <- x[1]
  fdir <- x[2]
  dataset$tglobe[i] <- calc_tglobe(dataset$temp[i]+273.15,dataset$rh[i]/100,pres,dataset$speed[i],dataset$rad[i],fdir,cza)
}
for(i in c(1:length(dataset$temp))){   # calculate Tmrt following Thorsson 2007
  dataset$tmrt[i] <- calc_tmrt(dataset$tglobe[i],dataset$temp[i],dataset$speed[i])
}
for(i in c(1:length(dataset$temp))){  # calculate UTCI following UTCI_a002.f90 (www.utci.org)
  dataset$utci[i] <- calc_utci(dataset$temp[i],dataset$rh[i],dataset$tmrt[i],max(dataset$speed[i],0.5))
}
png(paste(graphsdir,"current_utci.png",sep=""), height=300, width=600)
ggplot(dataset, aes(x=loc, y=as.numeric(utci))) +
  geom_bar(stat = "identity", fill="#65328f", colour="#65328f") +
  geom_text(aes(label=strftime(dataset$ctime,format="%H:%M",tz="Etc/GMT+0"),y=as.numeric(min(dataset$utci,na.rm=TRUE))-0.5),color="black",size=4,position=position_dodge(.9) ) +
  theme_bw() +
  theme(text=element_text(size=13,colour="black")) +
  theme(axis.ticks=element_line(size=2)) +
  xlab("Locatie") +
  ylab("Universal Thermal Climate Index ( graden C)") +
  coord_cartesian(ylim=c(as.numeric(min(dataset$utci,na.rm=TRUE))-0.5,as.numeric(max(dataset$utci,na.rm=TRUE))+0.5))
dev.off()

# upload to climatexchange is done by Fling on lab PC

# upload to climatexchange web server
allpngs = c("current_time.png", "current_temp.png", "current_rh.png", "current_rad.png", "current_wind.png", "current_utci.png")
for (pngname in allpngs) {
  uploadname <- paste("ftp://username:password@ftpserver.details.nl/sites/graphs/", pngname, sep="")
  print(paste("uploading to", uploadname))
  ftpUpload(paste(graphsdir,pngname,sep=""), uploadname)
}
