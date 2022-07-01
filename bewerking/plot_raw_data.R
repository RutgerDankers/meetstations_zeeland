# script for plotting raw station data

# libraries
#.libPaths("D:/UserData/R/win-library/3.3")
#library (RCurl)
library (ggplot2)
library (scales)
library (gtable)
library (grid)
library(caTools)

# get the functions for calculating UTCI
# this assumes these are located in the current working directory
#source("function_tmrt.r")
#source("function_utci.r")
#source("functions_wbgt.r")


# list of variable labels for plotting, and variable to plot
labels <- list(temp = "Temperatuur (graden C)",
               rh = "Luchtvochtigheid (%)",
               rad = "Straling (W/m2)",
               speed = "Windsnelheid (m/s)",
               utci = "Gevoelstemperatuur (graden C)")
myvar <- 'rh' #'temp'

# year to plot, and date range for time axis
year <- 2021
#range <-  c(as.Date("2021-01-01", "%Y-%m-%d"), as.Date("2021-10-31", "%Y-%m-%d"))
lims <- as.POSIXct(strptime(c("2021-01-01 00:00", 
                            "2021-10-31 23:59"), 
                             format = "%Y-%m-%d %H:%M"), 
                 tz = "Etc/GMT+1")

# list with names of all the locations
locnames = list(i108 = "Middelburg: Walplein",
            i109 = "Middelburg: Eisenhowerlaan",
            i110 = "Heinkenszand: Roofvogelstraat",
            i111 = "Vlissingen: Westerzicht",
            i112 = "Vlissingen: Scheldestraat")

# working directory
#rm(list = ls())
wd <- paste("/home/danke010/scratch/Zeeland/", year, "/", sep="")
# directory for graphs
graphsdir <- paste(wd,"graphs/",sep="")
if (!dir.exists(graphsdir)) {dir.create(graphsdir)}


# colours
#ec2c33
#128b4a
#165da7
#f37b2d
#65328f


# function for reading station data from file
# this file has been created when fetching the data from ftp - see ftpzeeland_vpd.py
read_station_data <- function(csvname) {
  # read in the csv file with all the data
  dataset <- read.table(csvname, header=FALSE, sep=",")

  # column names and convert time to POSIX
  colnames(dataset) <- c("datetime","batt","temp","rh","rad","speed")
  dataset$ctime <- as.POSIXct(dataset$datetime,format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT+1")

  return(dataset)

}

# read in the data for all the stations
i=108
datadir <- paste(wd,"Data ",i,"/",sep="")
csvname <- paste(datadir,"Alldata_",year,"_",i,".csv",sep="")
data108 <- read_station_data(csvname)

i=109
datadir <- paste(wd,"Data ",i,"/",sep="")
csvname <- paste(datadir,"Alldata_",year,"_",i,".csv",sep="")
data109 <- read_station_data(csvname)

i=110
datadir <- paste(wd,"Data ",i,"/",sep="")
csvname <- paste(datadir,"Alldata_",year,"_",i,".csv",sep="")
data110 <- read_station_data(csvname)

i=111
datadir <- paste(wd,"Data ",i,"/",sep="")
csvname <- paste(datadir,"Alldata_",year,"_",i,".csv",sep="")
data111 <- read_station_data(csvname)

i=112
datadir <- paste(wd,"Data ",i,"/",sep="")
csvname <- paste(datadir,"Alldata_",year,"_",i,".csv",sep="")
data112 <- read_station_data(csvname)



# make graphs
png(paste(graphsdir,myvar,'_rawdata_',year,'.png',sep=""), height=800, width=1200)
# y=as.numeric(data108[[myvar])))  

mytitle <- strsplit(labels[[myvar]], " ")[[1]]

p1 <- ggplot(data108, aes(x=ctime, y=data108[[myvar]])) +
    geom_point(colour="#ec2c33",size=1.5) +
    theme_bw() +
    theme(text=element_text(size=13,colour="black")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.ticks=element_line(size=1.5)) +
    theme(plot.subtitle = element_text(size = 12)) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) + 
    ylab(" ") +
    scale_x_datetime(limits = lims, breaks=date_breaks(width="month")) + 
    ggtitle(mytitle[1], subtitle=locnames$i108) 
p2 <- ggplot(data109, aes(x=ctime, y=data109[[myvar]])) +
    geom_point(colour="#128b4a",size=1.5) +
    theme_bw() +
    theme(text=element_text(size=13,colour="black")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.ticks=element_line(size=1.5)) +
    theme(plot.title = element_text(size = 12)) +
    scale_x_datetime(limits = lims, breaks=date_breaks(width="month")) + 
    ylab(" ") +
    ggtitle(locnames$i109) 
p3 <- ggplot(data110, aes(x=ctime, y=data110[[myvar]])) +
    geom_point(colour="#165da7",size=1.5) +
    theme_bw() +
    theme(text=element_text(size=13,colour="black")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.ticks=element_line(size=1.5)) +
    theme(plot.title = element_text(size = 12)) +
    scale_x_datetime(limits = lims, breaks=date_breaks(width="month")) + 
    ylab(labels[[myvar]]) + 
    ggtitle(locnames$i110) 
p4 <- ggplot(data111, aes(x=ctime, y=data111[[myvar]])) +
    geom_point(colour="#f37b2d",size=1.5) +
    theme_bw() +
    theme(text=element_text(size=13,colour="black")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.ticks=element_line(size=1.5)) +
    theme(plot.title = element_text(size = 12)) +
    scale_x_datetime(limits = lims, breaks=date_breaks(width="month")) + 
    ylab(" ") +
    ggtitle(locnames$i111) 
p5 <- ggplot(data112, aes(x=ctime, y=data112[[myvar]])) +
    #annotate("rect", xmin=as.POSIXct(min(dataset.sub$ctime)), xmax=as.POSIXct(max(dataset.sub$ctime)), ymin=-Inf, ymax=9, fill="#AAEEFF", alpha=0.6) +
    #annotate("rect", xmin=as.POSIXct(min(dataset.sub$ctime)), xmax=as.POSIXct(max(dataset.sub$ctime)), ymin=9, ymax=26, fill="#87DEAA", alpha=0.6) +
    #annotate("rect", xmin=as.POSIXct(min(dataset.sub$ctime)), xmax=as.POSIXct(max(dataset.sub$ctime)), ymin=26, ymax=32, fill="#F4D7D7", alpha=0.6) +
    #annotate("rect", xmin=as.POSIXct(min(dataset.sub$ctime)), xmax=as.POSIXct(max(dataset.sub$ctime)), ymin=32, ymax=Inf, fill="#E9AFAF", alpha=0.6) +
    geom_point(colour='black',size=1.5) +
    #scale_x_datetime(breaks=date_breaks(width="day")) +
    scale_x_datetime(limits = lims, breaks=date_breaks(width="month"), date_labels = "%d/%m") + 
    theme_bw() +
    theme(text=element_text(size=13,colour="black")) +
    theme(axis.ticks=element_line(size=1.5)) +
    theme(plot.title = element_text(size = 12)) +
    xlab(paste("Datum (",year,")", sep="")) +
    ylab(" ") +
    ggtitle(locnames$i112) 

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
