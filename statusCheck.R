library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(cowplot)

#function to make chamber status plot
statusPlot <- function(chData, threshData, statusTimeOffset){
  #Set internal variables
  d <- chData
  thresh <- threshData
  offsetTime <- statusTimeOffset
  # make dataframe of the most most recent timestamp row from each chamber
  dlast <- d %>% group_by(chamber) %>% top_n(1, dateTime)
  #extra chamber number from the labview file
  dlast$chamber <-  as.numeric(substr(dlast$chamber, 3,4))
  #merge the most recent laview data and the thresholds into one file
  dt <- merge(dlast, thresh, by = "chamber", all.y = TRUE)
  
  #set status with details. the detail text is not used, but helps clarity of code
  dt$monitoredStatus <- "Monitored"
  dt$monitoredStatus[dt$monitor == "no"] <- "Not Monitored"
  dt$temperatureStatus <- "OK"
  dt$temperatureStatus[dt$temperature < dt$tempLow] <- "Temperature Below Threshold"
  dt$temperatureStatus[dt$temperature > dt$tempHigh] <- "Temperature Above Threshold"
  dt$temperatureStatus[is.na(dt$temperature)] <- "Temperature Data Missing"
  dt$airStatus <- "OK"
  dt$airStatus[dt$airMFC < dt$airLow] <- "Air MFC output below threshold"
  dt$airStatus[dt$airMFC > dt$airHigh] <- "Air MFC output above thre#shold"
  dt$airStatus[is.na(dt$airMFC)] <- "Air MFC Data Missing"
  dt$co2Status <- "OK"
  dt$co2Status[dt$co2MFC < dt$co2Low] <- "CO2 MFC output below threshold"
  dt$co2Status[dt$co2MFC > dt$co2High] <- "CO2 MFC output above threshold"
  dt$co2Status[is.na(dt$co2MFC)] <- "CO2 MFC Data Missing"
  checkTime <- Sys.time() - minutes(offsetTime)
  dt$newDataStatus <- "OK"
  dt$newDataStatus[dt$dateTime < checkTime] <- "No current data from chamber"
  
  #set status as "OK" vs "Problem" for graphing
  dt$tStatus <- "OK"
  dt$tStatus[dt$temperatureStatus != "OK"] <- "Problem"
  dt$aStatus <- "OK"
  dt$aStatus[dt$airStatus != "OK"] <- "Problem"
  dt$cStatus <- "OK"
  dt$cStatus[dt$co2Status != "OK"] <- "Problem"
  dt$nStatus <- "OK"
  dt$nStatus[dt$newDataStatus != "OK"] <- "Problem"
  
  #subset to only moniotored chambers
  dt <- dt[dt$monitoredStatus == "Monitored",]
  
  #convert to long-skinny data frame for graphing
  dg <- gather(dt, alarmComp, alarmStatus, tStatus, aStatus, cStatus, nStatus)
  
  #set text for each of the cells of the graph
  #the "updated" cells show the most recent dateTime from labview
  #Temperaure, air and CO2 cells show the target in parantheses and the most recent labview value after
  dg$graphText <- ""
  dg$graphText[dg$alarmComp == "tStatus"] <- paste("(",dg$tempTarget[dg$alarmComp == "tStatus"], ")\n",
                                                   round(dg$temperature[dg$alarmComp == "tStatus"],2),
                                                   sep="")
                                                   
  dg$graphText[dg$alarmComp == "aStatus"] <- paste("(",dg$airTarget[dg$alarmComp == "aStatus"],")\n",
                                                   round(dg$airMFC[dg$alarmComp == "aStatus"],2),
                                                   sep = "")
  dg$graphText[dg$alarmComp == "cStatus"] <- paste("(",dg$co2Target[dg$alarmComp == "cStatus"], ")\n",
                                                   round(dg$co2MFC[dg$alarmComp == "cStatus"],2),
                                                   sep = "")
  dg$graphText[dg$alarmComp == "nStatus"] <- paste(format(dg$dateTime[dg$alarmComp == "nStatus"], "%b_%d"),
                                                   "\n", 
                                                   format(dg$dateTime[dg$alarmComp == "nStatus"], "%H:%M"),
                                                   sep = "")             
  
  #convert chamber to factor
  dg$chamber <- factor(dg$chamber)
  #set the order of the alarms for the graph
  dg$alarmComp <- factor(dg$alarmComp, levels = c("nStatus", "tStatus", "aStatus", "cStatus"))
  
  # color palette is classic green=OK red=problem
  colors <- c("green", "red")
  #the plot
  p <- ggplot(dg, aes(x = alarmComp, y = chamber)) +
    geom_tile(aes(fill = alarmStatus), color = "gray") +
    scale_fill_manual(values=colors) +
    geom_text(aes(label = graphText))+
    ylab("Chamber")+
    scale_y_discrete(limits = rev(levels(dg$chamber))) +
    scale_x_discrete(labels=c("Recent Update", "Temperature", "Air MFC", "CO2 MFC"))+
    theme(axis.title.x = element_blank(), legend.position="none",
          text = element_text(size=20),axis.text.x = element_text(angle = 90)) 
  
  return(p)
}

setwd("C:\\Users\\paul.mcelhany\\Downloads")
chD <- read.csv("ChamberData.csv",stringsAsFactors = FALSE)
chD$dateTime <- as.POSIXct(chD$dateTime)
threshD <- read.csv("chamberThresholds.csv",stringsAsFactors = FALSE)
threshD2 <- read.csv("chamberThresholds2.csv",stringsAsFactors = FALSE)
offsetT <- 15

p1 <- statusPlot(chD, threshD, offsetT)
p2 <- statusPlot(chD, threshD2, offsetT)


plot_grid(p1,p2, nrow = 1, rel_widths = c(2, 1))



