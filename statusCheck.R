
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

setwd("/Users/paul.mcelhany/Downloads")
d <- read.csv("ChamberData (6).csv",stringsAsFactors = FALSE)
thresh <- read.csv("chamberThresholds.csv",stringsAsFactors = FALSE)

d$dateTime <- as.POSIXct(d$dateTime)

dlast <- d %>% group_by(chamber) %>% top_n(1, dateTime)
dlast$chamber <-  as.numeric(substr(dlast$chamber, 3,4))
dt <- merge(dlast, thresh, by = "chamber", all.y = TRUE)

dt$monitoredStatus <- NA
dt$monitoredStatus[dt$monitor == "no"] <- "Not Monitored"
dt$temperatureStatus <- NA
dt$temperatureStatus[dt$temperature < dt$tempLow] <- "Temperature Below Threshold"
dt$temperatureStatus[dt$temperature > dt$tempHigh] <- "Temperature Above Threshold"
dt$airStatus <- NA
dt$airStatus[dt$airMFC < dt$airLow] <- "Air MFC output below threshold"
dt$airStatus[dt$airMFC > dt$airHigh] <- "Air MFC output above threshold"
dt$co2Status <- NA
dt$co2Status[dt$co2MFC < dt$co2Low] <- "CO2 MFC output below threshold"
dt$co2Status[dt$co2MFC > dt$co2High] <- "CO2 MFC output above threshold"
currrentDT <- Sys.time()
offsetTime <- 15
checkTime <- currrentDT - minutes(offsetTime)
dt$newDataStatus <- NA
dt$newDataStatus[dt$dateTime < checkTime] <- "No current data from chamber"
dt$status <-  unite(data = dt, col = "status", monitoredStatus, newDataStatus, temperatureStatus, airStatus, co2Status, sep = " & ", na.rm = TRUE)
dt$status









