#######
## Chamber Plots

library(shiny)
library(ggplot2)
library(stringr)
library(shinyjs)
library(lubridate)
library(shinyFiles)

options(shiny.maxRequestSize=50*1024^2) 

chamberIDs <- c("CH01", "CH02", "CH03", "CH04", "CH05", 
  "CH06","CH07", "CH08", "CH09", "CH10", "CH11")

calcPCO2 <- function(airFlowSLPM, airCO2ppm, CO2flowSCCM){
  airFracCO2 <- airCO2ppm / 1000000
  CO2flowSLPM <- CO2flowSCCM / 1000
  totalFlowSLPM <- airFlowSLPM + CO2flowSLPM
  fracCO2fromCO2stream <- CO2flowSLPM / totalFlowSLPM
  fracCO2fromAirStream <- airFlowSLPM / totalFlowSLPM * airFracCO2
  fracCO2inStream <- fracCO2fromCO2stream + fracCO2fromAirStream
  mixedGasCO2ppm <- fracCO2inStream * 1000000
  return(mixedGasCO2ppm)
}

# Define UI ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("CO2 Chambers"),
  sidebarLayout(
    sidebarPanel( width = 2,
      radioButtons("fileMode", h4("File Mode"),
                   choices = list("Select Files" = "selectFiles", 
                                  "Auto Update" = "autoUpdate"), selected = "selectFiles"),
       fileInput("files", h4("Select Input Files"), multiple = TRUE, accept = c(".lvm", ".txt")),
       shinyDirButton('folder', 'Update Files Folder', 'Please select a folder', FALSE),
       fileInput("batFile", h4("Update batchfile"), multiple = FALSE, accept = c(".bat")),
       textInput("updateInt", "Update time interval (minutes)", "10"),
       fileInput("threshFile", h4("Alarm Thresholds File"), multiple = FALSE, accept = c(".csv")),
       actionButton("startAutoUpdate", "Start auto update"),
       textInput("avgWin", "Moving average window (nObs)", "4"),
       textInput("inCO2", "Input Air stream CO2 (ppm)", "6"),
       radioButtons("plotType", h4("Plot Variable"),
                    choices = list("Temperature" = "temperature", 
                                   "Air MFC" = "airMFC", 
                                   "CO2 MFC" = "co2MFC",
                                   "CO2 Concentration" = "obsCO2ppm"), selected = "temperature"),
       checkboxGroupInput("chambers", 
                          h4("Graph Chambers"), choices = chamberIDs,
                          selected = chamberIDs),
       sliderInput("ySlider", "Y-axis Range\n(times 1000 for CO2 concentration)", 
                   min = 0, max = 20, value = c(6, 12)),
       checkboxInput("yRangeCheckbox", "Limit Graph Y-axixs Range", value = FALSE),
      radioButtons("graphDates", h4("Graph Dates"),
                   choices = list("All dates" = "allDates",
                                  "Selected Range" = "selectedDates", 
                                  "Last hour" = "lastHour",
                                  "Last day" = "lastDay",
                                  "Last week" = "lastWeek",
                                  "Last 30 days" = "last30"),
                   selected = "allDates"),
       uiOutput("dateRange"),
       downloadButton("downloadData", "Download")
    ),
    mainPanel(
      width = 10,
      # this suppresses error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      plotOutput("plot", width = "100%", height = "740px")
      #dataTableOutput("dTable")
      )
  )
)

# Define server logic ----
server <- function(input, output) {
  volumes <- getVolumes()
  
  shinyDirChoose(input, 'folder', roots = volumes, filetypes=c('', 'txt'))
  
  processFiles <- function(fileNames, filePaths){
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 1, max = length(fileNames))
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    
    progress$set(message = "Processing File", value = 0)
    
    d <- data.frame(chamber = character(), dateTime = character(),
                    temperature = numeric(), airMFC = numeric(), co2MFC = numeric(),
                    targetAir = numeric(), targetCIO2 = numeric(), obsCO2conc = numeric(),
                    stringsAsFactors = FALSE)
    for(i in 1:length(fileNames)){
      # Set the progress bar, and update the detail text.
      progress$set(value = i, detail = paste(i, " of ", length(fileNames)))
      chamberID <- word(fileNames[i], 1, 1, sep = "_")
      fileStartDateTime <- word(fileNames[i], 2, 3, sep = "_")
      fileStartDateTime <- as.POSIXct(strptime(fileStartDateTime, "%y-%m-%d_%H%M"))
      dtemp <- read.table(filePaths[i], header = FALSE, sep = "\t", skip = 22, skipNul = TRUE)
      #difference (in seconds) from start of program and start of file
      offsetTime <- dtemp$V1[1]
      dateTime <- fileStartDateTime + dtemp$V1 - offsetTime
      temperature <- dtemp$V3
      airMFC <- dtemp$V5
      airMFC[airMFC < 0] <- NA
      co2MFC <- dtemp$V7
      setAirMFC <- dtemp$V4
      setCO2MFC <- dtemp$V6
      inputAirCO2 <- as.numeric(input$inCO2)
      obsCO2ppm <- calcPCO2(airMFC, inputAirCO2, co2MFC)
      dx <- data.frame(dateTime, temperature, airMFC, co2MFC, setAirMFC, setCO2MFC, obsCO2ppm)
      dx$chamber <- chamberID
      #this next loop creates the short dataset, averaging over winlen number of records
      #take the total number of data rows, divide by winLen, and round to nearest integer.
      winLen <- as.numeric(input$avgWin)
      sLen <- as.integer(nrow(dx)/winLen)
      #create shortened data frame
      dShort <-dx[1:sLen, ]
      dStartIndex <- 1
      dEndIndex <- winLen
      for (j in 1:sLen){
        dShort[j, ]<- dx[dStartIndex, ]
        dShort[j, 2:7] <- apply(dx[dStartIndex:dEndIndex,2:7],2,mean)
        dStartIndex <- dStartIndex + winLen
        dEndIndex <- dEndIndex + winLen
      }
      d <- rbind(d, dShort)
    }
    return(d)
  } 
  
  setStatus <- function(d, thresh){
    ds <- data.frame(chamber = thresh$chamber)
    ds$status <- ""
    status[thresh$monitor == "no"] <- "Chamber not monitored"
  }
  
  observeEvent(input$fileMode, {
    if(input$fileMode == "autoUpdate"){
      shinyjs::disable("files")
      shinyjs::hide("files")
      shinyjs::enable("folder")
      shinyjs::enable("batFile")
      shinyjs::enable("updateInt")
#      shinyjs::enable("threshFile")
      shinyjs::enable("startAutoUpdate")
      shinyjs::show("folder")
      shinyjs::show("batFile")
      shinyjs::show("updateInt")
 #     shinyjs::show("threshFile")
      shinyjs::show("startAutoUpdate")
    }
    if(input$fileMode == "selectFiles"){
      shinyjs::enable("files")
      shinyjs::show("files")
      shinyjs::disable("folder")
      shinyjs::disable("batFile")
      shinyjs::disable("updateInt")
 #     shinyjs::disable("threshFile")
      shinyjs::disable("startAutoUpdate")
      shinyjs::hide("folder")
      shinyjs::hide("batFile")
      shinyjs::hide("updateInt")
 #     shinyjs::hide("threshFile")
      shinyjs::hide("startAutoUpdate")
    }
  })
  
  values <- reactiveValues(chData = NULL, dataDir = NULL, batFilePath = NULL, thresholds = NULL, 
                           updateInterval = NULL, graphMinDateTime = NULL, 
                           graphMaxDateTime = NULL, chStatus = NULL, test = NULL)
  
  observeEvent(input$files,{
    values$chData <- processFiles(input$files$name, input$files$datapath)
  })
  
  observeEvent(input$folder, {
    values$dataDir <- parseDirPath(volumes, input$folder)
  })
  
  observeEvent(input$batFile, {
    values$batFilePath <- input$batFile$datapath
  })
  
  observeEvent(input$batFile, {
    values$batFilePath <- input$batFile$datapath
  })
  
  observeEvent(input$threshFile, {
    values$thresholds <- read.csv(input$threshFile$datapath, stringsAsFactors = FALSE)
    print(values$thresholds)
  })
  
  observeEvent(input$updateInt,{
    #user input is in minutes, but reactivetimer uses milliseconds
    values$updateInterval <- as.integer(input$updateInt) * 60 * 1000
  })
  
  updateData <- function(){
    shell(values$batFilePath)
    filesInFolder <- list.files(path=values$dataDir, pattern="*.lvm", full.names=TRUE, recursive=FALSE)
    filePaths <- unlist(filesInFolder)
    fileNames <- basename(filePaths)
    values$chData <- processFiles(fileNames, filePaths)
  }
  
  observeEvent(input$startAutoUpdate,{
    timer <- reactiveTimer(values$updateInterval)
    observe({
      timer()
      updateData()
    })
  })

  observe({
   minDateTime <- min(values$chData$dateTime)
   maxDateTime <- max(values$chData$dateTime)

  if(input$graphDates == "allDates"){
    values$graphMinDateTime <- minDateTime
    values$graphMaxDateTime <- maxDateTime
  }
  if(input$graphDates == "selectedDates"){
    values$graphMinDateTime <- input$dateRng[1]
    values$graphMaxDateTime <- input$dateRng[2]
  }
  if(input$graphDates == "lastHour"){
    values$graphMinDateTime <- maxDateTime - minutes(60)
    values$graphMaxDateTime <- maxDateTime
  }
  if(input$graphDates == "lastDay"){
    values$graphMinDateTime <- maxDateTime - days(1)
    values$graphMaxDateTime <- maxDateTime
  }
  if(input$graphDates == "lastWeek"){
    values$graphMinDateTime <- maxDateTime - days(7)
    values$graphMaxDateTime <- maxDateTime
  }
  if(input$graphDates == "last30"){
    values$graphMinDateTime <- maxDateTime - days(30)
    values$graphMaxDateTime <- maxDateTime
  }
  })
  
  output$dateRange <- renderUI({
    dateRangeInput("dateRng", "Select the date range:",
                   start = as.Date(min(values$chData$dateTime)),
                   end = (as.Date(max(values$chData$dateTime)) + 1),
                   min = (as.Date(min(values$chData$dateTime))),
                  max = (as.Date(max(values$chData$dateTime)) + 1))
  })
  

  #output$testText <- renderText(values$test)

  output$plot <- renderPlot({
    
    d <- values$chData
    d <- subset(d, (chamber %in% input$chambers))
    startGraphDateTime <- values$graphMinDateTime
    endGraphDateTime <- values$graphMaxDateTime
    #add a day to make sure it gets all the way to midnight
    endGraphDateTime <- endGraphDateTime + 86400
    d <- subset(d, dateTime >= startGraphDateTime & dateTime < endGraphDateTime)
    yLimits <- c(0, 20)
    if(input$plotType == "temperature"){
      yLimits <- c(min(d$temperature), max(d$temperature))
    }
    if(input$plotType == "airMFC"){
      yLimits <- c(min(d$airMFC), max(d$airMFC))
    }
    if(input$plotType == "co2MFC"){
      yLimits <- c(min(d$co2MFC), max(d$co2MFC))
    }
    if(input$plotType == "obsCO2ppm"){
      yLimits <- c(min(d$obsCO2ppm), max(d$obsCO2ppm))
    }
    if(input$yRangeCheckbox){
      yLimits <- c(input$ySlider[1], input$ySlider[2])
      if(input$plotType == "obsCO2ppm"){
        yLimits <- c(input$ySlider[1] * 1000, input$ySlider[2] *1000)
      }
    }
    
    ggplot(d, aes_string("dateTime", input$plotType)) +
    geom_line(aes(colour = chamber)) +
      ylim(yLimits) +
      theme_bw(base_size = 24)
  })
  
  #output$dTable <- renderDataTable(values$chData) 
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ChamberData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$chData, file, row.names = FALSE)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)