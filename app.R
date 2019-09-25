#######
## Chamber Plots

library(shiny)
library(ggplot2)
library(stringr)

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
  titlePanel("CO2 Chambers"),
  sidebarLayout(
    sidebarPanel( width = 2,
       fileInput("files", h4("File input"), multiple = TRUE, accept = c(".lvm", ".txt")),
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
       dateRangeInput("dates", h4("Date range"), start = "2018-03-13"),
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
  values <- reactiveValues(chData = NULL, test = NULL)

  values$chData <- observeEvent(input$files, {
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 1, max = length(input$files$name))
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing File", value = 0)
    
    d <- data.frame(chamber = character(), dateTime = character(),
                    temperature = numeric(), airMFC = numeric(), co2MFC = numeric(),
                    targetAir = numeric(), targetCIO2 = numeric(), obsCO2conc = numeric(),
                    stringsAsFactors = FALSE)
    for(i in 1:length(input$files$name)){
      # Set the progress bar, and update the detail text.
      progress$set(value = i, detail = paste(i, " of ", length(input$files$name)))
      chamberID <- word(input$files$name[i], 1, 1, sep = "_")
      #dTime <- read.table(input$files$datapath[i], header = FALSE, sep = "\t", skip = 9, nrows = 2)
      #startDateTime <- paste(dTime$V2[1], dTime$V2[2], sep = "_")
      #startDateTime <- as.POSIXct(strptime(startDateTime, "%Y/%m/%d_%H:%M:%OS"))
      fileStartDateTime <- word(input$files$name[i], 2, 3, sep = "_")
      fileStartDateTime <- as.POSIXct(strptime(fileStartDateTime, "%y-%m-%d_%H%M"))
      #values$test <- startDateTime
      dtemp <- read.table(input$files$datapath[i], header = FALSE, sep = "\t", skip = 22, skipNul = TRUE)
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
      #output$dTable <- renderDataTable({d})
    }
    values$chData <- d
  })
  #output$testText <- renderText(values$test)

  output$plot <- renderPlot({
    
    d <- values$chData
    d <- subset(d, (chamber %in% input$chambers))
    startGraphDateTime <- as.POSIXct(as.Date(input$dates[1]))
    endGraphDateTime <- as.POSIXct(as.Date(input$dates[2]))
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