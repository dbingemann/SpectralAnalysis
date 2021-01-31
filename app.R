#
# Validation App 
#
#

library(shiny)

source("Shiny Template Settings Include File.R")
source("helperFunctions.R")


source("analysis.R")


# get Enlighten folder
currentFile <- getCurrentFile()


# get model names from analysis functions
modelList <- list()
modelNames <- getModelNames()
modelList[modelNames] <- modelNames


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Logo
    img(src = "Wasatch_Photonics_gradient_logo_1000px.png",
        alt = "Wasatch Photonics",
        width = 200,
        align = "right",
        style = "margin:10px"),
    
    # Application title
    h1("Application of Spectral Model"),
    
    br(),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("File to Analyze"),
            
            # current file
            h5(strong("Watch Current Enlighten Folder?")),
            uiOutput("folderCheckBox"),
            # see blow in server for details to get folder to update if needed
#            checkboxInput(inputId = "watchCurrent", 
#                          paste("Folder:", currentFile$folderName), 
#                          value=TRUE),

            
            # load a file
            fileInput(inputId = "loadSpectrum", 
                      "Select Enlighten CSV File to Load", 
                      multiple = FALSE,
                      accept = c(".csv", "text/csv", "text/comma-separated-values")),
            
            br(),
            
            h4("Model to Apply"),         
            
            # all models
            checkboxInput(inputId = "allModels", 
                          "Apply All Models", value = FALSE),
            
            # single model
            selectInput(inputId = "singleModel", 
                        h5("Select Single Model to Apply"), 
                        choices = modelList,
                        selected = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("spectrumPlot"),
           
           h4("Analysis Results"),
           
           verbatimTextOutput("results")
           
         )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # periodically poll file folders
    pollingDelay <- 1000 # ms
    currentFileInfo <- reactivePoll(pollingDelay, session,
                                checkFunc = function() {
                                    fileInfo <- getCurrentFile()
                                    return(paste(fileInfo$folderName,
                                                 fileInfo$fileName, sep = "/"))
                                },
                                valueFunc = function() {
                                    fileInfo <- getCurrentFile()
                                    return(fileInfo) 
                                })

    output$folderCheckBox <- renderUI({
        currentFile <- currentFileInfo()
        folderName <- currentFile$folderName
        checkboxInput(inputId = "watchCurrent", 
                      paste("Folder:", folderName), 
                      value=TRUE)        
    })
    
    spectrumInput <- reactive({    
        
        updateFileInfo <- FALSE
        if (is.null(input$watchCurrent)) {
            # not yet ready, call file info
            updateFileInfo <- TRUE
        } else {
            # exists, check what it says
            if (input$watchCurrent) {
                updateFileInfo <- TRUE
            }
        }
        
        if (updateFileInfo) {
            # load current file - this is reactive and is regularly polled
            currentFile <- currentFileInfo()
            fileID <- currentFile$fileName
            spectraInput <- enlightenSpectra(fileName = fileID,
                                             filePath = currentFile$folderPath)
            
        } else {
            # use manually selected file
            if (! is.null(input$loadSpectrum)) {
                # there is a file
                loadSpectrum <- input$loadSpectrum
                fileID <- loadSpectrum$name
                if (substr(fileID, nchar(fileID)-4, nchar(fileID)) == ".csv") {
                    pathName <- loadSpectrum$datapath
                    spectraInput <- enlightenSpectra(fileName = basename(pathName),
                                                     filePath = dirname(pathName))
                } else {
                    # random walk as made up filler
                    fileID <- "Incorrect File Type"
                    spectraInput <- data.frame(x = seq(100),
                                               y = cumsum(rnorm(100)))
                }
            } else {
                # random walk as made up filler
                fileID <- "No File Provided"
                spectraInput <- data.frame(x = seq(100),
                                           y = cumsum(rnorm(100)))
            }
        }
        # say which variable to plot
        if (ncol(spectraInput) == 1) {
            yvar <- names(spectraInput)[1]
            spectraInput$Index <- seq(nrow(spectraInput))
            xvar <- "Index"
        }
        if (ncol(spectraInput) == 2) {
            xvar <- names(spectraInput)[1]
            yvar <- names(spectraInput)[2]
        } else {
            # pick in this order
            xNames <- c("Wavenumber", "Wavelength", "Pixel")
            yNames <- c("Processed", "Raw")
            xMatch <- sapply(xNames, function(name) name %in% names(spectraInput))
            yMatch <- sapply(yNames, function(name) name %in% names(spectraInput))
            xvar <- names(xMatch)[xMatch][1]
            yvar <- names(yMatch)[yMatch][1]
        }
        return(list(FileName = fileID, Spectrum = spectraInput,
                    x = xvar, y = yvar))
    })
    
    
    output$spectrumPlot <- renderPlot({
        
        spectrum <- spectrumInput()
        
        # draw the histogram with the specified number of bins
        ggplot(spectrum$Spectrum, aes_string(x=spectrum$x, y=spectrum$y)) + 
            geom_line(col = WasatchGreen) + 
            WasatchTheme +
            ggtitle(spectrum$FileName) +
            xlab(spectrum$x) +
            ylab(spectrum$y) + 
            geom_hline(yintercept = 0, color = plotGrey, alpha = 0.4)
    })
    

    output$results <- renderPrint({

        spectrum <- spectrumInput()
        
        if (input$allModels) {
            models <- modelNames
        } else {
            models <- input$singleModel
        }
        results <- doAnalysis(spectrum$Spectrum, models)
    

        for (index in seq_along(results)) {
            cat(names(results)[index], ":", results[[index]])
            cat("\n")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
