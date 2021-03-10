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
modelNames <- getModelNames()
modelNameList <- list()
modelNameList[modelNames] <- modelNames

# also get actual 
modelList <- list()
modelList[modelNames] <- getModels(modelNames)


# get tuning parameters from analysis functions
# can be NULL
tuningParametersList <- getModelParameters()
tuningParameterNames <- names(tuningParametersList)
tuningParameters <- unlist(tuningParametersList)


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
                        choices = modelNameList,
                        selected = 1),

            # if there are parameters add another UI section
            uiOutput("parameter_conditional")

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("spectrumPlot"),
           
           h3("Analysis Results"),
           
           fluidRow(
               column(5,
                      br(),
                      
                      verbatimTextOutput("results")
                ),
               column(7, 
                       plotOutput("SIMCAfit", height = 300)
                )
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    # add UI portion for parameter input
    output$parameter_conditional <- renderUI({
        
        if (length(tuningParametersList) == 0) {
            # ignore section
            return()
        } else {
            tuningPanel <- tagList(
                br(),
                h4("Parameters to Use"))    
                
            for (index in seq_along(tuningParametersList) ) {
                inputLabel <- paste("parameter", index, sep = ".")
                tuningPanel <- tagAppendChild(tuningPanel,
                    textInput(inputLabel,
                              names(tuningParametersList)[index],
                              value = as.character(tuningParametersList[[index]]))
                )
            }
            return(tuningPanel)
        }        
    })
    
    
    
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
                if (substr(fileID, nchar(fileID)-3, nchar(fileID)) == ".csv") {
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
    
    
    prediction <- reactive({
        
        spectrum <- spectrumInput()
        
        if (input$allModels) {
            models <- modelNames
        } else {
            models <- input$singleModel
        }
        
        if (length(tuningParametersList) == 0) {
            results <- doAnalysis(spectrum$Spectrum, models, modelList)
        } else {
            for (index in seq_along(tuningParametersList)) {
                inputLabel <- paste("parameter", index, sep = ".")
                if (! is.null(input[[inputLabel]])) {
                    # first time around the input field does not exist yet
                    tuningParameters[index] <- input[[inputLabel]]
                }
            }
            tuningParametersList <- list(tuningParameters)
            results <- doAnalysis(spectrum$Spectrum, 
                                  models, modelList, 
                                  tuningParametersList)
        }
        
        return(results)
    })

    
    output$results <- renderPrint({
        
        results <- prediction()
        
        for (index in seq_along(results)) {
            
            modelName <- names(results)[index]
            predictions <- results[[modelName]]
            member <- predictions$member
            names(member) <- NULL
            
            cat(modelName, ":", ifelse(member, "PASS", "---"), "\n")
        }
        
    })
        
        

    output$SIMCAfit <- renderPlot({
        
        results <- prediction()
        plotModel <- "none"
        
        for (index in seq_along(results)) {
            
            modelName <- names(results)[index]
            predictions <- results[[modelName]]
            member <- predictions$member
            names(member) <- NULL
            
            if (member) {
                if (plotModel == "none") {
                    plotModel <- modelName
                } else {
                    # not a single match
                    plotModel <- "multi"
                }
            }
        }
        
        if ((plotModel != "multi") & (plotModel != "none")) {
            # single match from multi or single model
            modelPredictions <- results[[plotModel]]
            model <- modelList[[plotModel]]
            SIMCAplot <- plotSIMCA(simcaModel = model,
                                   prediction = modelPredictions)
        } else {
            if (! input$allModels) {
                # single model selected
                plotModel <- input$singleModel
                modelPredictions <- results[[plotModel]]
                model <- modelList[[plotModel]]
                SIMCAplot <- plotSIMCA(simcaModel = model,
                                       prediction = modelPredictions)
            } else {
            # no match from multi
            SIMCAplot <- ggplot() + 
                annotate("text", x = 4, y = 25, size=8, label = "No Match") + 
                theme_void()
            }
        }
        SIMCAplot <- SIMCAplot + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
        print(SIMCAplot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
