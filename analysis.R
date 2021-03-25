#
#
# analysis.R
#
# provide functions needed for Spectral Analysis App
#
# 1. getModelNames() 
#   return Names of all the models that can be chosen
#   example: return( c("Model 1", "Model 2") )
#   can be a single model name
#   these names will be used when the analysis itself is called
#
#
# 2. getModelParameters()
#   return the names and initial values of the parameters 
#   that the user can set for the analysis as key/value pairs
#   example return( list(Threshold = 0.95, Dimensions = 3) )
#   all parameters are numeric (at this point)
#   the interface will provide a numeric input field for every name
#   can be NULL, then the "Parameter Input" section is dropped from the UI
#
#
# 3. doAnalysis(spectrumInput, models, parameters)
#   Input: 
#   you will get a single spectrum read from Enlighten as .csv 
#   in a data frame. Possible columns are (depending on your file): 
#       Pixel, Wavelength, Wavenumber, Raw, Dark, Processed, ...
#
#   the function will also receive an array of all model names to be applied
#
#   as well as a key/value list with the paremeters to be used in the analysis.
#   the keys in the parameter list are identical with the paameter names
#   provided by the function getModelParameters()
#
#   Output: 
#   return key/value pairs from your analysis in a list
#   the key/value pairs will be printed on separate rows
#   example: return( list(Result = "Pass", Confidence = "97.2%", Matches = 5) )
#
#
#

source("SIMCAfunctions.R")

# read model names
getModelNames <- function() {
    # can be a single model name
    modelFileNames <- dir(path = "./Models")
    modelNames <- str_remove(modelFileNames, "\\.[a-zA-Z0-9]+$")
    return( modelNames )
}


# load all models
getModels <- function(modelNames) {
    models <- list()
    modelPath <- "./Models"
    for (modelName in modelNames) {
        fileName <- paste0(modelPath, "/", modelName, ".Rdat")
        models[[modelName]] <- loadSIMCA(fileName)
    }
    return(models)
}


# tuning parameters
getModelParameters <- function() {
    # can be NULL if adjustment panel is not desired
    return(NULL)
#    return( list(Threshold = 0.95) )
    
    # call global parameter function in SIMCA routines
#    simcaParameters <- simcaParameters()
#    return( list(Threshold = 1 - simcaParameters$alphaLevel) )
    
#    return( list(Threshold = 0.95, Dimension = 2) )
}


doAnalysis <- function(spectrumInput = NULL, 
                       modelNames = NULL,   # names only
                       modelList = NULL,       # actual models
                       tuningParametersList = NULL) {
    
    if (is.null(spectrumInput)) {
        returnList <- list(Result = "No Data File")
    } else {
        if (is.null(modelNames)) {
            returnList <- list(Result = "No Model Names Provided")
        } else {         
            if (is.null(modelList)) {
                returnList <- list(Result = "No Models Provided")
            } else {
            # we have everything!
                returnList <- list()
                for (modelName in modelNames) {
                    # call SIMCA
                    if (is.null(tuningParametersList)) {
                        alphaLevel <-  NULL  # will then use default
                    } else {
                        alphaLevel <- 1 - as.numeric(tuningParametersList$Threshold)
                    }
                    predictions <- predictSIMCA(modelList[[modelName]], 
                                                spectrumInput, 
                                                alphaLevel)
                    
                    # save results
                    returnList[[modelName]] <- predictions
                        
                }
            }
        }
    }
    
    return(returnList)
}

