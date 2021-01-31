#
#
# analysis.R
#
# provide functions needed for Spectral Analysis App
#
# 1. getModelNames() 
#   return Names of all the models that can be chosen
#   example: return c("Model 1", "Model 2")
#   can be a single model name
#   these names will be used when the analysis itself is called
#
# 2. doAnalysis(spectrumInput)
#   Input: you will get a single spectrum read from Enlighten as .csv 
#   in a data frame. Possible columns are (depending on your file): 
#       Pixel, Wavelength, Wavenumber, Raw, Dark, Processed, ...
#
#   Output: return key/value pairs from your analysis in a list
#   the key/value pairs will be printed on separate rows
#   example: return list(Result = "Pass", Confidence = "97.2%", Matches = 5)
#
#
#

getModelNames <- function() {
    return( c("Model 1", "Model 2", "Model 3") )
}


doAnalysis <- function(spectrumInput = NULL, models = NULL) {
    if (is.null(spectrumInput)) {
        returnList <- list(Result = "No Data File")
    } else {
        if (is.null(models)) {
            returnList <- list(Result = "No Models Provided")
        } else {
            # we have everything!
            returnList <- list()
            modelIndex <- 0
            for (analysisModel in models) {
                returnList[[analysisModel]] <- 
                    list(Model = analysisModel,
                                   Min = min(spectrumInput$Processed),
                                   Max = max(spectrumInput$Processed),
                                   Result = "Pass")
            }
            returnList <- unlist(returnList)
        }
    }
    
    return(returnList)
}
