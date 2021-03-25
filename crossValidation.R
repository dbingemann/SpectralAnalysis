############################################################
#
#   Cross Validation Tools
#   to be used with SIMCA functions
#
#   v 1.0
#   17 March 2021
#
############################################################



#
# this uses the SIMCA functions
#   
#   analysisSIMCA(spectraInput, preProcessingParameters = NULL, SIMCAcomp = NULL)
#       returns simcaModel
#
#   simcaModel is a list
#       contains distances for every spectrum
#           scoreDistances
#           orthogonalDistances
#
#   



#######################################
#
#   Wrapper to call analysis function from CV
#
#######################################



analysisCV <- function(spectraInput, model = NULL, numComp = NULL, alphaLevel = NULL) {
   
    #   wrapper
    #
    #   if no model is provided:
    #       call analysisSIMCA(spectraInput, preProcessingParameters = NULL, SIMCAcomp = NULL)
    #
    #   then call predictSIMC(simcaModel, newSpectraInput, alphaLevel = NULL)
    #       returns 1-percentile (alpha level, p value)
    #
    
    if (is.null(model)) {
        # no model, find new one
        if (is.null(numComp)) {
            stop("Need to provide number of components for CV if no model is provided")
        } else {
            # alpha now in model.
            # if passing NULL, model will pick global default
            model <- analysisSIMCA(spectraInput, 
                                   SIMCAcomp = numComp, 
                                   alphaLevel = alphaLevel)
        }
    }
    
    simcaModel <- model
    
    # alpha now in model
    predictions <- predictSIMCA(simcaModel, spectraInput)

    # return a number for every sample
    scorePercentile <- predictions$scorePercentile
    orthogonalPercentile <- predictions$orthogonalPercentile
    pValue <- 1 - apply(cbind(scorePercentile, orthogonalPercentile), MARGIN = 1, FUN = max)   
    
    return(pValue)
    
}


###########################################
#
#
#   general CV function
#
#
###########################################



crossValidation <- function(spectraInput, numFolds = 5, random = FALSE, maxComp) {
    
    #
    # cross validation 
    #
    #   use input data and work through 
    #   uses some of the data to determine a model
    #       find training distances, quantify as rel. distance/quantile
    #       find validation distances, quantify the same
    #   
    #   change flexibility of model and repeat
    #
    #   calls wrapper analysis(spectraInput, numComp) 
    #       returns p-values for each train sample (if training call) 
    #           or for each test sample (if test call)
    #   
    #   changes the number of components of model
    #       from 1 to numComp
    #       and keep track of the full results for each number of components
    #
    #
    # input
    #   spectraInput
    #       Enlighten format of many spectra to analyze together
    #
    #   numFolds
    #       number of folds to cross validate over
    #
    #   random
    #       if order of samples is random or in order
    #
    #   maxComp
    #       maximum number of components to try
    #
    #
    # output
    #   crossValResults
    #       return model results for each CV num component run
    #       
    #       
    
    
    # find samples for training
    
    
    # find model
    
    
    # do predictions with remaining samples - validation
    
    
    # save
    #   return values for each train and test/val sample
    #       for each number of components
    #
    
}



##############################################
#
#
#   general multiple random CV
#
#
##############################################


multipleRandomCV <- function() {

    #
    # multiple Random cross validations
    #
    #   call the cross validation function repeatedly
    #   for each call, find the CV results
    #   
    #
    # input - same as what is needed for single CV
    #
    #   spectraInput
    #       Enlighten format of many spectra to analyze together
    #
    #   numFolds
    #       number of folds to cross validate over
    #
    #   numComp
    #       maximum number of components to try
    #
    #   also: numRepeats
    #       number of repeats to go through in random CV
    #
    #
    #   we set random in single CV to TRUE
    #       set order of samples to random order
    #
    #
    #   output
    #
    #       multiRandomCrossValResults
    #           one crossValResults for each run
    #
    
    
}





