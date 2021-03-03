#############################################################
#
#   SIMCA Functions
#
#   version 1.0
#   14 Feb 2021
#
#############################################################


############################
#
# Parameters
#
############################




simcaParameters <- function() {
    
    #
    # some 'global' setting for SIMCA
    # analysis: preProcessing, SIMCAcomp
    #   'global' values will be used in 'analysis' 
    #   if no preProcessing parameters are provided (default = NULL)
    # prediction:
    #   'global' alphaLevel will be used is no value is provided to alpha level (default - NULL)
    
    # SIMCA
    pickComponents <- 3
    alphaLevel <- 1e-5
    
    # preprocessing settings
    # parameters for preprocessing: interpolation, SG, min wavenumber
    preProcessingParameters <- list(interpSpacing = 1,
                                    startWavenumber = 300,
                                    endWavenumber = 1800,
                                    windowHalfWidth = 4,
                                    derivOrder = 1,
                                    polynomialSG = 2)
    
    parameters <- list(SIMCAcomp = pickComponents,
                       alphaLevel = alphaLevel,
                       preProcessingParameters = preProcessingParameters)
    
    return(parameters)
    
}




############################
#
#   Preprocessing
#
############################


# we add a few points to avoid out of range error
# we do not need this yet...

require(pracma)

interp <- function(x, y, xi) {
    if (xi[1] < x[1]) {
        # extrapolate
        y0 <- y[1] # - (x[1]-xi[1]) * (y[2]-y[1])/(x[2]-x[1])
        x <- c(xi[1], x)
        y <- c(y0, y)
    }
    ni <- length(xi)
    n <- length(x)
    if (xi[ni] > x[n]) {
        # extrapolate
        yn <- y[n] # + (xi[ni]-x[n]) * (y[n]-y[ni])/(x[n]-x[n-1])
        x <- c(x, xi[ni])
        y <- c(y, yn)
    }
    return(interp1(x = x, y = y, xi = xi))
}


preProcessing <- function(spectraInput, preProcessingParameters) {
    
    #
    # Preprocessing Function
    #
    # a function as part of the analysis set
    # simcaPreprocessing(spectraInput, parameters)
    # input = enlighten spectra as DF - any of the usual col names (Pixel, Wavenumber, etc)
    #        parameters = list with key value pairs
    # output = preprocessed spectra - format = matrix 
    #     with samples going down the rows, wavelengths across the columns
    #     for a single spectrum, this can also be a simple array
    #
    # maybe include a definition string (preProc name, version, etc, as an identifier?)
    # should be checked in simca model prediciton - is preProc same as in model calc?
    # 
    
    #
    # Parameters: - for this version
    #
    #       # interpolation: 
    #           interpolSpacing <- 1
    #       
    #       # range:
    #           startWavenumber <- 300  
    #           endWavenumber <- 1800
    #
    #       # parameter for Savitzki Golay
    #           windowHalfWidth <- 4
    #           derivOrder <- 1
    #           polynomialSG <- 2
    #
    #
    
    # ID string will be added as "attr" to output
    functionString <- "SIMCA preProc"
    versionDate <- "02MAR2021"
    interpolString <- "interpolation: yes"
    preProcessingString1 <- "Range Trimming Start"
    preProcessingString2 <- "Range Trimming End"
    preProcessingString3 <- "SG 1st derivative"
    preProcessingString4 <- "SNV scaling"
    parameterString <-  paste(names(preProcessingParameters), preProcessingParameters, 
                              sep = ": ", collapse = " | ")
        
    versionString <- paste(functionString,
                       versionDate,
                       interpolString,
                       preProcessingString1,
                       preProcessingString2,
                       preProcessingString3,
                       preProcessingString4,
                       parameterString, sep = " - ")
    
    # Savitzi Golay
    require(prospectr)
    
    # parameter for interpolation
    interpSpacing <- preProcessingParameters$interpSpacing
    
    
    # parameter for Savitzki Golay
    windowHalfWidth <- preProcessingParameters$windowHalfWidth
    derivOrder <- preProcessingParameters$derivOrder
    polynomialSG <- preProcessingParameters$polynomialSG
    
    # parameter for trimming
    startWavenumber <- preProcessingParameters$startWavenumber 
    endWavenumber <- preProcessingParameters$endWavenumber
    
    
    # for interpolation
    wavenumberCol <- grep(pattern = "[wW]avenumber", x = names(spectraInput))
    wavenumbers <- unlist(spectraInput[, wavenumberCol])

    interpWavenumbers <- seq(startWavenumber, endWavenumber, by = interpSpacing)
    interpPixels <- seq(length(interpWavenumbers))
    
    nonSpectraCols <- c("[pP]ixel", "[wW]avelength", "[wW]avenumber")
    spectraCols <- which(! apply(sapply(nonSpectraCols, function(x) {
                                            grepl(pattern = x, names(spectraInput)) }),
                                    MARGIN = 1, FUN = any) )
    
    spectraCombinedDerivList <- list()
    index <- 0
    
    numPixels <- nrow(spectraInput)
    pixels <- seq(numPixels)
    
    for (spectrumCol in spectraCols) {
        
        index <- index + 1
        spectrum <- unlist(spectraInput[, spectrumCol])
        interpSpectrum <- interp(x = wavenumbers,
                                 y = spectrum,
                                 xi = interpWavenumbers)
        
        spectrumSG <- savitzkyGolay(interpSpectrum, 
                                    m = derivOrder,  # order
                                    p = polynomialSG,  # polynomial fit
                                    w = 2 * windowHalfWidth + 1) # window
        
        zeroPadding <- rep(0, windowHalfWidth)
        
        derivative <- c(zeroPadding, spectrumSG, zeroPadding)
        
        spectraCombinedDerivList[[index]] <- data.frame(SpectrumID = index,
                                                        Pixel = interpPixels,
                                                        Wavenumber = interpWavenumbers,
                                                        Derivative = derivative)  
        
    }
    
    spectraCombinedDeriv <- bind_rows(spectraCombinedDerivList)  
    
    ## Scaling
    
    
    spectraCombinedDerivNorm <- spectraCombinedDeriv %>%
        group_by(SpectrumID) %>%
        mutate(DerivativeSNV = Derivative / sd(Derivative)) %>%
        ungroup()
    
    
    ## Massage into Matrix
    
    numPixels <- spectraCombinedDerivNorm %>% 
        filter(SpectrumID == min(SpectrumID, na.rm = TRUE)) %>%
        summarize(NumPixel = max(Pixel) - min(Pixel) + 1) %>%
        pull(NumPixel)
    
    spectra <- matrix(spectraCombinedDerivNorm %>% 
                          pull(DerivativeSNV),
                      nrow = numPixels)
    
    colnames(spectra) <- (names(spectraInput))[spectraCols]
    
    minPixel <- min(spectraCombinedDerivNorm$Pixel)
    
    rownames(spectra) = paste("Pixel", seq(minPixel, length.out = numPixels), sep = ".")
    
    attr(spectra, "preProcessing") <- versionString
    
    return(spectra)
}





#############################
#
#   Analysis
#   
#############################

analysisSIMCA <- function(spectraInput, preProcessingParameters = NULL, SIMCAcomp = 2) {
    #
    # wrapper to call SIMCA model calc with not pre-processed Enlighten spectra
    #
    
    # allow for global settings
    if (is.null(preProcessingParameters)) {
        # get global values form start of file
        parameters <- simcaParameters()
        preProcessingParameters <- parameters$preProcessingParameters
        SIMCAcomp <- parameters$SIMCAcomp
    }
    
    spectra <- preProcessing(spectraInput, preProcessingParameters)
    simcaModel <- analysisSIMCApreProcessed(spectra, SIMCAcomp, preProcessingParameters)
    return(simcaModel)
}


analysisSIMCApreProcessed <- function(spectra, SIMCAcomp = 2, preProcessingParameters) {
    
    #
    #   performs SIMCA analysis
    #   input: pre-processed spectra in matrix format - pixels going down rows
    #           spectra contain attr for preprocessing
    #   output: simca model results as a list
    #
    
    simcaModel <- list()
    
    preProcessing <- attr(spectra, "preProcessing")
    simcaModel[["preProcessing"]] <- preProcessing
    simcaModel[["preProcessingParameters"]] <- preProcessingParameters
    
    # PCA
    X <- t(spectra)
    PCAresults <- prcomp(X, center = TRUE)
    
    simcaModel[["trainSpectra"]] <- spectra
    simcaModel[["PCAresults"]] <- PCAresults
    
    
    scoresSIMCA <- PCAresults$x
    sdevSIMCA <- PCAresults$sdev
    loadingsSIMCA <- PCAresults$rotation
    
    # get score distance
    SIMCArange <- seq(SIMCAcomp)
    numTrain <- nrow(scoresSIMCA)
    
    simcaModel[["SIMCAcomp"]] <- SIMCAcomp
    simcaModel[["numTrain"]] <- numTrain
    
    SIMCAscoresSquared <- scoresSIMCA[, SIMCArange]^2
    SIMCAvariances <- sdevSIMCA[SIMCArange]^2
    SIMCAeigenvalues <- SIMCAvariances * numTrain
    
    simcaModel[["SIMCAvariances"]] <- SIMCAvariances
    
    # Maha Distance
    scoreDistances <- sqrt(rowSums(t(t(SIMCAscoresSquared) / SIMCAvariances)))
    
    simcaModel[["scoreDistances"]] <- scoreDistances
    
    
    # h-score
    #  h_SIMCA <- rowSums(t(t(SIMCAscoresSquared) / SIMCAeigenvalues))
    #  h0 <- SIMCAcomp / numTrain
    
    
    # get orthogonal distance
    mu <- PCAresults$center
    
    # projected back
    Xhat <- scoresSIMCA[, SIMCArange] %*% t(loadingsSIMCA[, SIMCArange])
    Xhat <- scale(Xhat, center = -mu, scale = FALSE)
    
    Xdiff <- X - Xhat
    orthogonalDistances <- sqrt(rowSums(Xdiff^2))
    
    # get orthogonal variance   
    v_SIMCA <- rowSums(Xdiff^2)
    v0 <- mean(v_SIMCA)
    logNormWidth <- sd(v_SIMCA/v0)
    
    
    simcaModel[["orthogonalDistances"]] <- orthogonalDistances
    simcaModel[["meanOrthogonalDistanceSquared"]] <- v0
    simcaModel[["logNormWidthOrthogonalDistancesSquaredNorm"]] <- logNormWidth
    
    return(simcaModel)
    
}



thresholdsSIMCA <- function(simcaModel, alphaLevel = 0.05) {
    
    #
    #   determines thresholds for score and orthogonal distances
    #   for a given SIMCA model and a given alpha level
    #
    
    # cutoff
    confLevel <- 1 - alphaLevel
    
    SIMCAcomp <- simcaModel$SIMCAcomp
    v0 <- simcaModel$meanOrthogonalDistanceSquared
    logNormWidth <- simcaModel$logNormWidthOrthogonalDistancesSquaredNorm
    
    # confidence - in plane
    confLevelSD <- confLevel
    h_threshold <- qchisq(confLevelSD, df = SIMCAcomp)
    thresholdSD <- sqrt(h_threshold)
    
    # confidence - orthogonal
    confLevelOD <- confLevel
    v_threshold <- exp(qnorm(confLevelOD, sd = logNormWidth))
    thresholdOD <- sqrt(v0 * v_threshold)
    
    thresholds <- list(alphaLevel = alphaLevel,
                       confLevel = confLevel,
                       thresholdSD = thresholdSD,
                       thresholdOD = thresholdOD)
    
    return(thresholds)
    
}



##############################
#
#   Write/Read Model
#
##############################




require(tools)


saveSIMCA <- function(simcaModel, fileName) {
    # save as binary file - no JSON converter for PCA
    extension <- ".Rdat"
    fileName <- file_path_sans_ext(fileName)
    fileName <- paste0(fileName, extension)
    saveRDS(simcaModel, fileName)
}


loadSIMCA <- function(fileName) {
    extension <- ".Rdat"
    fileName <- file_path_sans_ext(fileName)
    fileName <- paste0(fileName, extension)
    simcaModel <- readRDS(fileName)
    return(simcaModel)
}




###############################
#
#   Prediction
#
###############################


predictSIMCA <- function(simcaModel, newSpectraInput, alphaLevel = NULL) {
    #
    # wrapper to call pre processing from Enlighten Spectra, then predict
    #
    
    if (is.null(alphaLevel)) {
        parameters <- simcaParameters()
        alphaLevel <- parameters$alphaLevel
    }
    
    # get rest of parameters from model
    preProcessingParameters <- simcaModel$preProcessingParameters
    newSpectra <- preProcessing(newSpectraInput, preProcessingParameters)
    predictions <- predictSIMCApreProcessed(simcaModel, newSpectra, alphaLevel)
    return(predictions)
}



predictSIMCApreProcessed <- function(simcaModel, newSpectra, alphaLevel = 0.05) {
    
    
    # input = SIMCA model - list format
    # newSpectra = input in matrix format - coming from preProcessing
    # preprocessing has been called prior to this prediction
    #
    # expected preprocessing to do any massaging and conversion from "spectraInput" DF
    # to matrix format, along with any derivatives, interpolation, etc.
    # same preprocessing used for model formation and prediction
    #
    # also: preprocessing adds a preprocressing ID string in the attributed of matrix
    #
    # returns both distances for each spectrum, also assignment (yes/no), and thresholds used
    
    
    
    # check preprocessing string
    preProcessing <- attr(newSpectra, "preProcessing")
    
    if (preProcessing != simcaModel$preProcessing) {
        print(paste("Model Pre-Processing:", simcaModel$preProcessing))
        print(paste("New Spectrum Pre-Processing:", preProcessing))
        stop("Pre-Processing Processes do not match - Stopping SIMCA prediction")
    }
    
    # PCA
    X <- t(newSpectra)
    
    pcaSIMCA <- simcaModel$PCAresults
    scoresSIMCA <- predict(pcaSIMCA, newdata = X)
    
    # variances from model
    SIMCAcomp <- simcaModel$SIMCAcomp
    SIMCArange <- seq(SIMCAcomp)
    SIMCAvariances <- simcaModel$SIMCAvariances
    
    # Maha Distance
    # make sur eit stays a matrix - hence the drop = FALSE for single spectrum
    SIMCAscoresSquared <- (scoresSIMCA[, SIMCArange, drop=FALSE])^2
    d_Scores <- sqrt(rowSums(t(t(SIMCAscoresSquared) / SIMCAvariances)))
    
    
    # find original remainder
    mu <- pcaSIMCA$center
    
    loadingsSIMCA <- pcaSIMCA$rotation
    
    Xhat <- scoresSIMCA[, SIMCArange, drop=FALSE] %*% t(loadingsSIMCA[, SIMCArange, drop=FALSE])
    Xhat <- scale(Xhat, center = -mu, scale = FALSE)
    Xdiff <- X - Xhat
    d_Orthogonal <- sqrt(rowSums(Xdiff^2))
    
    
    # assign pass/fail
    thresholds <- thresholdsSIMCA(simcaModel, alphaLevel)
    
    members <- (d_Scores < thresholds$thresholdSD & 
                    d_Orthogonal < thresholds$thresholdOD)
    
    # return distances
    distances <- list(scoreDistances = d_Scores,
                      orthogonalDistances = d_Orthogonal,
                      member = members,
                      alphaLevel = alphaLevel,
                      thresholdSD = thresholds$thresholdSD,
                      thresholdOD = thresholds$thresholdOD)
    return(distances)
    
}




###############################
#
#   Plotting
#
###############################


plotPCAscores <- function(PCAresults, components, printPlot = FALSE) {
    
    #
    #   Plots scores
    #   input: PCA result
    #
    
    # PCAresults = result of prcomp
    scores <- PCAresults$x
    scoresDF <- data.frame(x = c(scores[, components[1]]),
                           y = c(scores[, components[2]]))
    
    scorePlot <- ggplot(data = scoresDF,
           mapping = aes(x = x, y = y)) + 
        geom_point(alpha = 0.6, color = WasatchGreen) + 
        WasatchTheme + 
        ggtitle(paste("SCORES - PC ", paste(components, collapse = " and "))) + 
        xlab(paste("PC", components[1])) + 
        ylab(paste("PC", components[2])) + 
        theme(aspect.ratio = 1)
    
    if (printPlot) {
        print(scorePlot)
    }
    return(scorePlot)
}



plotPCAcenter <- function(PCAresults, printPlot = FALSE) {
    
    centerArray <- PCAresults$center
    centerDF <- data.frame(x = seq_along(centerArray),
                           y = centerArray)

    #
    #   plots center spectrum from PCA
    #   input PCA results
    #
    
    centerPlot <- ggplot(data = centerDF,
           mapping = aes(x = x, y = y)) + 
        geom_line(alpha = 0.8, color = WasatchGreen) + 
        WasatchTheme + 
        ggtitle(paste("Center Spectrum")) + 
        xlab(paste("x")) + 
        ylab(paste("y"))
    
    if (printPlot) {
        print(centerPlot)
    }
    return(centerPlot)
}



plotSIMCAdist <- function(d_Scores, d_Orthogonal, thresholdSD = NA, 
                          thresholdOD = NA, printPlot = FALSE) {
    
    #
    #   plots distances for SIMCA
    #   input: score and orthogonal distances, optional thresholds for a given alpha
    #
    
    distDF <- data.frame(x = d_Scores,
                         y = d_Orthogonal)
 
    distPlot <- ggplot(data = distDF,
                       mapping = aes(x = x, y = y)) + 
        geom_point(alpha = 0.6, color = WasatchGreen) + 
        WasatchTheme + 
        ggtitle(paste("SIMCA Distances")) + 
        xlab(paste("Score Distance")) + 
        ylab(paste("Orthogonal Distance")) +
        theme(aspect.ratio = 1)
    
    if (! is.na(thresholdOD)) {
        distPlot <- distPlot + geom_hline(yintercept = thresholdOD, color = plotGrey, alpha = 0.4)
    }
    if (! is.na(thresholdSD)) {
        distPlot <- distPlot + geom_vline(xintercept = thresholdSD, color = plotGrey, alpha = 0.4)
    }
    
    if (printPlot) {
        print(distPlot)
    }    
    return(distPlot)
}



plotSIMCAnormDist <- function(h, v, h_threshold = NA, v_threshold = NA, 
                              printPlot = FALSE) {
    
    #
    # Plots a square distance, normalized to h0 and v0
    # input: square distances h and v, as well as scale h0 and v0
    #
    
    distDF <- data.frame(x = h,
                         y = v)
    
    distPlot <- ggplot(data = distDF,
                       mapping = aes(x = x, y = y)) + 
        geom_point(alpha = 0.6, color = WasatchGreen) + 
        WasatchTheme + 
        ggtitle(paste("Norm. SIMCA Square Dist.")) + 
        xlab(paste("Score Distance as h/h0")) + 
        ylab(paste("Orthogonal Distance as v/v0")) +
        theme(aspect.ratio = 1)
    
    if (! is.na(v_threshold)) {
        distPlot <- distPlot + geom_hline(yintercept = v_threshold, 
                                          color = plotGrey, alpha = 0.4)
    }
    if (! is.na(h_threshold)) {
        distPlot <- distPlot + geom_vline(xintercept = h_threshold, 
                                          color = plotGrey, alpha = 0.4)
    }
    if (printPlot) {
        print(distPlot)
    }
    return(distPlot)
}





plotSIMCA <- function(simcaModel = NULL, prediction = NULL,
                      modelPlotType = c("center", "scores", "distances"), 
                      alphaLevel = 0.05, printPlot = FALSE) {
    
    #
    #   plots model results (if no prediction is given) - pick one form
    #       center spectrum
    #       scores
    #       distances - uses alpha level from input
    #   and/or prediction results
    #       distances - uses thresholds from prediciton alpha, ignores input
    #
    
    distPlot <- NULL
    
    if ((! is.null(prediction)) & (! is.null(simcaModel))) {
        # combine model (train) and test in one
        
        distDF <- data.frame(x = prediction$scoreDistances,
                             y = prediction$orthogonalDistances,
                             Type = "Test")
        distDF <- rbind(distDF, data.frame(x = simcaModel$scoreDistances,
                                           y = simcaModel$orthogonalDistances,
                                           Type = "Train"))
        alphaLevels <- c(0.8, 0.3)
        sizeLevels <- c(1.5, 4)
        thresholdSD <- prediction$thresholdSD
        thresholdOD <- prediction$thresholdOD
        
        distPlot <- ggplot(data = distDF,
                           mapping = aes(x = x, y = y, alpha = Type, size = Type)) + 
            geom_point(color = WasatchGreen) + 
            scale_size_manual(values = sizeLevels) +
            scale_alpha_manual(values = alphaLevels) +
            WasatchTheme + 
            ggtitle(paste("SIMCA Distances")) + 
            xlab(paste("Score Distance")) + 
            ylab(paste("Orthogonal Distance")) +
            theme(aspect.ratio = 1) + 
            geom_hline(yintercept = thresholdOD, color = plotGrey, alpha = 0.4) +
            geom_vline(xintercept = thresholdSD, color = plotGrey, alpha = 0.4)
        
        if (printPlot) {
            print(distPlot)
        }
        
    } else {
    
        if (! is.null(simcaModel)) {
            # plot model
            if (modelPlotType == "center") {
                PCAresults <- simcaModel$PCAresults
                plotPCAcenter(PCAresults)
            } else if (modelPlotType == "scores") {
                PCAresults <- simcaModel$PCAresults
                plotPCAscores(PCAresults, components = c(1,2))
            } else {
                d_Scores <- simcaModel$scoreDistances
                d_Orthogonal <- simcaModel$orthogonalDistances
                thresholds <- thresholdsSIMCA(simcaModel, alphaLevel)
                thresholdSD <- thresholds$thresholdSD
                thresholdOD <- thresholds$thresholdOD
                distPlot <- plotSIMCAdist(d_Scores, d_Orthogonal, thresholdSD, 
                                          thresholdOD, printPlot)
            }
        } 
        
        if (! is.null(prediction)) {
            # plot prediction
            d_Scores <- prediction$scoreDistances
            d_Orthogonal <- prediction$orthogonalDistances
            thresholdSD <- prediction$thresholdSD
            thresholdOD <- prediction$thresholdOD
            distPlot <- plotSIMCAdist(d_Scores, d_Orthogonal, thresholdSD, 
                                      thresholdOD, printPlot)
        }
        
    }
    return(distPlot)
}


