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
#   Preprocessing
#
############################




preProcessing <- function(spectraInput, parameters) {
    
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
    
    
    # ID string will be added as "attr" to output
    versionString <- "SIMCA preProc - 10FEB2021 - interpol. to 1 1/cm - SG 1st derivative: w=9, p=2 - range(300,2000) - SNV scaling"
    
    # Savitzi Golay
    require(prospectr)
    
    # parameter for Savitzki Golay
    windowHalfWidth <- 4
    derivOrder <- 1
    polynomialSG <- 2
    
    spectraCols <- grep(pattern = "[pP]rocessed", names(spectraInput))
    
    spectraCombinedDerivList <- list()
    index <- 0
    
    numPixels <- nrow(spectraInput)
    pixels <- seq(numPixels)
    
    for (spectrumCol in spectraCols) {
        
        index <- index + 1
        spectrum <- spectraInput[, spectrumCol]
        
        spectrumSG <- savitzkyGolay(spectrum, 
                                    m = derivOrder,  # order
                                    p = polynomialSG,  # polynomial fit
                                    w = 2 * windowHalfWidth + 1) # window
        
        zeroPadding <- rep(0, windowHalfWidth)
        
        derivative <- c(zeroPadding, spectrumSG, zeroPadding)
        
        spectraCombinedDerivList[[index]] <- data.frame(SpectrumID = index,
                                                        Pixel = pixels,
                                                        Wavenumber = spectraInput$Wavenumber,
                                                        Derivative = derivative)  
        
    }
    
    spectraCombinedDeriv <- bind_rows(spectraCombinedDerivList)  
    
    ## Scaling
    
    startWavenumber <- 300  
    
    spectraCombinedDerivNorm <- spectraCombinedDeriv %>%
        filter(Wavenumber > startWavenumber) %>%
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
    
    colnames(spectra) <- spectraCombinedDerivNorm %>% 
        filter(Pixel == first(Pixel)) %>%
        mutate(Label = paste("Spectrum", SpectrumID, sep = ".")) %>%
        pull(Label)
    
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


analysisSIMCA <- function(spectra, SIMCAcomp = 2) {
    
    #
    #   performs SIMCA analysis
    #   input: spectra in matrix format - pixels going down rows
    #           spectra contain attr for preprocessing
    #   output: simca model results as a list
    #
    
    simcaModel <- list()
    
    preProcessing <- attr(spectra, "preProcessing")
    simcaModel[["preProcessing"]] <- preProcessing
    
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
    
    
    simcaModel[["orthogonalDistances"]] <- orthogonalDistances
    simcaModel[["meanOrthogonalDistanceSquared"]] <- v0
    
    
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
    
    # confidence - in plane
    confLevelSD <- confLevel
    h_threshold <- qchisq(confLevelSD, df = SIMCAcomp)
    thresholdSD <- sqrt(h_threshold)
    
    # confidence - orthogonal
    confLevelOD <- confLevel
    v_threshold <- exp(v0 * qnorm(confLevelOD))
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




predictSIMCA <- function(simcaModel, newSpectra, alphaLevel = 0.05) {
    
    
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
    SIMCAscoresSquared <- scoresSIMCA[, SIMCArange]^2
    d_Scores <- sqrt(rowSums(t(t(SIMCAscoresSquared) / SIMCAvariances)))
    
    
    # find original remainder
    mu <- pcaSIMCA$center
    
    loadingsSIMCA <- pcaSIMCA$rotation
    
    Xhat <- scoresSIMCA[, SIMCArange] %*% t(loadingsSIMCA[, SIMCArange])
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


plotPCAscores <- function(PCAresults, components) {
    
    #
    #   Plots scores
    #   input: PCA result
    #
    
    # PCAresults = result of prcomp
    scores <- PCAresults$x
    scoresDF <- data.frame(x = c(scores[, components[1]]),
                           y = c(scores[, components[2]]))
    
    ggplot(data = scoresDF,
           mapping = aes(x = x, y = y)) + 
        geom_point(alpha = 0.6, color = WasatchGreen) + 
        WasatchTheme + 
        ggtitle(paste("SCORES - PC ", paste(components, collapse = " and "))) + 
        xlab(paste("PC", components[1])) + 
        ylab(paste("PC", components[2])) + 
        theme(aspect.ratio = 1)
    
}



plotPCAcenter <- function(PCAresults) {
    centerArray <- PCAresults$center
    centerDF <- data.frame(x = seq_along(centerArray),
                           y = centerArray)

    #
    #   plots center spectrum from PCA
    #   input PCA results
    #
    
    ggplot(data = centerDF,
           mapping = aes(x = x, y = y)) + 
        geom_line(alpha = 0.8, color = WasatchGreen) + 
        WasatchTheme + 
        ggtitle(paste("Center Spectrum")) + 
        xlab(paste("x")) + 
        ylab(paste("y"))
    
}



plotSIMCAdist <- function(d_Scores, d_Orthogonal, thresholdSD = NA, thresholdOD = NA) {
    
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
    
    print(distPlot)
}



plotSIMCAnormDist <- function(h, v, h_threshold = NA, v_threshold = NA) {
    distDF <- data.frame(x = h,
                         y = v)
    #
    # Plots a square distance, normalized to h0 and v0
    # input: square distances h and v, as well as scale h0 and v0
    #
    
    
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
    print(distPlot)
}





plotSIMCA <- function(simcaModel = NULL, prediction = NULL,
                      modelPlotType = c("center", "scores", "distances"), 
                      alphaLevel = 0.05) {
    
    #
    #   plots model results (if no prediction is given) - pick one form
    #       center spectrum
    #       scores
    #       distances - uses alpha level from input
    #   and/or prediction results
    #       distances - uses thresholds from prediciton alpha, ignores input
    #
    
    
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
            plotSIMCAdist(d_Scores, d_Orthogonal, thresholdSD, thresholdOD)
        }
    } 
    
    if (! is.null(prediction)) {
        # plot prediction
        d_Scores <- prediction$scoreDistances
        d_Orthogonal <- prediction$orthogonalDistances
        thresholdSD <- prediction$thresholdSD
        thresholdOD <- prediction$thresholdOD
        plotSIMCAdist(d_Scores, d_Orthogonal, thresholdSD, thresholdOD)
    }
    
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

        print(distPlot)
        
    }
    
}


