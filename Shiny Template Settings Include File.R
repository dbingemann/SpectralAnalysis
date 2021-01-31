

# set default echo output to FALSE if no output is desired
# set to TRUE if all echo output is desired

require(tidyverse)
require(colorspace)

# new colors as of 18 June 2019 - set to marketing standard
WasatchColor <- RGB(R = 66, G = 142, B = 181)
WasatchColorHex <- "#428eb5"
plotGrey <- "#808285"
typeFont <- "Calibri"

sysinfo <- Sys.info()

if (sysinfo["sysname"] == "Windows") {
  windowsFonts(Calibri = windowsFont("calibrib"))
}




# for gradient
# start
WasatchBlue <- "#428eb5"
# center
WasatchTeal <- "#638e97"
# end
WasatchGreen <- "#42996b"
# dark
WasatchDark <- "#132B43"


# qualitative scale
# maybe turn into gradient and pick the correct number of colors from it?
# Red, Orange, Yellow, Green, Blue, Purple
WasatchQualitative <- c(red = "#ff002a", 
                        orange = "#ff7e03", 
                        yellow = "#ead800", 
                        green = "#40ba76", 
                        blue = WasatchBlue, 
                        purple = "#ad2cc6")


WasatchPalette <- function(
    primary = "green",
    other = "blue",
    direction = 1
) {
    stopifnot(primary %in% names(WasatchQualitative))
    
    function(n) {
        if (n > length(WasatchQualitative)) {
            warning(paste("WP Qualitative only has", length(WasatchQualitative), "colors."))
        }
        
        if (n == 2) {
            other <- if (!other %in% names(WasatchQualitative)) {
                other
            } else {
                WasatchQualitative[other]
            }
            color_list <- c(other, WasatchQualitative[primary])
        } else {
            # find spread out indices
            indices <- seq(1, by = (length(WasatchQualitative) %/% n), length.out = n)
            color_list <- WasatchQualitative[indices]
        }
        
        color_list <- unname(unlist(color_list))
        if (direction >= 0) color_list else rev(color_list)
    }
}
    

scale_colour_WasatchQualitative <- function(
    primary = "green",
    other = "blue",
    direction = 1,
    ...
) {
    ggplot2::discrete_scale(
        "colour", "Wasatch",
        WasatchPalette(primary, other, direction),
        ...
    )
}

scale_color_WasatchQualitative <- scale_colour_WasatchQualitative

scale_fill_WasatchQualitative <- function(
    primary = "green",
    other = "blue",
    direction = 1,
    ...
) {
    ggplot2::discrete_scale(
        "fill", "Wasatch",
        WasatchPalette(primary, other, direction),
        ...
    )
}


scale_color_gradient_WasatchColors <- function(...) {
    ggplot2::scale_colour_gradient(..., low = WasatchBlue,
  high = WasatchGreen, space = "Lab",
  na.value = "grey50", guide = "colourbar", aesthetics = "colour")
}
    
scale_colour_gradient_WasatchColors <- scale_color_gradient_WasatchColors

scale_color_gradient_WasatchBlue <- function(...) {
    ggplot2::scale_colour_gradient(..., low = WasatchDark, 
  high = muted(WasatchBlue, l = 70), space = "Lab",
  na.value = "grey50", guide = "colourbar", aesthetics = "colour")
}
    
scale_colour_gradient_WasatchBlue <- scale_color_gradient_WasatchBlue



# this seems to be the best
scale_color_gradient_Wasatch <- function(...) {
    ggplot2::scale_colour_gradient(..., low = WasatchDark, 
  high = muted(WasatchTeal, l = 70), space = "Lab",
  na.value = "grey50", guide = "colourbar", aesthetics = "colour")
}
    
scale_colour_gradient_Wasatch <- scale_color_gradient_Wasatch


WasatchTheme <- theme_bw() + 
  theme(text = element_text(colour = plotGrey,
                            family = typeFont),
        line = element_line(colour = plotGrey),
        rect = element_rect(colour = plotGrey),
        plot.title = element_text(colour = WasatchColorHex, 
                                  hjust = 0.5,
                                  vjust = 0.5,
                                  size = 22),
        panel.border = element_rect(colour = plotGrey),
        axis.title = element_text(colour = WasatchColorHex, 
                                  size = 18),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 14, 
                                 colour = plotGrey,
                                 family = typeFont),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = plotGrey))




# emulate ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# I dont like the red/cyan combo this generates for two classes
twoColors <- gg_color_hue(7)[c(1,5)]

# Wasatch Standards
spectrometerColors <- c("#cbc392", "#20201e", "#ff332b", "#fd8e25", "#32bf71")
names(spectrometerColors) <- c("WP-1064", "WP-830",  "WP-785",  "WP-633",  "WP-532")

# we add a few points to avoid out of range error
# we do not need this yet...

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

enlightenFormat <- function(fileName, filePath = NULL) {
  
  if (!is.null(filePath)) {
    setwd(filePath)
  }

  enlightenVersion <- "unknown"
  firstLineHeader <-  read.csv(fileName, header = FALSE, skip = 0, nrows = 1)
  if (firstLineHeader$V1 == "ENLIGHTEN Version") {
    enlightenVersion <- "new"
  } 
  if (firstLineHeader$V1 == "Integration Time") {
    enlightenVersion <- "old"
  }
  
  if (firstLineHeader$V1 == "WPSpecCal Version") {
    enlightenVersion <- "WPSC"
  }
  
  return(enlightenVersion)
}


# Enlighten Block Export Reader Funcitons

enlightenMetaData <- function(fileName, filePath = NULL) {
    
  if (!is.null(filePath)) {
    setwd(filePath)
  }
    
    # Header
    
    # this is a generous estimate for the header size
    maxLines <- 50
    
    metaDataHeader <-  read_lines(fileName, n_max = maxLines, skip = 1)
    
    lastLine <- which(gsub(pattern = ",", replacement = "", x = metaDataHeader) == "") - 1
    
    # now each line is a full set of comma-separated values
    # we now find the columns that say "Processed" in the spectrum header row 
    # (last line + 3)
    
    # single files ahve different format, skipping one line...
    if (grepl(pattern = "[pP]rocessed", metaDataHeader[lastLine + 2])) {
      headerLine <- lastLine + 2
      singleSpectrum <- TRUE
    } else {
      headerLine <- lastLine + 3
      singleSpectrum <- FALSE
    }
    
    spectraHeader <- metaDataHeader[headerLine]
    colLabels <- read_csv(paste0(gsub(",", "\n", spectraHeader), "\n"), col_names = FALSE)
    spectraLabelRegExpr <- "[pP]rocessed"
    
    spectraIndex <- which(regexpr(spectraLabelRegExpr, colLabels$X1) > 0)
    numSpectra <- length(spectraIndex)
    
    # now discard all other lines, only keep actual meta data
    metaDataTrimmed <- metaDataHeader[seq(lastLine)]
    
    # now turn list of lines into data frame
    metaData <- data.frame(spectrumID = seq(numSpectra))
    
    for (lineIndex in seq(lastLine)) {
        headerLineText <- metaDataTrimmed[lineIndex]
        headerLineCR <- paste0(gsub(",", "\n", headerLineText), "\n")
        # this CSV routine is pretty smart: skips empty cell and uses first cell as the name
        # but some rows are empty all along, so we nedd to turn 'skip empty' off
        colData <- read_csv(headerLineCR, skip_empty_rows = FALSE)
        
        if (singleSpectrum) {
          # always the first entry
          colDataSave <- colData[1, 1]
        } else {
          # select the data points that correspond to the columns 
          # that read "Processed" as spectral label
          colDataSave <- colData[spectraIndex-1, 1]
        }
        metaData <- cbind(metaData, colDataSave)
    }
    
    # yeah!
    return(metaData)
}




enlightenSpectra <- function(fileName, filePath = NULL) {
    # repeat code to get columns that matter and start line

  if (!is.null(filePath)) {
    setwd(filePath)
  }
    

    # this is a generous estimate for the header size
    maxLines <- 50
    metaDataHeader <-  read_lines(fileName, n_max = maxLines, skip = 1)
    
    # find empty line = end of header and marker of start of spectra
    lastLine <- which(gsub(pattern = ",", replacement = "", x = metaDataHeader) == "") - 1
    
    # single files ahve different format, skipping one line...
    if (grepl(pattern = "[pP]rocessed", metaDataHeader[lastLine + 2])) {
      headerLine <- lastLine + 2
    } else {
      headerLine <- lastLine + 3
    }
    
    # read spectra
    spectraInput <- read.csv(fileName, header = TRUE, skip = headerLine)

}


# Enlighten Single Spectra Files


enlightenSingleMetaData <- function(fileName, filePath = NULL) {
    
  if (!is.null(filePath)) {
    setwd(filePath)
  }
    
    # Header
    
    # this is a generous estimate for the header size
    maxLines <- 50
    
    metaDataHeader <-  read_lines(fileName, n_max = maxLines, skip = 1)
    
    lastLine <- which(gsub(pattern = ",", replacement = "", 
                           x = metaDataHeader) == "") - 1
    
    # now each line is a full set of comma-separated values
    # we now find the columns that say "Processed" in the spectrum header row 
    # (last line + 2) - for single spectra files
    
    spectraIndex <- 2
    numSpectra <- length(spectraIndex)
    
    # now discard all other lines, only keep actual meta data
    metaDataTrimmed <- metaDataHeader[seq(lastLine)]
    
    # now turn list of lines into data frame
    metaData <- data.frame(spectrumID = seq(numSpectra))
    
    for (lineIndex in seq(lastLine)) {
        headerLineText <- metaDataTrimmed[lineIndex]
        headerLineCR <- gsub(",", "\n", headerLineText)
        # this CSV routine is pretty smart: skips empty cell and uses first cell as the name
        # but some rows are empty all along, so we nedd to turn 'skip empty' off
        colData <- read_csv(headerLineCR, skip_empty_rows = FALSE)
        # select the data points that correspond to the columns 
        # that read "Processed" as spectral label
        colDataSave <- colData[spectraIndex-1, 1]
        metaData <- cbind(metaData, colDataSave)
    }
    
    # yeah!
    return(metaData)
}




enlightenSingleSpectra <- function(fileName, filePath = NULL) {
    # repeat code to get columns that matter and start line

  if (!is.null(filePath)) {
    setwd(filePath)
  }
    

    # this is a generous estimate for the header size
    maxLines <- 50
    metaDataHeader <-  read_lines(fileName, n_max = maxLines, skip = 1)
    
    # find empty line = end of header and marker of start of spectra
    spectrumHeader <- which(gsub(pattern = ",", replacement = "", 
                                 x = metaDataHeader) == "") + 1
    
    # read spectra
    spectraInput <- read.csv(fileName, header = TRUE, skip = spectrumHeader)

}



enlightenGetProcessed <- function(spectraInput, spectraLabel = "Processed") {
  
  firstLetter <- substr(spectraLabel, 1, 1)
  restOfLabel <- str_to_lower(substr(spectraLabel, 2, nchar(spectraLabel)))
  spectraLabelRegExpr <- paste0("[", str_to_lower(firstLetter),
                               str_to_upper(firstLetter), "]", 
                               restOfLabel)
  
    spectraIndex <- which(regexpr(spectraLabelRegExpr, names(spectraInput)) > 0)

    return(as.matrix(spectraInput[, spectraIndex]))
}


enlightenCalcWavelengths <- function(metaData) {
    
    # calc a matrix of wavelengths from the wavecal coeffcients.
    # can be more than one set of wavecals
    
        
    # read all wavecals
    waveCalNames <- c("CCD C0", "CCD C1", "CCD C2", "CCD C3")
    if (! (all(sapply(waveCalNames, function(x) x %in% names(metaData)))) ) {
      wavelengths <- NA
    } else {
          waveCalCoefs <- metaData[, waveCalNames]
    
        # generate Matrix of pixels
        pixels <- sapply(metaData$`Pixel Count`, seq) - 1
        ones <- matrix(1, nrow = nrow(pixels), ncol = ncol(pixels))
        
        wavelengths <- t(t(ones) * waveCalCoefs[, 1]) + 
                            t(t(pixels) * waveCalCoefs[, 2]) + 
                            t(t(pixels^2) * waveCalCoefs[, 3]) + 
                            t(t(pixels^3) * waveCalCoefs[, 4]) 
    }
    
    return(c(wavelengths))
}





enlightenCalcWavenumbers <- function(metaData) {
    
    # calc a matrix of wavelengths from the wavecal coeffcients.
    # can be more than one set of wavecals
    
    wavelengths <- enlightenCalcWavelengths(metaData)
    
    # get laser wavelengths - can be different
    
    laserWavelengths <- metaData$`Laser Wavelength`
    
    # determine corresponding wavenumbers for each set
    wavenumbers <-  t(-t(10^7 / wavelengths) + 10^7 / laserWavelengths)
    
    return(wavenumbers)
}




# combine data frames with only partial overlap of variables

rbindWithDiff <- function(df1=NULL, df2=NULL) {
  # add df1 + df2, return dfCombined like rbind(df1, df2), but more flexible
  
  if (is.null(df1)) {
    return(df2)
  } else {
    if (is.null(df2)) {
      return(df1)
    } else {
      # both not NULL
      names1 <- names(df1)
      names2 <- names(df2)
      
      # names in both
      namesBoth <- intersect(names1, names2)
      
      if (length(namesBoth) == 0) {
        stop("In 'rbindWithDiff': df1 and df2 do not have common names.")
      }
      namesDF1only <- setdiff(names1, namesBoth)
      namesDF2only <- setdiff(names2, namesBoth)
      
      # length
      n1 <- nrow(df1)
      n2 <- nrow(df2)
      
      # remove factors
      for (name in namesBoth) {
        if (is.factor(df1[1,name])) {
          df1[, name] <- as.character(df1[, name])
          df2[, name] <- as.character(df2[, name])
        }
      }
      for (name in namesDF1only) {
        if (is.factor(df1[1,name])) {
          df1[, name] <- as.character(df1[, name])
        }
      }
      for (name in namesDF2only) {
        if (is.factor(df2[1,name])) {
          df2[, name] <- as.character(df2[, name])
        }
      }
      

      dfCombined <- rbind(data.frame(X = df1[, namesBoth], stringsAsFactors = FALSE),
                          data.frame(X = df2[, namesBoth], stringsAsFactors = FALSE))
      names(dfCombined) <- namesBoth
      
      df1add <- NULL
      for (name in namesDF1only) {
        if (is.null(df1add)) {
          df1add <- data.frame(X = c(df1[, name], rep(NA, n2)))
        } else {
          df1add <- cbind(df1add, data.frame(X = c(df1[, name], rep(NA, n2))))
        }
      }
      if (! is.null(df1add)) {
        names(df1add) <- namesDF1only
        dfCombined <- cbind(dfCombined, df1add)
      }
      
      df2add <- NULL
      for (name in namesDF2only) {
        if (is.null(df2add)) {
          df2add <- data.frame(X = c(rep(NA, n1), df2[, name]))
        } else {
          df2add <- cbind(df2add, data.frame(X = c(rep(NA, n1), df2[, name])))
        }
      }
      if (! is.null(df2add)) {
        names(df2add) <- namesDF2only
        dfCombined <- cbind(dfCombined, df2add)
      }
      

      dfCombinedNames <- names(dfCombined)
      
      # make all factors again
      for (name in dfCombinedNames) {
        if (is.character(dfCombined[1,name])) {
          dfCombined[, name] <- factor(dfCombined[, name])
        }
      }
      
      return(dfCombined)

    } # one df NULL
  } # other df NULL
  
}



flaggedPixel <- function(spectrum, threshold = 3.5) {
  
  # we use the difference of consecutive pixels
  # and then check for consecutive differences
  # this function simply accepts all edge pixels
  # a smarter version could come up with a special treatment of the edge pixels

  diffForward <- c(0, diff(spectrum))
  medianForward <- median(diffForward, na.rm = TRUE)
  # note: MAD is already scaled to SD
  madForward <- mad(diffForward, na.rm = TRUE)
  zForward <- (diffForward - medianForward) / madForward
  aboveThresholdForward <- abs(zForward) > threshold
  flagged <- c(abs(diff(sign(zForward * aboveThresholdForward))) == 2, FALSE)

  return(flagged)
  
}


impoundFlaggedPixels <- function(spectrum, flags) {
  
  # spectrum = intensities
  # flags = True, False
  # the function used to determine spikes cannot flag the first and last pixel
  # we here do not assume that this is the case for the flags,
  # that is, flags[first] and flags[last] do not have to be FALSE,
  # but we did not test this case
    
  # in case flags was numbers 0 or 1
  flags <- (flags != 0)
  
  if (length(flags) != length(spectrum)) {
    warning("Spectrum and Flags not of same length!")
  }
  
  if (sum(flags) > 0)  {
    
    flaggedPixels <- which(flags)
    numFlagged <- length(flaggedPixels)
    numPixels <- length(spectrum)
    
    # use pixel on right and left of flagged pixel
    # check that neighbors are not flagged
    for (pixelID in seq(numFlagged)) {
      
      leftPixel <- flaggedPixels[pixelID] - 1
      while (leftPixel %in% flaggedPixels) {
        leftPixel <- leftPixel - 1
      }
      rightPixel <- flaggedPixels[pixelID] + 1
      while (rightPixel %in% flaggedPixels) {
        rightPixel <- rightPixel + 1
      }
      # check if we moved past the spectrum
      if (leftPixel > 0) {
        leftValue <- spectrum[leftPixel]
      } else {
        # not part of the spectrum, use same as on the other side
        leftValue <- spectrum[rightPixel]
      }
      if (rightPixel < numPixels + 1) {
        rightValue <- spectrum[rightPixel]
      } else {
        # not part of the spectrum, use same as on other side
        rightValue <- spectrum[leftPixel]
      }
      
      # now replace with mean
      spectrum[flaggedPixels[pixelID]] <- 0.5 * (rightValue + leftValue)

    } # for pixel ID...
    
  } # some flagged
  
  return(spectrum)
}


