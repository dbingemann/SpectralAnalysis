#
# find the folders in Enlighten Documents Path
#

getCurrentFile <- function() {

    enlightenPath <- "~/EnlightenSpectra/"
    
    # find folders
    allFolders <- list.dirs(enlightenPath, full.names = FALSE, recursive = FALSE)
    
    allDateFolders <- grep(pattern = "^20[0-9]{2}-[0-9]{2}-[0-9]{2}",
                               x = allFolders, value = TRUE)
    
    lastDateFolder <- allDateFolders[order(allDateFolders, decreasing = TRUE)[1]]
    
    currentPath <- paste0(enlightenPath, lastDateFolder)
    
#    setwd(currentPath)
    fileNames <- dir(path = currentPath, pattern = ".*.csv")
    timeStamps <- str_extract(fileNames, '[0-9]{8}-[0-9]{6}-[0-9]{6}')
    
    currentFile <- fileNames[order(timeStamps, decreasing = TRUE)[1]]
    
    return(list(folderName = lastDateFolder,
                folderPath = currentPath,
                numFiles = length(fileNames),
                fileName = currentFile))

}




addLogLine <- function(modelName, spectrum, predictions) {
    
    # get most current log file
    logFilePath <- "./LogFiles"
    logFiles <- dir(path = logFilePath, pattern = ".*.log")
    currentDate <- Sys.Date()
    logFileBaseName <- "LogFile"
    currentLogFile <- grep(pattern = currentDate, x = logFiles, value = TRUE)
    
    if (length(currentLogFile) == 0) {
        currentLogFile <- paste0(logFileBaseName, "_", currentDate, ".log")
        newLogFilePath <- paste0(logFilePath, "/", currentLogFile)
        headerLine <- "FileName,Model, Member,ScoreDistance,OrthogonalDistance,AlphaLevel,ThresholdSd,ThresholdOD"
        writeLines(headerLine, newLogFilePath)
    }
    
    currentLogFilePath <- paste0(logFilePath, "/", currentLogFile)
    newLogLine <- data.frame(FileName = spectrum$FileName,
                             Model = modelName,
                             Member = predictions$member[1],
                             ScoreDistance = predictions$scoreDistances[1],
                             OrthogonalDistance = predictions$orthogonalDistances[1],
                             AlphaLevel = predictions$alphaLevel,
                             ThresholdSD = predictions$thresholdSD,
                             ThresholdOD = predictions$thresholdOD)
    
    write.table(newLogLine, currentLogFilePath, append = TRUE, 
                sep = ",", row.names = FALSE, col.names = FALSE)
    
}




