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
