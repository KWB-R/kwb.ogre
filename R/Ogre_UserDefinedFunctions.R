# downloadLatestPankeDataFromQuantum -------------------------------------------
downloadLatestPankeDataFromQuantum <- function # download Panke data from Quantum
### download latest Panke data from Quantum web portal
(
  target.dir
  ### full path to target directory
)
{
  variables <- getQuantumVariables()
  
  variableIDs = c(variables$vAm_Buergerpark_PankeW1,
                  variables$vAm_Buergerpark_PankeQS)
  
  downloadDataOfQuarter(
    year = currentYear(), 
    quarter = currentQuarter(), 
    variableIDs = variableIDs,
    target.dir = target.dir
  )
  
  ### full path(s) to downloaded csv file(s)
}

#
# PATHS
#

# getCurrentFlowSubDirectory ---------------------------------------------------
getCurrentFlowSubDirectory <- function # getCurrentFlowSubDirectory
### getCurrentFlowSubDirectory
(
  flowDirectory,
  ### full path to the directory
  do.stop = TRUE
  ### if \code{TRUE} (default) the program will stop if there are unexpected
  ### files or folders, otherwise a warning is given
)
{
  subDirectories <- dir(flowDirectory)
  
  patternSubdir <- "^\\d{8}$"
  unexpected <- grep(patternSubdir, subDirectories, value=TRUE, invert=TRUE)
  
  if (! isNullOrEmpty(unexpected)) {
    
    messageText <- sprintf(
      "There are unexpected files/folders in \"%s\":\n  %s\n%s\n",
      flowDirectory, paste(hsQuoteChr(unexpected), collapse = "\n  "), 
      "Only folders of type 'YYYYMMDD' expected!"
    )
    
    args <- c(list(messageText), call. = FALSE)
              
    do.call(ifelse(do.stop, "stop", "warning"), args)
  }
  
  currentSubdir <- sort(setdiff(subDirectories, unexpected), decreasing=TRUE)[1]
  
  if (is.na(currentSubdir)) {
    warning("There is no subdirectory 'YYYYMMDD' in ", flowDirectory)
  }
  else {
    cat("Most recent flow sub-directory:", currentSubdir, "\n")
  }  
  
  return(currentSubdir)
}

#
# HYDRAULICS
#

# read_hydraulics --------------------------------------------------------------
read_hydraulics <- function # read_hydraulics
### read_hydraulics
(
  settings,
  ### list of settings as returned by \code{\link{configure}}
  dictionary = selectElements(settings, "dictionary"),
  ### list of path definitions
  do.stop = TRUE
  ### if \code{TRUE} (default) the program will stop if there are unexpected
  ### files or folders, otherwise a warning is given
)
{
  flowDirectory <- getOrCreatePath("FLOW_DIR", dictionary)
  
  csv <- getOrCreatePath(
    variableName = "FLOW_CSV", 
    dictionary = dictionary,
    FLOW_SUBDIR_CURRENT = getCurrentFlowSubDirectory(flowDirectory, do.stop)
  )
  
  hydraulics <- renameAndSelect(
    data = kwb.logger::readLogger_NIVUS_PCM4_2(csv), 
    renames = list(
      myDateTime = "DateTime",
      Fuellstand_m = "H",
      Geschw_m_s = "v",
      Durchfluss_l_s = "Q",
      T_.C = "T"
    )
  )
  
  hydraulics$DateTime <- hsToPosix(hydraulics$DateTime)
  
  hydraulics
  ### data frame with columns \emph{DateTime} (POSIXct, UTC), \emph{H},
  ### \emph{v}, \emph{Q}, \emph{T}
}

#
# SAMPLING
#

# readOgreSamplerFileByName ----------------------------------------------------
readOgreSamplerFileByName <- function # readOgreSamplerFileByName
### readOgreSamplerFileByName
(
  samplerFile, bottlesToConsider, siteCode = NA
)  
{
  cat(sprintf("Reading sample data from \"%s\"... ", basename(samplerFile)))
  
  if (containsNulString(samplerFile)) {
    
    cat("skipped!\n")
    
    warning(
      sprintf(
        "File \"%s\" contains null string and is skipped!",
        basename(samplerFile)
      ),
      "Remove first two bytes of the file!"
    )
    
    sampleDataExtended <- NULL
    
  } else {
    
    sampleData <- kwb.logger::readLogger_SIGMA_SD900(samplerFile)
    
    cat("ok.\n")
    
    stopOnWrongSampleSite(sampleData, siteCode, samplerFile)
    
    sampleDataExtended <- cbind(
      samplerFile = basename(samplerFile), 
      sampleData, 
      stringsAsFactors = FALSE
    )
  }
  
  # filter for relevant bottles
  sampleDataExtended <- filterForRelevantBottles(sampleDataExtended, bottlesToConsider)
  
  kwb.utils::renameColumns(sampleDataExtended, list(myDateTime = "sampleTime"))
}

# stopOnWrongSampleSite --------------------------------------------------------
stopOnWrongSampleSite <- function # stopOnWrongSampleSite
### stopOnWrongSampleSite
(
  sampleData, siteCode, samplerFile = "<unknown>"
)
{
  sampleSite <- attr(sampleData, "metadata")$SITE_ID
  
  if (!is.na(siteCode) && sampleSite != siteCode) {
    
    stop(call. = FALSE, sprintf(
      paste0(
        "The SITE_ID (\"%s\") given in the sampler file\n\n'%s'\n\n", 
        "is not \"%s\" as expected!"
      ),
      sampleSite, samplerFile, siteCode
    ))
  }  
}

#
# OTHER
#

# usePredictedFlowInTimeInterval -----------------------------------------------
usePredictedFlowInTimeInterval <- function # usePredictedFlowInTimeInterval
### usePredictedFlowInTimeInterval
(
  hydraulicData, 
  firstTimestamp, 
  lastTimestamp
)
{
  rows <- which(hsTsIn(hydraulicData$DateTime, firstTimestamp, lastTimestamp))
  hydraulicData$Q[rows] <- hydraulicData$Q.interpol[rows]
  hydraulicData
}



