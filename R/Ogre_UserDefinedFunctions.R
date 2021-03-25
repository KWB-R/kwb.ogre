# downloadLatestPankeDataFromQuantum -------------------------------------------
#' Download Panke Data from Quantum
#' @description Download latest Panke data from Quantum web portal
#' @param target.dir full path to target directory
#'
#' @return full path(s) to downloaded csv file(s)
#' @export
#' @importFrom kwb.quantum downloadDataOfQuarter getQuantumVariables
#' @importFrom kwb.datetime currentYear currentQuarter
downloadLatestPankeDataFromQuantum <- function 
(
  target.dir
)
{
  variables <- kwb.quantum::getQuantumVariables()
  
  variableIDs = c(variables$vAm_Buergerpark_PankeW1,
                  variables$vAm_Buergerpark_PankeQS)
  
  kwb.quantum::downloadDataOfQuarter(
    year = kwb.datetime::currentYear(), 
    quarter = kwb.datetime::currentQuarter(), 
    variableIDs = variableIDs,
    target.dir = target.dir
  )
  
}

#
# PATHS
#

# getCurrentFlowSubDirectory ---------------------------------------------------
#' getCurrentFlowSubDirectory
#'
#' @param flowDirectory full path to the directory
#' @param do.stop if \code{TRUE} (default) the program will stop if there are 
#' unexpected files or folders, otherwise a warning is given
#'
#' @return path to most recent flow sub-directory
#' @export

getCurrentFlowSubDirectory <- function
(
  flowDirectory,
  do.stop = TRUE
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
#' read_hydraulics
#'
#' @param settings list of settings as returned by \code{\link{configure}}
#' @param dictionary list of path definitions
#' @param do.stop  if \code{TRUE} (default) the program will stop if there are 
#' unexpected files or folders, otherwise a warning is given
#'
#' @return data frame with columns \emph{DateTime} (POSIXct, UTC), \emph{H},
#' \emph{v}, \emph{Q}, \emph{T}
#' @export
#' @importFrom kwb.logger readLogger_NIVUS_PCM4_2
#' @importFrom kwb.utils renameAndSelect selectElements
#' @importFrom kwb.monitoring getOrCreatePath
read_hydraulics <- function
(
  settings,
  dictionary = kwb.utils::selectElements(settings, "dictionary"),
  do.stop = TRUE
)
{
  flowDirectory <- kwb.monitoring::getOrCreatePath("FLOW_DIR", dictionary)
  
  csv <- kwb.monitoring::getOrCreatePath(
    variableName = "FLOW_CSV", 
    dictionary = dictionary,
    FLOW_SUBDIR_CURRENT = getCurrentFlowSubDirectory(flowDirectory, do.stop)
  )
  
  hydraulics <- kwb.utils::renameAndSelect(
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
}

#
# SAMPLING
#

# readOgreSamplerFileByName ----------------------------------------------------
#' readOgreSamplerFileByName
#'
#' @param samplerFile full path to sampler file
#' @param bottlesToConsider bottlesToConsider 
#' @param siteCode siteCode (default: NA)
#'
#' @return data frame with ???
#' @export
#' @importFrom kwb.logger readLogger_SIGMA_SD900
#' @importFrom kwb.utils containsNulString renameColumns
readOgreSamplerFileByName <- function 
(
  samplerFile, bottlesToConsider, siteCode = NA
)  
{
  cat(sprintf("Reading sample data from \"%s\"... ", basename(samplerFile)))
  
  if (kwb.utils::containsNulString(samplerFile)) {
    
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
  sampleDataExtended <- kwb.monitoring:::filterForRelevantBottles(sampleDataExtended, bottlesToConsider)
  
  kwb.utils::renameColumns(sampleDataExtended, list(myDateTime = "sampleTime"))
}

# stopOnWrongSampleSite --------------------------------------------------------
#' stopOnWrongSampleSite
#'
#' @param sampleData sampleData
#' @param siteCode siteCode 
#' @param samplerFile samplerFile (default: "<unknown>")
#'
#' @return error if site_id not found
#' @export

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
#' usePredictedFlowInTimeInterval
#'
#' @param hydraulicData hydraulicData
#' @param firstTimestamp firstTimestamp
#' @param lastTimestamp lastTimestamp
#'
#' @return data frame with filtered hydraulic data (first - last timestamp)
#' @export
#' @importFrom kwb.datetime hsTsIn
usePredictedFlowInTimeInterval <- function
(
  hydraulicData, 
  firstTimestamp, 
  lastTimestamp
)
{
  rows <- which(kwb.datetime::hsTsIn(hydraulicData$DateTime, 
                                     firstTimestamp, 
                                     lastTimestamp))
  hydraulicData$Q[rows] <- hydraulicData$Q.interpol[rows]
  hydraulicData
}



