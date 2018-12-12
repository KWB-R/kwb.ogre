# read_BWB_LaboratoryReportFromXls ---------------------------------------------
read_BWB_LaboratoryReportFromXls <- function  # read_BWB_LaboratoryReportFromXls
### read_BWB_LaboratoryReportFromXls. NOTE:  only rows are used in which
### parameter, LabMethodName and LabUnits are given
(
  labResult.xls, 
  ### full path to Excel file
  sheetName = "Tabelle1",
  ### name of sheet in Excel file. Default: "Tabelle1"
  date.format = underscoreToPercent("_d._m._Y"),
  ### date format used in Excel file. Default: "%d.%m.%Y"
  methodRequired = FALSE,
  ### if TRUE, only those rows of the Excel file are considered in which a
  ### method is given in column "Methode". Default: FALSE
  dbg = FALSE,
  open.on.error = TRUE
)
{
  tableName <- paste0(sheetName, "$")
  
  labResults.raw <- hsGetTable(
    labResult.xls, 
    tbl = tableName, # as.is = TRUE,
    stringsAsFactors = FALSE, 
    dbg = dbg
  )
  
  labResults <- labResults.raw
  
  keywords <- list(
    LabSampleCode = "Proben-Nr.",
    SamplingDate = "Probenahmedatum",
    SiteCode = "ext. Nummer",
    LabMethodName = "Methode",
    LabUnits = "Einheit"
  )
  
  # get the positions (row, column) of the keywords or stop
  keywordPositions <- getKeywordPositions(
    dataFrame = labResults, 
    keywords = keywords
  )
  
  # distinguish between different groups of keywords
  keyNames <- names(keywords)
  sampleKeywords <- keyNames[1:3]
  nonSampleKeywords <- setdiff(keyNames, sampleKeywords)
  
  all_are_equal_integers <- function(x) kwb.utils::allAreEqual(as.integer(x))
  
  # all sample keywords must be in the same column
  stopifnot(all_are_equal_integers(keywordPositions[2, sampleKeywords]))
  
  # all non-sample keywords must be in the same row
  stopifnot(all_are_equal_integers(keywordPositions[1, nonSampleKeywords]))
  
  # the actual measurement values are in the columns in which a site 
  # (keywords$SiteCode) is given...
  row.site <- keywordPositions$SiteCode[1]
  valueColumns <- which(!is.na(labResults[row.site, ]))
  
  # ... minus the columns containing the keys
  valueColumns <- setdiff(
    valueColumns, c(keywordPositions$LabSampleCode[2], 
                    keywordPositions$LabMethodName[2],
                    keywordPositions$LabUnits[2]))
  
  if (length(valueColumns) == 0) {
    
    if (open.on.error) {
      kwb.utils::hsOpenWindowsExplorer(labResult.xls)
    }
    
    stop(sprintf(
      paste("Not at least one valid sample given in '%s'",
            "(expecting a SiteCode in row '%s')!"),
      labResult.xls, keywords$SiteCode
    ))
  }
  
  # we use only those rows in which parameter, LabMethodName and LabUnits are given
  valueRows <- which(!(
    is.na(labResults[, keywordPositions$SiteCode[2]]) | 
      (methodRequired & is.na(labResults[, keywordPositions$LabMethodName[2]])) | 
      is.na(labResults[, keywordPositions$LabUnits[2]])
  ))
  
  # we need to preserve the key rows containing LabSampleCode, SamplingDate, SiteCode
  keyRows <- as.integer(keywordPositions[1, sampleKeywords])
  
  sampleDataKeys <- as.data.frame(
    t(labResults[keyRows, valueColumns]), 
    stringsAsFactors = FALSE)
  
  sampleDataValues <- as.data.frame(
    t(labResults[valueRows, valueColumns]), 
    stringsAsFactors = FALSE)
  
  sampleData <- cbind(sampleDataKeys, sampleDataValues)
  
  parameterColumn <- keywordPositions$LabSampleCode[2]
  parameterNames <- hsTrim(labResults[valueRows, parameterColumn])
  
  names(sampleData) <- c(sampleKeywords, parameterNames)
  
  SamplingDates <- as.Date(sampleData$SamplingDate, format = date.format)
  
  if (any(is.na(SamplingDates))) {
    quotedDates <- hsQuoteChr(sampleData$SamplingDate[is.na(SamplingDates)])
    stop(paste("I could not convert the following sample dates from text",
               "to Date using date.format =", hsQuoteChr(date.format), ":\n",
               paste(quotedDates, collapse = ", ")))
  } else {
    quotedDates <- hsQuoteChr(sort(unique(SamplingDates)))
    cat(paste(
      "\n*** I found the following sample dates, are they correct?\n",
      "If not, check the date.format, being currently:", 
      hsQuoteChr(date.format), "\n",
      paste(quotedDates, collapse = ", "),
      "\n"
    ))
  }
  
  sampleData$SamplingDate <- SamplingDates
  
  sampleDataList <- hsMatrixToListForm(
    sampleData, 
    keyFields = sampleKeywords,
    colNamePar = "VariableCode",
    colNameVal = "DataValueText")
  
  # provide metadata on parameters in additional columns
  parameterInfo <- data.frame(
    VariableCode = hsTrim(labResults[valueRows, keywordPositions$LabSampleCode[2]]),
    LabMethodName = hsTrim(labResults[valueRows, keywordPositions$LabMethodName[2]]), 
    LabUnits = hsTrim(labResults[valueRows, keywordPositions$LabUnits[2]]),
    stringsAsFactors=FALSE
  )
  
  merge(sampleDataList, parameterInfo)
  ### data frame with columns \emph{VariableCode}, \emph{LabSampleCode}, 
  ### \emph{SamplingDate}, \emph{SiteCode}, \emph{DataValueText},
  ### \emph{LabMethodName}, \emph{LabUnits}
}

