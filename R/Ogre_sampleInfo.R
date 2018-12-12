# getInfoOnAnalysedSamplesFromExcel --------------------------------------------
getInfoOnAnalysedSamplesFromExcel <- function # getInfoOnAnalysedSamplesFromExcel
### Read information on analysed samples from Excel file maintained by OGRE team
(
  xls, 
  ### full path to Excel file
  dbg = FALSE,
  ### if TRUE, debug messages are shown, else not
  sheetPrefix = "data_"
  ### prefix of sheet names of sheets to be read
)
{
  ##seealso<< \code{\link{getInfoOnAnalysedSamplesForStation}}
  
  tableNames <- hsTables(xls)
  
  if (dbg) {
    cat("Es gibt die folgenden Tabellen:\n", commaCollapsed(tableNames), "\n")
  }
  
  prefixPattern <- paste0("^", sheetPrefix)
  dataTableNames <- grep(prefixPattern, tableNames, value = TRUE)
  
  if (length(dataTableNames) == 0) {
    stop(
      sprintf("There are no data ranges (starting with '%s') defined in '%s'!", 
              sheetPrefix, xls),
      "Please select the cell range that spans the table ",
      "(including the header row and the expected maximum number of rows) ",
      sprintf("and give that range the name '%s' with '<SiteCode>' ",
              paste0(sheetPrefix, "_<SiteCode>")),
      "being replaced with the actual SiteCode."
    )
  }

  stations <- gsub(prefixPattern, "", dataTableNames)
  
  datalist <- lapply(stations, function(station) {
    catIf(dbg, "\n*** Station:", station, "\n")
    getInfoOnAnalysedSamplesForStation(
      xls, station = station, dbg = dbg, sheetPrefix = sheetPrefix
    )
  })

  kwb.utils::rbindAll(datalist)
}

# getInfoOnAnalysedSamplesForStation -------------------------------------------
getInfoOnAnalysedSamplesForStation <- function # getInfoOnAnalysedSamplesForStation
### Read information on analysed samples for one monitoring station from Excel
### file maintained by OGRE team
(
  xls,
  ### full path to Excel file
  station,
  ### three letter code of monitoring station for which data are to be read
  dbg = FALSE,
  ### if TRUE, debug messages are shown, else not
  sheetPrefix = "data_"
  ### prefix of sheet names of sheets to be read
)
{
  ##seealso<< \code{\link{getInfoOnAnalysedSamplesFromExcel}}
  
  # Compose sheet name
  sheet <- paste0(sheetPrefix, station)
  
  # Get field configuration and names of required fields
  fieldConfig <- .getFieldConfig()

  requiredFields <- fieldConfig$Name[fieldConfig$Type == "required"]
  
  # Get names of available fields
  availableFields <- kwb.db::hsFields(xls, sheet)

  # Check for missing columns
  missingFields <- setdiff(requiredFields, availableFields)
  
  if (length(missingFields) > 0) {
    warning(
      sprintf("There are missing columns in range '%s' of '%s': %s. ", 
              sheet, xls, commaCollapsed(hsQuoteChr(missingFields))),
      "I skip this range!"
    )
    return (NULL)
  }

  # Read the data table from the Excel file
  sampling <- kwb.db::hsGetTable(
    xls, 
    tbl = sheet, 
    cond = "NOT IsNull([LIMS-Nr]) OR NOT IsNull([BAK-LIMS-Nr])",
    stringsAsFactors = FALSE,
    dbg = dbg
  )
  
  # Return NULL if there is no data at all
  if (isNullOrEmpty(sampling)) {
    warning(sprintf("Range '%s' of '%s' did not contain any data", sheet, xls))
    return (NULL)
  }
  
  # Reduce to configured fields and rename fields if they have an alias
  sampling <- kwb.utils::renameColumns(
    selectColumns(sampling, intersect(fieldConfig$Name, availableFields)), 
    .getFieldRenames(fieldConfig)
  )
  
  # Provide the timestamps of beginning and end of sampling
  sampling$Zeitraum_Probenahme <- removeSpaces(sampling$Zeitraum_Probenahme)
  
  pattern <- "^\\d{2}:\\d{2}(-\\d{2}:\\d{2})?$"
  valid <- grepl(pattern, sampling$Zeitraum_Probenahme)
  
  if (! all(valid)) {
    stop("There are invalid values of 'Zeitraum_Probenahme' ('hh:mm-hh:mm' ",
         "or 'just hh:mm'): ", commaCollapsed(
           hsQuoteChr(sampling$Zeitraum_Probenahme[! valid])))
  }
  
  parts <- strsplit(sampling$Zeitraum_Probenahme, split = "-")
  
  # Daum_Ereignis must be given
  dateIsMissing <- kwb.utils::isNaOrEmpty(sampling$Datum_Ereignis)
  
  if (any(dateIsMissing)) {
    invalidRows <- sampling[dateIsMissing, ]
    stop(
      "'Datum_Ereignis' is missing for these LIMS-/BAK-LIMS-Numbers: ",
      paste(invalidRows$LIMS_Nr, "/", invalidRows$BAK_LIMS_Nr, collapse = ", ")
    )
  }
  
  sampling$firstSampling <- .toTimestamp(sampling$Datum_Ereignis, parts, 1)
  sampling$lastSampling  <- .toTimestamp(sampling$Datum_Ereignis, parts, 2)    
  
  nextDay <- sampling$lastSampling < sampling$firstSampling
  
  printIf(dbg, sampling[nextDay, ], "*** These samplings end at the next day")
  
  sampling$lastSampling[nextDay] <- .nextDay(sampling$lastSampling[nextDay])
  
  printIf(dbg, sampling[nextDay, ], "*** After correction")

  sampling$station <- station

  columns <- c("station", "LIMS_Nr", "BAK_LIMS_Nr", "firstSampling", 
               "lastSampling", "Art_der_Probe")  
  
  selectColumns(sampling, columns)
}

# .getFieldConfig --------------------------------------------------------------
.getFieldConfig <- function()
{
  fieldConfigLines <- c(
    "Name;Type;Alias",
    "Datum_Ereignis;required;",
    "Ereignis_Nr_innerhalb_Sample_log;optional;subevent",
    "Zeitraum_Probenahme;required;",
    "Probe_entnommen_am;optional;",
    "Probe_erstellt_am;optional;",
    "Art_der_Probe;required;",
    "LIMS-Nr;required;LIMS_Nr",
    "BAK-LIMS-Nr;required;BAK_LIMS_Nr"
  )

  utils::read.csv2(text = fieldConfigLines, stringsAsFactors = FALSE)
}

# .getFieldRenames -------------------------------------------------------------
.getFieldRenames <- function(fieldConfig)
{
  hasAlias <- fieldConfig$Alias != ""
  
  kwb.utils::toLookupList(
    keys = fieldConfig$Name[hasAlias], 
    values = fieldConfig$Alias[hasAlias]
  )
}

# .toTimestamp -----------------------------------------------------------------
.toTimestamp <- function(dates, parts, partNumber)
{
  times <- sapply(parts, function(x) {
    ifelse(length(x) >= partNumber, x[partNumber], x[1])
  })
  
  hsToPosix(paste(dates, times))
}

# .nextDay ---------------------------------------------------------------------
.nextDay <- function(x)
{
  x + 60 * 60 * 24
}

# checkLimsNumbers -------------------------------------------------------------
checkLimsNumbers <- function # checkLimsNumbers
### check for duplicate LIMS numbers in sample information
(
  x
  ### data frame as returned by \code{\link{getInfoOnAnalysedSamplesFromExcel}}
  ### or \code{\link{getInfoOnAnalysedSamplesForStation}}
)
{
  na_omit <- stats::na.omit
  
  lims <- na_omit(x$LIMS_Nr)
  baklims <- na_omit(x$BAK_LIMS_Nr)
  
  numberOfLimsNumbers <- length(lims) + length(baklims)
  
  lims.all <- c(lims, baklims)
  numberOfDifferent <- length(unique(lims.all))
  
  if (numberOfLimsNumbers != numberOfDifferent) {
    
    times <- stats::aggregate(lims.all, by = list(LIMS_Nr = lims.all), FUN = length)
    duplicates <- times$LIMS_Nr[times$x > 1]
    
    cat("\n*** Duplicate LIMS numbers:\n")
    print(duplicates)
    
    cat("\n*** corresponding rows:\n")
    print(rbind(x[x$LIMS_Nr %in% duplicates, ],
                x[x$BAK_LIMS_Nr %in% duplicates, ]))
    
    stop("There are ", numberOfLimsNumbers, " LIMS numbers but only ",
         numberOfDifferent, " of them are different!")
  }
}

