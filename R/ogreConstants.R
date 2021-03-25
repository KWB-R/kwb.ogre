# OGRE_VARIABLES ---------------------------------------------------------------
#' OGRE_VARIABLES
#'
#' @return data frame with laboratory variables
#' @export
#' @importFrom utils read.table
#' @examples
#' str(OGRE_VARIABLES())
#' 
OGRE_VARIABLES <- function
(
)
{
  csv <- system.file("extdata", "laboratoryVariables.csv", package="kwb.ogre")  
  
  utils::read.table(
    file = csv, sep = ";", header=TRUE, stringsAsFactors = FALSE, 
    comment.char = ""
  )
}

# OGRE_ODM_SAMPLE_TYPES --------------------------------------------------------
#' OGRE_ODM_SAMPLE_TYPES
#'
#' @param stringsAsFactors TRUE or FALSE (default: \code{\link{default.stringsAsFactors}})
#' passed on to data.frame()
#' @return data frame with sample types
#' @export
#' @importFrom kwb.utils callWithStringsAsFactors
#' @examples
#' str(OGRE_ODM_SAMPLE_TYPES())
OGRE_ODM_SAMPLE_TYPES <- function # OGRE_ODM_SAMPLE_TYPES
### OGRE_ODM_SAMPLE_TYPES
(
  stringsAsFactors = default.stringsAsFactors()
  ### passed on to data.frame()
)
{
  kwb.utils::callWithStringsAsFactors(
    stringsAsFactors,
    rbind,
    data.frame(
      Term = "Composite", 
      Definition = "Composite sample of one or more bottles of autosampler (OGRE)"
    ),
    data.frame(
      Term = "Single", 
      Definition = "Single sample of one sampling or one bottle of autosampler (OGRE)"
    )
  )
}

# OGRE_ODM_UNITS ---------------------------------------------------------------
#' OGRE_ODM_UNITS
#' @param stringsAsFactors TRUE or FALSE (default: \code{\link{default.stringsAsFactors}})
#' passed on to data.frame()
#' @return data frame with units
#' @export
#' @importFrom kwb.utils callWithStringsAsFactors
#' @examples
#' str(OGRE_ODM_UNITS())
OGRE_ODM_UNITS <- function 
(
  stringsAsFactors = default.stringsAsFactors()
)
{
  kwb.utils::callWithStringsAsFactors(
    stringsAsFactors,
    rbind,
    data.frame(UnitsName = "plaque-forming units per 100 milliliters",
               UnitsType = "Organism Concentration",
               UnitsAbbreviation = "PFU/100 ml"),
    data.frame(UnitsName = "number of organisms per 100 milliliters",
               UnitsType = "Organism Concentration",
               UnitsAbbreviation = "#/100 ml")
  )
}

# OGRE_ODM_METHODS -------------------------------------------------------------
#' OGRE_ODM_METHODS
#' @param stringsAsFactors TRUE or FALSE (default: \code{\link{default.stringsAsFactors}})
#' passed on to data.frame()
#' @return data frame with methods
#' @export
#' @importFrom kwb.utils callWithStringsAsFactors
#' @examples
#' str(OGRE_ODM_METHODS())
OGRE_ODM_METHODS <- function 
(
  stringsAsFactors = default.stringsAsFactors()
)
{
  kwb.utils::callWithStringsAsFactors(
    stringsAsFactors,
    rbind,
    data.frame(MethodDescription = "R-import from BWB lab result file (xls)"),
    data.frame(MethodDescription = "Manual setting by DW"),
    data.frame(MethodDescription = "Manually calculated EMC")
  )
}

# OGRE_ODM_LABMETHODS ----------------------------------------------------------
#' OGRE_ODM_LABMETHODS
#' 
#' @param stringsAsFactors TRUE or FALSE (default: \code{\link{default.stringsAsFactors}})
#' passed on to data.frame()
#' @return data frame with labmethods
#' @export
#' @importFrom kwb.utils callWithStringsAsFactors
#' @examples
#' str(OGRE_ODM_LABMETHODS())
OGRE_ODM_LABMETHODS <- function
(
  stringsAsFactors = default.stringsAsFactors()
)
{
  kwb.utils::callWithStringsAsFactors(
    stringsAsFactors,
    rbind,
    data.frame(
      LabName = "BWB Laboratory", 
      LabOrganization = "BWB", 
      LabMethodName = "BWB Lab: standard parameters and trace contaminants"
    )
  )
}

# OGRE_ODM_SOURCES -------------------------------------------------------------
#' OGRE_ODM_SOURCES 
#' 
#' @param stringsAsFactors TRUE or FALSE (default: \code{\link{default.stringsAsFactors}})
#' passed on to data.frame()
#' @return data frame with sources
#' @export
#' @importFrom kwb.utils callWithStringsAsFactors
#' @examples
#' str(OGRE_ODM_SOURCES())
OGRE_ODM_SOURCES <- function
(
  stringsAsFactors = default.stringsAsFactors()
)
{
  kwb.utils::callWithStringsAsFactors(
    stringsAsFactors,
    rbind,
    data.frame(
      Organization = "KWB", 
      SourceDescription = "Monitoring des KWB, Projekt OGRE", 
      ContactName = "Daniel Wicke"
    )
  )
}

# OGRE_ODM_SITES ---------------------------------------------------------------
#' OGRE_ODM_SITES 
#' @description TODO: further information on the sites (e.g. addresses need to be
#' added manually within the database...)  
#' @param stringsAsFactors TRUE or FALSE (default: \code{\link{default.stringsAsFactors}})
#' passed on to data.frame()
#' @return data frame with sources
#' @export
#' @importFrom kwb.utils callWithStringsAsFactors
#' @examples
#' str(OGRE_ODM_SITES())
OGRE_ODM_SITES <- function # OGRE_ODM_SITES

(
  stringsAsFactors = default.stringsAsFactors()
  ### passed on to data.frame()
)
{
  kwb.utils::callWithStringsAsFactors(
    stringsAsFactors,
    rbind,
    data.frame(
      SiteCode = "ALT", 
      SiteName = "Altbau", 
      Latitude = 0, 
      Longitude = 0, 
      LatLongDatumID = 0
    ),
    data.frame(
      SiteCode = "NEU", 
      SiteName = "Neubau", 
      Latitude = 0, 
      Longitude = 0, 
      LatLongDatumID = 0
    ),
    data.frame(
      SiteCode = "STR",  
      SiteName = "Strasse", 
      Latitude = 0, 
      Longitude = 0, 
      LatLongDatumID = 0
    ),
    data.frame(
      SiteCode = "EFH", 
      SiteName = "Einfamilienhaus", 
      Latitude = 0, 
      Longitude = 0, 
      LatLongDatumID = 0
    ),
    data.frame(
      SiteCode = "GEW", 
      SiteName = "Gewerbe", 
      Latitude = 0, 
      Longitude = 0, 
      LatLongDatumID = 0
    ),
    data.frame(
      SiteCode = "PNK", 
      SiteName = "Panke", 
      Latitude = 0, 
      Longitude = 0, 
      LatLongDatumID = 0
    )
  )
}

# OGRE_DICTIONARY_FILE ---------------------------------------------------------
#' Default folder "dictionary" file
#' @description default "dictionary" file describing the folder structure to be 
#' used in OGRE
#' @return path to OGRE path dictionary file
#' @export
#'
#' @examples
#' OGRE_DICTIONARY_FILE()
#' 
OGRE_DICTIONARY_FILE <- function 
(
)
{
  system.file("extdata", "ogrePathDictionary.txt", package = "kwb.ogre")
}

# OGRE_RAIN_GAUGES -------------------------------------------------------------
#' OGRE_RAIN_GAUGES
#' 
#' @return data frame with rain gauges used in OGRE
#' @export
#' @importFrom kwb.read BWB_RAIN_GAUGES
OGRE_RAIN_GAUGES <- function
()
{
  bwbShortNames <- c('Bln IX','Bln X','Bln XI','Kar','Mal','Rei I','Stg','Wil',
                     'Wil a','Wit','Zhl I')
  
  bwbRainGauges <- kwb.read::BWB_RAIN_GAUGES()
  bwbRainGauges[bwbRainGauges$BWB_SHORT %in% bwbShortNames, ]
}

# OGRE_SITES -------------------------------------------------------------------
#' OGRE_SITES
#'
#' @return  list of lists. One list element per Site, each of which is a list
#' with exactly one list element: SiteID, holding the SiteID of the
#' corresponding site
#' @export
#' @importFrom kwb.odm odmSites
OGRE_SITES <- function
() 
{
  sites <- kwb.odm::odmSites(odbc = "OGRE_META")
  
  result <- sapply(sites$SiteID, FUN = function(x) {
    list(list(SiteID = x))
  })
  
  names(result) <- sites$SiteCode
  
  result
}

# OGRE_TIMESERIES --------------------------------------------------------------
#' OGRE_TIMESERIES
#'
#' @return list
#' @export
#' @importFrom kwb.db hsLookupOrAddRecord setCurrentDb
OGRE_TIMESERIES <- function
()
{
  db <- "OGRE_META"
  
  kwb.db::setCurrentDb(db)
  
  list (
    
    RAIN_FUB_RAW = list(
      
      MethodID = kwb.db::hsLookupOrAddRecord(
        db, "Methods", 
        list(MethodDescription="R-import rain mm/h"), 
        list(),
        idField = "MethodID"),
      
      VariableID = kwb.db::hsLookupOrAddRecord(
        db, "Variables", 
        list(VariableCode = "Precipitation"),
        list(VariableName = "Precipitation",
             VariableUnitsID = kwb.db::hsLookupOrAddRecord(
               db, "Units", 
               list(UnitsAbbreviation = "mm"),
               list(),
               idField = "UnitID")),
        idField = "VariableID"),
      
      SourceID = kwb.db::hsLookupOrAddRecord(
        db, "Sources", 
        list(Organization = "FUB"),         
        list(SourceDescription = "Freie Universitaet Berlin"),
        idField = "SourceID"),
      
      QualityControlLevelID = kwb.db::hsLookupOrAddRecord(
        db, "QualityControlLevels", 
        list(Definition = "Raw data"), 
        list(),
        idField = "QualityControlLevelID")
    ),
    
    RAIN_BWB5min_RAW = list(
      
      MethodID = kwb.db::hsLookupOrAddRecord(
        db, "Methods", 
        list(MethodDescription = "R-import of BWB raw rain data (xls)")),
      
      VariableID = kwb.db::hsLookupOrAddRecord(
        db, "Variables", 
        list(VariableName = "Precipitation")),
      
      SourceID = kwb.db::hsLookupOrAddRecord(
        db, "Sources", 
        list(Organization = "BWB"), 
        list(SourceDescription = "Berliner Wasserbetriebe")),
      
      QualityControlLevelID = kwb.db::hsLookupOrAddRecord(
        db, "QualityControlLevels", 
        list(Definition = "Raw data"))
    )
  )
}
