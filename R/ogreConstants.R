# OGRE_VARIABLES ---------------------------------------------------------------
OGRE_VARIABLES <- function # OGRE_VARIABLES
### OGRE_VARIABLES
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
OGRE_ODM_UNITS <- function # OGRE_ODM_UNITS
### OGRE_ODM_UNITS
(
  stringsAsFactors = default.stringsAsFactors()
  ### passed on to data.frame()
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
OGRE_ODM_METHODS <- function # OGRE_ODM_METHODS
### OGRE_ODM_METHODS
(
  stringsAsFactors = default.stringsAsFactors()
  ### passed on to data.frame()
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
OGRE_ODM_LABMETHODS <- function # OGRE_ODM_LABMETHODS
### OGRE_ODM_LABMETHODS
(
  stringsAsFactors = default.stringsAsFactors()
  ### passed on to data.frame()
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
OGRE_ODM_SOURCES <- function # OGRE_ODM_SOURCES
### OGRE_ODM_SOURCES
(
  stringsAsFactors = default.stringsAsFactors()
  ### passed on to data.frame()
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
OGRE_ODM_SITES <- function # OGRE_ODM_SITES
### OGRE_ODM_SITES. 
### TODO: further information on the sites (e.g. addresses need to be
### added manually within the database...)  
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
OGRE_DICTIONARY_FILE <- function # default folder "dictionary" file
### default "dictionary" file describing the folder structure to be used in OGRE
(
)
{
  system.file("extdata", "ogrePathDictionary.txt", package = "kwb.ogre")
}

# OGRE_RAIN_GAUGES -------------------------------------------------------------
OGRE_RAIN_GAUGES <- function # OGRE_RAIN_GAUGES
### OGRE_RAIN_GAUGES
()
{
  bwbShortNames <- c('Bln IX','Bln X','Bln XI','Kar','Mal','Rei I','Stg','Wil',
                     'Wil a','Wit','Zhl I')
  
  bwbRainGauges <- BWB_RAIN_GAUGES()
  bwbRainGauges[bwbRainGauges$BWB_SHORT %in% bwbShortNames, ]
}

# OGRE_SITES -------------------------------------------------------------------
OGRE_SITES <- function # OGRE_SITES
### OGRE_SITES
() 
{
  sites <- odmSites(odbc = "OGRE_META")
  
  result <- sapply(sites$SiteID, FUN = function(x) {
    list(list(SiteID = x))
  })
  
  names(result) <- sites$SiteCode
  
  result
  ### list of lists. One list element per Site, each of which is a list
  ### with exaclty one list element: SiteID, holding the SiteID of the
  ### corresponding site
}

# OGRE_TIMESERIES --------------------------------------------------------------
OGRE_TIMESERIES <- function # OGRE_TIMESERIES
### OGRE_TIMESERIES
()
{
  db <- "OGRE_META"
  
  setCurrentDb(db)
  
  list (
    
    RAIN_FUB_RAW = list(
      
      MethodID = hsLookupOrAddRecord(
        db, "Methods", 
        list(MethodDescription="R-import rain mm/h"), 
        list(),
        idField = "MethodID"),
      
      VariableID = hsLookupOrAddRecord(
        db, "Variables", 
        list(VariableCode = "Precipitation"),
        list(VariableName = "Precipitation",
             VariableUnitsID = hsLookupOrAddRecord(
               db, "Units", 
               list(UnitsAbbreviation = "mm"),
               list(),
               idField = "UnitID")),
        idField = "VariableID"),
      
      SourceID = hsLookupOrAddRecord(
        db, "Sources", 
        list(Organization = "FUB"),         
        list(SourceDescription = "Freie Universitaet Berlin"),
        idField = "SourceID"),
      
      QualityControlLevelID = hsLookupOrAddRecord(
        db, "QualityControlLevels", 
        list(Definition = "Raw data"), 
        list(),
        idField = "QualityControlLevelID")
    ),
    
    RAIN_BWB5min_RAW = list(
      
      MethodID = hsLookupOrAddRecord(
        db, "Methods", 
        list(MethodDescription = "R-import of BWB raw rain data (xls)")),
      
      VariableID = hsLookupOrAddRecord(
        db, "Variables", 
        list(VariableName = "Precipitation")),
      
      SourceID = hsLookupOrAddRecord(
        db, "Sources", 
        list(Organization = "BWB"), 
        list(SourceDescription = "Berliner Wasserbetriebe")),
      
      QualityControlLevelID = hsLookupOrAddRecord(
        db, "QualityControlLevels", 
        list(Definition = "Raw data"))
    )
  )
}
