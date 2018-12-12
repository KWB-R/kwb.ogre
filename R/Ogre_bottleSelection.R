# openBottleSelector -----------------------------------------------------------
openBottleSelector <- function # openBottleSelector
### openBottleSelector
(
  bottleEvents, 
  ### data frame as returned by \code{\link{samplingEventsToBottleEvents}}, with
  ### begin and end of time intervals represented by an auto sampler's bottle.
  version=2
)
{
  n <- nrow(bottleEvents)
  
  manipulate.args <- c(
    .expressionPlotFunctionCall(version, n),
    .manipulateControlArguments(version, n, bottleEvents$dur_minutes),
    list(ok= button(label="Ok"),
         cancel= button(label="Cancel")))
  
  do.call(manipulate, manipulate.args)
  
  #manipulatorSetState("bottleEvents.tmp", bottleEvents)
}

# .expressionPlotFunctionCall --------------------------------------------------
.expressionPlotFunctionCall <- function(version, n=0)
{
  if (version == 1) {
    variableNames_numeric <- .variableNames(n, "n")
    expressionLine <- sprintf(".ganttPlotSamples(version=1, c(%s), ok=ok, cancel=cancel)",
                              commaCollapsed(variableNames_numeric))
  }
  else {
    expressionLine <- ".ganttPlotSamples(version=2, bottle, duration, ok=ok, cancel=cancel)"
  } 
  
  list("_expr"=stringToExpression(expressionLine))
}

# .manipulateControlArguments --------------------------------------------------
.manipulateControlArguments <- function(version, n=0, durations_min)
{
  controlArguments <- list()
  
  if (version == 1) {
    
    variableNames_numeric <- .variableNames(n, "n")
    
    # add n sliders
    for (i in 1:n) {      
      label <- paste("\"length\" bottle", i, "(0 = discard)")
      maxValue <- floor(durations_min[i])
      bottleSlider <- slider(0, maxValue, maxValue, label, step=1)
      controlArguments[[variableNames_numeric[i]]] <- bottleSlider
    }    
  }
  else {
    
    choices <- paste(1:n)
    pickerArgs <- c(as.list(choices), initial=choices[1], label="bottle")
    
    controlArguments[["bottle"]] <- do.call(picker, pickerArgs)
    controlArguments[["duration"]] <- slider(0, 1, 1, "duration", step=0.01)    
  }
  
  return (controlArguments)
}

# .ganttPlotSamples ------------------------------------------------------------
.ganttPlotSamples <- function
(
  version = 1, 
  ### 1 or 2
  ...,
  ### arguments passed to \code{\link{ganttPlotSamples_v1}} or 
  ### \code{\link{ganttPlotSamples_v2}}
  ok,
  cancel
)
{
  variableName <- "bottleEvents.tmp"
  
  bottleEvents <- manipulatorGetState(variableName)
  
  if (is.null(bottleEvents)) {
    
    bottleEvents <- getGlobally("bottleEvents")
  }  
  
  if (ok || cancel) {
    
    if (ok) {
      
      variableNameUser <- "bottleEvents.user"
      
      assignGlobally(variableNameUser, manipulatorGetState(variableName))
      
      cat("*** The global variable", variableNameUser, "has been set.\n")
    }    
    
    grDevices::dev.off()
    
    return()
  } 
  
  bottleEvents <- if (version == 1) {
    
    ganttPlotSamples_v1(bottleEvents, ...)
    
  } else {
    
    ganttPlotSamples_v2(bottleEvents, ...)
  }        
  
  manipulatorSetState(variableName, bottleEvents)    
}

# ganttPlotSamples_v1 ----------------------------------------------------------
ganttPlotSamples_v1 <- function # ganttPlotSamples_v1
### ganttPlotSamples_v1
(
  bottleEvents, 
  ### data frame as returned by \code{\link{samplingEventsToBottleEvents}}, with
  ### begin and end of time intervals represented by an auto sampler's bottle.
  endTimeOffsets
  ### vector of integer offsets determining new end times for the bottle events
  ### by: bottleEvents$tEnd <- bottleEvents$tBeg + 60*endTimeOffsets - 1
)
{
  bottleEvents$tEnd <- bottleEvents$tBeg + 60*endTimeOffsets - 1
  bottleOk <- bottleEvents$tEnd >= bottleEvents$tBeg
  ganttPlotEvents(bottleEvents[bottleOk, ])
  
  return (bottleEvents)
}

# ganttPlotSamples_v2 ----------------------------------------------------------
ganttPlotSamples_v2 <- function # ganttPlotSamples_v2
### ganttPlotSamples_v2
(
  bottleEvents,
  ### data frame as returned by \code{\link{samplingEventsToBottleEvents}}, with
  ### begin and end of time intervals represented by an auto sampler's bottle.  
  bottle,
  ### bottle number
  duration
  ### "duration" of "bottle event"
)
{
  bottle <- as.integer(bottle)
  
  cat(sprintf("ganttPlotSamples_v2(bottle=%d, duration=%f)\n",
              bottle, duration))
  
  bottleEvents$tEnd[bottle] <- bottleEvents$tBeg[bottle] + 
    60*(duration*bottleEvents$dur_minutes[bottle]) - 1
  
  bottleOk <- bottleEvents$tEnd >= bottleEvents$tBeg
  
  ganttPlotEvents(bottleEvents[bottleOk, ])
  
  return (bottleEvents)
}

# .variableNames ---------------------------------------------------------------
.variableNames <- function(numberOfValues, variableName="v")
{
  paste(variableName, 1:numberOfValues, sep="")  
}
