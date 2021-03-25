# openBottleSelector -----------------------------------------------------------
#' Open Bottle Selecter
#'
#' @param bottleEvents data frame as returned by \code{\link{samplingEventsToBottleEvents}}, 
#' with begin and end of time intervals represented by an auto sampler's bottle. 
#' @param version 1 or 2 (default: 2), used to switch between different plotting 
#' options
#' @return opens bottle selector
#' @export
#' @importFrom manipulate button manipulate
openBottleSelector <- function(bottleEvents, version = 2) {
  n <- nrow(bottleEvents)
  
  manipulate.args <- c(
    .expressionPlotFunctionCall(version, n),
    .manipulateControlArguments(version, n, bottleEvents$dur_minutes),
    list(ok= manipulate::button(label="Ok"),
         cancel= manipulate::button(label="Cancel")))
  
  do.call(manipulate::manipulate, manipulate.args)
  
  #manipulatorSetState("bottleEvents.tmp", bottleEvents)
}

# .expressionPlotFunctionCall --------------------------------------------------
#' .expressionPlotFunctionCall
#'
#' @param version version
#' @param n n
#'
#' @return plot
#'
#' @keywords internal
#' @noRd
#' @importFrom kwb.utils stringToExpression
#' 
.expressionPlotFunctionCall <- function(version, n=0)
{
  if (version == 1) {
    variableNames_numeric <- .variableNames(n, "n")
    expressionLine <- sprintf(".ganttPlotSamples(version=1, c(%s), ok=ok, cancel=cancel)",
                              kwb.utils::commaCollapsed(variableNames_numeric))
  }
  else {
    expressionLine <- ".ganttPlotSamples(version=2, bottle, duration, ok=ok, cancel=cancel)"
  } 
  
  list("_expr"=kwb.utils::stringToExpression(expressionLine))
}

# .manipulateControlArguments --------------------------------------------------
#' .manipulateControlArguments
#'
#' @param version version
#' @param n n
#' @param durations_min durations_min 
#'
#' @return ???
#' @keywords internal
#' @noRd
#' @importFrom manipulate picker slider
.manipulateControlArguments <- function(version, n=0, durations_min)
{
  controlArguments <- list()
  
  if (version == 1) {
    
    variableNames_numeric <- .variableNames(n, "n")
    
    # add n sliders
    for (i in 1:n) {      
      label <- paste("\"length\" bottle", i, "(0 = discard)")
      maxValue <- floor(durations_min[i])
      bottleSlider <- manipulate::slider(0, maxValue, maxValue, label, step=1)
      controlArguments[[variableNames_numeric[i]]] <- bottleSlider
    }    
  }
  else {
    
    choices <- paste(1:n)
    pickerArgs <- c(as.list(choices), initial=choices[1], label="bottle")
    
    controlArguments[["bottle"]] <- do.call(manipulate::picker, pickerArgs)
    controlArguments[["duration"]] <- manipulate::slider(0, 1, 1, "duration", step=0.01)    
  }
  
  return (controlArguments)
}

# .ganttPlotSamples ------------------------------------------------------------
#' .ganttPlotSamples
#'
#' @param version version
#' @param ... ...
#' @param ok ok
#' @param cancel cancel
#'
#' @return plot
#' @keywords internal
#' @noRd
#' @importFrom grDevices dev.off
#' @importFrom manipulate manipulatorGetState manipulatorSetState
#' @importFrom kwb.utils assignGlobally getGlobally
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
  
  bottleEvents <- manipulate::manipulatorGetState(variableName)
  
  if (is.null(bottleEvents)) {
    
    bottleEvents <- kwb.utils::getGlobally("bottleEvents")
  }  
  
  if (ok || cancel) {
    
    if (ok) {
      
      variableNameUser <- "bottleEvents.user"
      
      kwb.utils::assignGlobally(variableNameUser, 
                                manipulate::manipulatorGetState(variableName))
      
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
  
  manipulate::manipulatorSetState(variableName, bottleEvents)    
}

# ganttPlotSamples_v1 ----------------------------------------------------------
#' Gantt Plot Samples (version 1)
#'
#' @param bottleEvents data frame as returned by \code{\link{samplingEventsToBottleEvents}}, 
#' with begin and end of time intervals represented by an auto sampler's bottle. 
#' @param endTimeOffsets vector of integer offsets determining new end times for 
#' the bottle events by: bottleEvents$tEnd <- bottleEvents$tBeg + 60*endTimeOffsets - 1
#' 
#' @return ganttPlotSamples version 1
#' @export
#' @importFrom kwb.event ganttPlotEvents
ganttPlotSamples_v1 <- function (bottleEvents, endTimeOffsets) {
  bottleEvents$tEnd <- bottleEvents$tBeg + 60*endTimeOffsets - 1
  bottleOk <- bottleEvents$tEnd >= bottleEvents$tBeg
  kwb.event::ganttPlotEvents(bottleEvents[bottleOk, ])
  
  return (bottleEvents)
}

# ganttPlotSamples_v2 ----------------------------------------------------------
#' Gantt Plot Samples (version 2)
#'
#' @param bottleEvents data frame as returned by \code{\link{samplingEventsToBottleEvents}}, 
#' with begin and end of time intervals represented by an auto sampler's bottle.  
#' @param bottle bottle number
#' @param duration "duration" of "bottle event"
#' @return ganttPlotSamples version 2
#' @export
#' @importFrom kwb.event ganttPlotEvents
ganttPlotSamples_v2 <- function (bottleEvents, bottle, duration)
{
  bottle <- as.integer(bottle)
  
  cat(sprintf("ganttPlotSamples_v2(bottle=%d, duration=%f)\n",
              bottle, duration))
  
  bottleEvents$tEnd[bottle] <- bottleEvents$tBeg[bottle] + 
    60*(duration*bottleEvents$dur_minutes[bottle]) - 1
  
  bottleOk <- bottleEvents$tEnd >= bottleEvents$tBeg
  
  kwb.event::ganttPlotEvents(bottleEvents[bottleOk, ])
  
  return (bottleEvents)
}

# .variableNames ---------------------------------------------------------------
.variableNames <- function(numberOfValues, variableName="v")
{
  paste(variableName, 1:numberOfValues, sep="")  
}
