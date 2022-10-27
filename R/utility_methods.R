
#' Add a plate to a plateset
#'
#' @param plateset a plateset S4 bject
#' @param plate an plate S4 object to add to the plateset
#'
#'
#' @export
addPlate <- function(plateset, plate, label) {
  plateset@plateNames <- c(plateset@plateNames, label)
  plateset@plates[[label]] <- plate
  plateset@plateIdList <- c(plateset@plateIdList, plate@plateId)
  return(plateset)
}



#' Transforms plate data based on a supplied plate set with a reference.
#'
#' @param plateSet a plateset with at least one reference and one experimental plate
#' @param method a transformation method, one of c('log2ratio', 'pct_change')
#' @param bkgrdCorr a boolean that controls if there is subtraction of median background.
#'
#' @return returns a PlateSet object with an additional set of transformed data.
#' @export
#'
#' @examples
dataTransform <- function(plateSet, method, bkgrdCorr) {
  if(length(plateSet@plates) %% 2 == 0) {
    for(i in 1:(length(plateSet@plates) - 1)) {
      refPlate <- plateSet@plates[[i]]
      exptPlate <- plateSet@plates[[i+1]]
      tfLabel <- paste0(names(plateSet@plates)[i+1], '_vs_' ,names(plateSet@plates)[i], '_(', method, ')')
      if(method == 'log2ratio') {
        tfd <- data.frame(log(exptPlate@plateData/refPlate@plateData,2), check.names = F)
        plateSet@transformedPlateData[[tfLabel]] <- tfd
        plateSet@transformationNames <- c(plateSet@transformationNames, tfLabel)
      } else if(method == 'pct_change') {

      }
    }
  }
  return(plateSet)
}



#' rawPlateMatrixStatToPlateFormat takes a plate object and returns a matrix formatted
#' according to plate dimensions containing data for a specific measured parameter
#'
#' @param plate The plate containing the data
#' @param paramater the measured parameter to report on
#'
#' @return returns a matrix with dimensions corresponding to plate type. A01 is in upper left.
#'
#' @examples
rawPlateMatrixStatToPlateFormat <- function(plate, parameter) {
  plateData <- as.numeric(plate@plateData[,parameter])
  map = ""
  if(length(plateData) == 384) {
    rc <- getRowAndColumnNames(384)
    plateFormatData <- matrix(plateData, ncol=24, nrow=16, byrow = T)
    colnames(plateFormatData) <- rc[['cols']]
    rownames(plateFormatData) <- rc[['rows']]
  }
  return(plateFormatData)
}


#' rawPlateMatrixStatsToPlateFormat takes a plate object and returns a list of matrix objects, each formatted
#' according to plate dimensions. The list of matrix objects is named according to a supplied vector of measured parameters
#'
#' @param plate The plate containing the data
#' @param paramaters the vector of measured parameter to report on
#'
#' @return Returns a list of plate matrix objects, named by each of the measured parameters in parameter
#'
#' @examples
rawPlateMatrixStatsToPlateFormat <- function(plate, parameters) {

  plateData <- plate@plateData[parameters]

  map = list()
  if(nrow(plateData) == 384) {

    rc <- getRowAndColumnNames(384)
    i = 0
    for(stat in parameters) {
      i = i + 1
      plateFormatData <- matrix(unlist(plateData[,i]), ncol=24, nrow=16, byrow = T)
      colnames(plateFormatData) <- rc[['cols']]
      rownames(plateFormatData) <- rc[['rows']]
      map[[stat]] <- plateFormatData
    }
  }
  return(map)
}



#' getEdgeWellTrimmedMatrixFromPlate is a utility method to specifically exclude wells that are on the edge of a plate.
#' This is a very specific action for certain data sets where edge wells are not used due to conditions like evaporation.
#' @param plate The plate containing the data to be trimmed
#'
#' @return A copy of the plate containing data for the remaining wells.
#'
#' @examples
getEdgeWellTrimmedMatrixFromPlate <- function(plate) {
  droppedWellIds <- grep("01", plate@wellIds)
  droppedWellIds <- union(grep("A", plate@wellIds), droppedWellIds)
  droppedWellIds <- union(grep("24", plate@wellIds), droppedWellIds)
  droppedWellIds <- union(grep("P", plate@wellIds), droppedWellIds)
  plate@plateData <- plate@plateData[-droppedWellIds,]
  return(plate)
}


#' getRowAndColumnNames is a utility method that returns the plate row and column labels
#'
#' @param plateDim dimenstion of the plate
#'
#' @return returns a list with two entries with keys 'rows' and 'cols'
#'
#' @examples
getRowAndColumnNames <- function(plateDim = 384) {
  rows <- LETTERS[seq(from = 1, to = 16)]
  cols <- seq(from = 1, to = 24)
  randc <- list()
  randc[["rows"]] <- rows
  randc[["cols"]] <- cols
  return(randc)
}


#' getPlateValueCutoffs a utility method that returns an upper and lower bound of values in the input matrix
#' given the maxSaturationCount which defines how many data elements may exceed the bounds.
#' Note that the staturated (beyond bound values) are split at the low and high ends of the bounds.
#' That means that if 10 elements are allowed to 'saturate' (exceed bounds), 5 fall off the high end and 5 fall off the low end.
#'
#' The utility of this method is to set reasonable color scale bounds for heatmaps.
#'
#' @param plateMatrix input plate dimension matrix
#' @param maxSaturationCount the number of points (plate wells) to fall off the returned scale (half will be off high end, half of these off the low end)
#'
#' @return returns a vector with two numeric values, a low-end cutoff and a high-end cutoff, in that order
#' @examples
getPlateValueCutoffs <- function(plateMatrix, maxSaturationCount = 10) {
  vals <- unlist(plateMatrix)
  vals2 <- na.omit(vals)
  valSort <- sort(vals2)
  lowBall <- valSort[maxSaturationCount/2]
  hiBall <- valSort[length(valSort)-(maxSaturationCount/2)]
  return(c(lowBall, hiBall))
}



