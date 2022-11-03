
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



#' missingDataReport reports on plate missing values
#'
#' @param plate ASO plate
#' @param plateSize plate size, e.g. 384 (default)
#'
#' @return returns a dataframe with columns 'parameter', 'good_count', 'missing_count', 'na_count', 'zero_count'
#' @export
#'
#' @examples
missingDataReport <- function(plate, plateSize = 384) {

  df <- plate@plateData
  if(colnames(df)[1] == "well_id") {
    df <- df[,2:ncol(df)]
  }

  # coerce NA for non-numeric data
  suppressWarnings(
    df <- data.frame(lapply(df, as.numeric), check.names = F)
  )
  res <- data.frame(colSums(is.na(df)), check.names=F)
  res2 <- data.frame(colSums(df == 0), check.names=F)
  res <- cbind(res, res2)

  res$missing_count <- res[,1] + res[,2]
  res$good_count <- 384 - res$missing_count
  res$parameter <- rownames(res)

  res <- res[,c(5,4,3,1,2)]

  colnames(res) <- c("parameter", "good_count", "missing_count", "na_count", "zero_count")

  return(res)
}


#' platePairMissingDataReport
#'
#' @param plateset a plateset object containing at least two plates
#' @param platePair a vector of plate names to compare
#' @param plateSize plate well count, e.g. 384, 96..
#'
#' @return returns a dataframe that summarizes the union of missing data across two referenced plates
#' @export
#'
#' @examples
platePairMissingDataReport <- function(plateset, platePair, plateSize = 384) {
    # transform to plate dimensions
    # matrix multiply to get a result on which to report on 0's and NAs using missingDataReport on cloned plate
}


#' encodeDiscreteParameters
#'
#' @param plate plate containing the data to encode
#' @param parameter parameter that requires encoding
#' @param textValues expected initial discreate values
#' @param numericValues replacement numeric values
#'
#' @return returns a copy of the plate having value substitutions for the discrete parameter
#' @export
#'
#' @examples
encodeDiscreteParameters <- function(plate, parameter, textValues, numericValues) {
  i = 1

  vals <- plate@plateData[,parameter]

  for(val in textValues) {
    numVal = numericValues[i]
    vals <- gsub(val, numVal, vals)

    plate@plateData[parameter] <- vals
    i = i + 1
  }

  return(plate)
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
dataTransform <- function(plateSet, platePair, method = 'log2ratio',
                          bkgrdCorr=F, bkgrdSamples = NULL, bkgrdMode = 'median', firstDataCol = 2) {

  bkgrndCorrected = F

  if(bkgrdCorr && !is.null(bkgrdSamples) && length(bkgrdSamples) > 0) {


    bkgrndCorrected = T
  }

  if(length(plateSet@plates) %% 2 == 0 && !is.null(platePair) && length(platePair) == 2) {
      refPlate <- plateSet@plates[[platePair[1]]]
      exptPlate <- plateSet@plates[[platePair[2]]]

      # plateData may include wellIds in first column, so for numeric transformation, we need to skip first column using firstDataCol
      refData <- refPlate@plateData[,firstDataCol:ncol(refPlate@plateData)]
      exptData <- exptPlate@plateData[,firstDataCol:ncol(exptPlate@plateData)]

      suppressWarnings(
      refData <- sapply(refData, 'as.numeric')
      )
      suppressWarnings(
      exptData <- sapply(exptData, 'as.numeric')
      )
      tfLabel <- paste0(platePair[2], '_vs_' , platePair[2], '_(', method, ')')
      if(method == 'log2ratio') {
        tfd <- data.frame(log(exptData/refData,2), check.names = F)
        plateSet@transformedPlateData[[tfLabel]] <- tfd
        plateSet@transformationNames <- c(plateSet@transformationNames, tfLabel)
      } else if(method == 'pct_change') {

      }

  }
  return(plateSet)
}

backgroundCorrection <- function(plate, bkgrdSamples = NULL, bkgrdMode = 'median') {

  if(!is.null(bkgrdSamples) && length(bkgrdSamples) > 0) {
    bkgrdData = c()
    for(sampleName in bkgrdSamples) {
      getPlateDataForSampleAllParameters(plate, sampleName)

    }

  }
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

    rc <- aso:::getRowAndColumnNames(384)
    i = 0
    for(stat in parameters) {
      i = i + 1
      plateFormatData <- data.frame(matrix(unlist(plateData[,i]), ncol=24, nrow=16, byrow = T))
      suppressWarnings(
        plateFormatData <- data.frame(lapply(plateFormatData, as.numeric))
      )
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


getPlateDataForSample <- function(plate, sampleName, parameter) {
  data <- plate@plateData
  data <- data[parameter]
  data$wellId <- plate@wellIds
  annot <- plate@plateAnnotMap
  ann <- annot[annot$Compound == sampleName,]
  # appending the data to the annotation subset
  ann[[parameter]] <- unlist(data[data$wellId %in% ann$Well,][1])

  return(ann)
}

getPlateDataForSampleAllParameters <- function(plate, sampleName) {
    # plate data and platemap annotations
    data <- plate@plateData
    annot <- plate@plateAnnotMap

    # add well ids to the data
    data$wellId <- plate@wellIds

    # get the annotations associated with the sample name
    ann <- annot[annot$Compound == sampleName,]

    # appending the data to the annotation subset, note that well ids are used as data keys
    ann <- cbind(ann, unlist(data[data$wellId %in% ann$Well,]))

    # return the annotations and attached data columns for all parameters and the specified sampleName
    return(ann)
}


clonePlate <- function(plate) {
  newPlate <- new("plate")
  newPlate@plateId <- plate@plateId
  newPlate@statList <- plate@statList
  newPlate@format <- plate@format
  newPlate@wellIds <- plate@wellIds
  newPlate@plateData <- plate@plateData
  newPlate@plateDataMap <- plate@plateDataMap
  newPlate@plateAnnotMap <- plate@plateAnnotMap
  newPlate@parameters <- plate@parameters
  newPlate@imputationFlag <- plate@imputationFlag
  newPlate@imputationMethod <- plate@imputationMethod
  newPlate@qualityFlags <- plate@qualityFlags
  # newPlate@corrPlateData <- plate@corrPlateData
  # newPlate@correctionProcesses <- plate@correctionProcesses
  return(newPlate)
}




