#' Starts a log file.
initializeLog <- function(logLevel="INFO") {
  logger::log_level(logLevel)
  logFileName <- paste0(getwd(),"/ASO_Log_",format(Sys.time(), "%Y%m%d_%H%M"), ".log")
  logger::log_appender(logger::appender_file(logFileName))

  logger::log_info("ASO Analysis Started -- Log Initialized")
}

idleLog <- function() {
  logger::log_level(logger::ERROR)
  logFileName <- paste0(getwd(), "/idle_log.log")
  logger::log_appender(logger::appender_tee(logFileName))
}

initializedExportLocation <- function(aso) {
  outDir <- constructFileName(baseFileName = paste0(aso@methodParameters$analysis_name, "_ASO_Analysis"))
  path <- paste0(getwd(), "/", outDir)
  if(!dir.exists(path)) {
    dir.create(path)
  }
  setwd(path)
  return(path)
}

constructFileName <- function(baseFileName, plateReadLabel = "", fileExtension = "", appendTimeStamp=T, appendSubdir=NULL) {
  fileName = baseFileName
  if(nchar(plateReadLabel) > 0) {
    fileName <- paste0(fileName, "_", plateReadLabel)
  }
  if(appendTimeStamp) {
    fileName <- paste0(fileName, "_", format(Sys.time(), "%Y%m%d_%H%M"))
  }
  fileName <- paste0(fileName, ".", fileExtension)

  return(fileName)
}

#' Add a plate to a plateset
#'
#' @param plateset a plateset S4 object
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

#' @title subsetPlateSet
#' @description
#'    Reduce platesSet object to the specified plate indices. This utility method is intended to be run prior to generation of transformed data and QC.
#'
#' @param plateSet The plateset to subset
#' @param plateIndicesToKeep The indices with in the plate collection to keep in the new PlateSet object
#'
#' @export
subsetPlateSet <- function(plateSet, plateIndicesToKeep) {

  plateSet@plateIdList <- plateSet@plateIdList[plateIndicesToKeep]
  plateSet@plates <- plateSet@plates[plateIndicesToKeep]
  plateSet@plateNames <- plateSet@plateNames[plateIndicesToKeep]

  print(length(plateSet@plates))

  return(plateSet)
}


#' filterLowDataParameters
#'
#' @param plate plate on which to filter parameters with low data representation
#' @param thresholdType one of 'well_count' or 'well_percentage'
#' @param missingDataThreshold
#'
#' @return a plate with updated plate data
#' @export
filterLowDataParameters <- function(plate, thresholdType = 'well_count', missingDataThreshold = 10) {

  missingData <- missingDataReport(plate)
  missingData <- missingData[missingData$missing_count > missingDataThreshold, ]

  if(nrow(missingData) > 0) {
    paramsToDrop <- missingData$parameter
    df <- plate@plateData[,!(names(plate@plateData) %in% paramsToDrop)]
    plate@plateData <- df
  }
  return(plate)
}


#' Utility method to replace blank and NA data with replacement values.
#' @param plateSet the plateset object to work on
#' @param blankReplacements a list object with parameter abbreviation keys and replacement values
#' @param naRelacements a list object with parameter abbreviations keys and replacement values
#' @return a plateset object with plate data replaced according to input values
replaceBlanksAndNAs <- function(plateSet, blankReplacements, naReplacements) {

  # run replacement for each plate
  # extract plate data for the plate
  # for each column name in replacment map, drop in value for blank or na relapcements as specified

  for(plateName in names(plateSet@plates)) {
    plate <- plateSet@plates[[plateName]]
    df = plate@plateData

    if(length(blankReplacements) > 0) {
      for(blankCol in names(blankReplacements)) {

        repVal = blankReplacements[[blankCol]]

        if(blankCol %in% colnames(df)) {
          vals <- df[[blankCol]]
          vals[vals == "Blank"] <- repVal

          df[[blankCol]] <- vals
        }
      }
    }

    if(length(naReplacements) > 0) {
      for(naCol in names(naReplacements)) {
        repVal = naReplacements[[naCol]]

        if(naCol %in% colnames(df)) {
          vals <- df[[naCol]]
          vals[vals == "N/A"] <- repVal

          df[[naCol]] <- vals
        }
      }
    }

    plate@plateData <- df
    plateSet@plates[[plateName]] <- plate
  }
  return(plateSet)
}

#' Runs a quality control process on the reference plate read. The method will export a excel report file.
#' @param plateSet the plate set to run QC on
#' @param rootExportDirectory the directory for report output.
#' @param plateFile the plate file name so that the report can capture the input plate file name.
runReferenceQC <- function(plateSet, rootExportDirectory, plateFile) {
  refPlate <- plateSet@plates[[1]]

  plateAnnot <- plateSet@plates[[1]]@plateAnnotMap

  df <- refPlate@plateData
  df <- sapply(df, as.numeric)
  means <- colMeans(na.omit(df))
  sds <- apply(na.omit(df), FUN=sd, MARGIN=2)
  cvs <- sds/means

  zDf <- sweep(df,2,means)
  zDf <- sweep(zDf,2,sds, FUN='/')
  zDf <- data.frame(zDf)

  colNames = colnames(zDf)
  zMats <- list()
  aveMat <- ""

  for(i in 1:ncol(zDf)) {
    m1 <- data.frame(matrix(zDf[,i], nrow=16, byrow = T))
    colnames(m1) <- as.numeric(c(1:24))
    rownames(m1) <- LETTERS[1:16]

    zMats[[colNames[i]]] <- m1

    if(i == 1) {
      aveMat <- abs(m1)
    } else {
      aveMat <- aveMat + abs(m1)
    }
  }

  # heres the average absolute z-scored per well
  aveMat <- aveMat/length(zMats)

  zNcol = ncol(zDf)

  # add number of parameters that exceed 1SD absolute for each well
  z1SDsums <- zDf > 1.0 | zDf < -1.0
  z1SDsums <- rowSums(z1SDsums)
  z3SDsums <- zDf > 3.0 | zDf < -3.0
  z3SDsums <- rowSums(z3SDsums)

  # add the median absolute z-score as a column in zDf
  rowMedians <- apply(abs(zDf), MARGIN = 1, FUN=median)
  zDf$abs_CV_Median <- unlist(rowMedians)

  # add the frequency GT 1SD
  zDf$freq_GT_1SD <- z1SDsums
  zDf$fract_param_GT_1SD <- signif(zDf$freq_GT_1SD/(zNcol*1.0), digits=3)

  zDf$freq_GT_3SD <- z3SDsums
  zDf$fract_param_GT_3SD <- signif(zDf$freq_GT_3SD/(zNcol*1.0), digits=3)


  # make an absolute median plate zscore matrix
  zAbsMedianPlateDf <- data.frame(matrix(zDf$abs_CV_Median, nrow=16, byrow = T))
  colnames(zAbsMedianPlateDf) <- as.numeric(c(1:24))
  rownames(zAbsMedianPlateDf) <- LETTERS[1:16]

  rowSpacing = 2

  wb = openxlsx::createWorkbook()
  headerStyle = openxlsx::createStyle(textDecoration = "Bold", fontSize=14)
  naString = "Masked"
  openxlsx::addWorksheet(wb, "zScore_Data")
  rowNum = 1

  # add the mean zscore result
  openxlsx::writeData(wb=wb, sheet=1, "Cross-Parameter Average Absolute Z-score", startRow=rowNum, colNames=F, rowNames =F, headerStyle = headerStyle)
  openxlsx::addStyle(wb=wb, sheet=1, style=headerStyle, rows=rowNum, cols=1)
  rowNum = rowNum + 1
  openxlsx::writeData(wb=wb, sheet=1, aveMat, startRow=rowNum, colNames=T, rowNames =T, keepNA = T, na.string=naString)

  # add a row for col header
  rowNum = rowNum + 1
  lowerColorLim <- quantile(aveMat, probs = c(0.05), na.rm=T)
  upperColorLim <- quantile(aveMat, probs = c(0.95), na.rm=T)
  openxlsx::conditionalFormatting(wb, sheet=1, type="colorScale", style = c('#ebf6f7','#4491fc'), rows = rowNum:(nrow(aveMat) + rowNum), cols = 2:(ncol(aveMat)+1), rule=c(lowerColorLim, upperColorLim))

  # move down to next chunk
  rowNum = rowNum + nrow(aveMat) + rowSpacing

  openxlsx::writeData(wb=wb, sheet=1, "Cross-Parameter Median Absolute Z-score", startRow=rowNum, colNames=F, rowNames =F, headerStyle = headerStyle)
  openxlsx::addStyle(wb=wb, sheet=1, style=headerStyle, rows=rowNum, cols=1)
  rowNum = rowNum + 1
  openxlsx::writeData(wb=wb, sheet=1, zAbsMedianPlateDf, startRow=rowNum, colNames=T, rowNames =T, keepNA = T, na.string=naString)

  # add a row for col header
  rowNum = rowNum + 1
  lowerColorLim <- quantile(zAbsMedianPlateDf, probs = c(0.05), na.rm=T)
  upperColorLim <- quantile(zAbsMedianPlateDf, probs = c(0.95), na.rm=T)
  openxlsx::conditionalFormatting(wb, sheet=1, type="colorScale", style = c('#ebf6f7','#4491fc'), rows = rowNum:(nrow(aveMat) + rowNum), cols = 2:(ncol(aveMat)+1), rule=c(lowerColorLim, upperColorLim))

  # move down to next chunk
  rowNum = rowNum + nrow(aveMat) + rowSpacing

  for(zMatName in names(zMats)) {
    zname <- paste0(zMatName, " Z-score")
    zMat <- zMats[[zMatName]]
    openxlsx::writeData(wb=wb, sheet=1, zname, startRow=rowNum, colNames=F, rowNames =F, headerStyle = headerStyle)
    openxlsx::addStyle(wb=wb, sheet=1, style=headerStyle, rows=rowNum, cols=1)
    rowNum = rowNum + 1
    openxlsx::writeData(wb=wb, sheet=1, zMat, startRow=rowNum, colNames=T, rowNames =T, keepNA = T, na.string = naString)

    # add a row for col header
    rowNum = rowNum + 1

    lowerColorLim <- quantile(zMat, probs=(0.05), na.rm=T)
    upperColorLim <- quantile(zMat, probs=(0.95), na.rm=T)

    openxlsx::conditionalFormatting(wb, sheet=1, type="colorScale", style = c('#ffcf40','#ebf6f7', '#4491fc'), rows = rowNum:(nrow(zMat)+rowNum), cols = 2:(ncol(zMat)+1), rule=c(lowerColorLim, 0.0, upperColorLim))

    rowNum = rowNum + nrow(zMat) + rowSpacing
  }

  # add the z-score unpivoted matrix
  openxlsx::addWorksheet(wb, "Unpivoted_Zscore_Matrix")
  zDf <- cbind(plateAnnot, zDf)
  rownames(zDf) <- refPlate@wellIds
  openxlsx::writeData(wb=wb, sheet=2, zDf, startRow=1, colNames=T, rowNames = T, keepNA = T, na.string=naString)
  openxlsx::freezePane(wb=wb, sheet=2, firstRow=T, firstCol=T)

  # conditional formatting
  lowerColorLim <- quantile(unlist(zDf$abs_CV_Median), probs = c(0.05), na.rm=T)
  upperColorLim <- quantile(unlist(zDf$abs_CV_Median), probs = c(0.95), na.rm=T)
  openxlsx::conditionalFormatting(wb, sheet=2, type="colorScale", style = c('#ebf6f7','#4491fc'), rows = 2:(nrow(zDf) + 2), cols = (ncol(zDf) - 1), rule=c(lowerColorLim, upperColorLim))

  # lets add the cvs summary data
  cvs <- data.frame(cvs)
  colnames(cvs) <- c('Coefficient of Variation')
  openxlsx::addWorksheet(wb, "Coef_of_Variation_Summary")
  openxlsx::writeData(wb=wb, sheet=3, paste0("Plate Input File: ",plateFile), startRow=1, colNames=F, rowNames=F)
  openxlsx::writeData(wb=wb, sheet=3, cvs, startRow=2, colNames=T, rowNames = T)

  fileName <- format(Sys.time(), "Reference_Plate_QC_%Y%m%d_%H%M.xlsx")

  fileName <- constructFileName(baseFileName = 'Reference_Plate_QC', fileExtension = 'xlsx')

  # fileName <- paste0(rootExportDirectory, "/", fileName)

  openxlsx::saveWorkbook(wb,file=fileName, overwrite = T)
}


#' This method performs the imputation method on missing data.
#' This is not typically part of the current data processing steps.
#' @param plateSet input plateset to work on
#' @param pctMin take this percentile, input as a fraction, to select the percentile minimum values. Default is 0.01, lowest 1 percent.
#' @param colsToImpute the set of parameters or parameter abbreviations to impute.
#' @return returns a plateSet object with imputed data values.
imputePercentMin <- function(plateSet, pctMin = 0.01, colsToImpute) {
  plates <- plateSet@plates

  for(plateName in names(plates)) {
    plate <- plates[[plateName]]
    data <- plate@plateData
    d2 <- sapply(data, FUN=as.numeric, axis=1)
    d2 <- data.frame(d2)

    # does this ignore NaNs
    minVal <- sapply(d2, axix=1, FUN=min, c(na.rm=T))

    for(col in 1:ncol(d2)) {
      colName <- colnames(d2)[col]
      d <- na.omit(unlist(d2[,col]))
      d <- d[d!=0]
      min <- min(d)
      repVal <- min * pctMin

      colData <- unlist(d2[,col])

      if(colName %in% colsToImpute$na_impute) {
        colData[is.na(colData)] <- repVal
      }

      if(colName %in% colsToImpute$zero_impute) {
        colData[colData == 0.0] <- repVal
      }

      d2[,col] <- colData
    }

    # put the updated plate data back...
    plate@plateData <- d2
    plates[[plateName]] <- plate
  }
  # drop in the modified plates with mods to plateData
  plateSet@plates <- plates

  return(plateSet)
}





#' missingDataReport reports on plate missing values
#' @param plate ASO plate
#' @param plateSize plate size, e.g. 384 (default)
#' @return returns a dataframe with columns 'parameter', 'good_count', 'missing_count', 'na_count', 'zero_count'
#' @export
missingDataReport <- function(plate, plateSize = 384) {

  df <- plate@plateData
  if(colnames(df)[1] == "well_id") {
    df <- df[,2:ncol(df)]
  }

  # coerce NA for non-numeric data
  #suppressWarnings(
  #  df <- data.frame(lapply(df, as.numeric), check.names = F)
  #)
  res <- data.frame(colSums(df == "N/A"), check.names=F)
  res2 <- data.frame(colSums(df == "Blank"), check.names=F)
  res <- cbind(res, res2)

  colnames(res) <- c("NA_Count", "Blank_Count")

  res$Missing_Count <- res[,1] + res[,2]
  res$Well_Count <- 384
  res$Empty_Wells <- colSums(df == "Empty")
  res$Masked_Wells <- colSums(df == "Masked")
  res$Keeper_Wells <- (res$Well_Count - res$Empty_Wells) - res$Masked_Wells
  res$Good_Count <- res$Keeper_Wells - res$Missing_Count
  res$Parameter <- rownames(res)
  # reorganize column order
  res <- res[,c(9,4,5,6,7,1,2,3,8)]
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
encodeDiscreteParameters <- function(plate, parameter, textValues, numericValues) {
  i = 1
  if(parameter %in% colnames(plate@plateData)){
    vals <- plate@plateData[,parameter]

    for(val in textValues) {
      numVal = numericValues[i]
      vals <- gsub(val, numVal, vals)
      
      plate@plateData[parameter] <- vals
      i = i + 1
    }
  }

  return(plate)
}

#' A utility method to harmonize parameters across plates. Simply makes sure all plate reads contain the same parameters.
#' @param plateSet the plateset to harmonize
#' @returns a plateset object with various plate reads containing the same, consistent variables.
#'
harmonizeParametersAcrossPlates <- function(plateSet) {
  commonParams <- c()
  commonParamNames <- c()
  i = 1
  for(plate in plateSet@plates) {
    if(i == 1) {
      commonParams <- colnames(plate@plateData)
      commonParamNames <- plate@parameters
    } else {
      params <- colnames(plate@plateData)
      paramNames <- plate@parameters
      commonParams <- intersect(commonParams, params)
      commonParamNames <- intersect(commonParamNames, paramNames)
    }
    i = i + 1
  }

  plateSet@plateNames
  for(name in plateSet@plateNames) {
    plate <- plateSet@plates[[name]]
    plate@plateData <- plate@plateData[,commonParams]
    plate@statList <- colnames(plate@plateData)
    plate@parameters <- commonParamNames
    plateSet@plates[[name]] <- plate
  }
  return(plateSet)
}


#' Transforms plate data based on a supplied plate set with a reference.
#'
#' @param plateSet a plateset with at least one reference and one experimental plate
#' @param method a transformation method, one of c('log2ratio', 'pct_change')
#' @param bkgrdCorr a boolean that controls if there is subtraction of median background.
#'
#' @return returns a PlateSet object with an additional set of transformed data.
#' @export
dataTransform <- function(plateSet, platePair, method = 'log2ratio', bkgrdCorr = F, bkgrdSamples = NULL, bkgrdMode = 'median', firstDataCol = 2) {

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
      tfLabel <- paste0(platePair[2], '_vs_' , platePair[1], '_(', method, ')')
      if(method == 'log2ratio') {
        exptData <- exptData + 0.001
        refData <- refData + 0.001
        tfd <- data.frame(log(exptData/refData,2), check.names = F)
        plateSet@transformedPlateData[[tfLabel]] <- tfd
        plateSet@transformationNames <- c(plateSet@transformationNames, tfLabel)
      } else if(method == 'pct_change') {

      }
  }
  return(plateSet)
}


## Not implemented yet. Intention was to be able to subtract a background value for each parameter
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
getPlateValueCutoffs <- function(plateMatrix, maxSaturationCount = 10) {
  vals <- unlist(plateMatrix)


  vals[!is.finite(vals)] <- NA
  vals2 <- na.omit(vals)

  valSort <- sort(vals2)
  lowBall <- valSort[maxSaturationCount/2]
  hiBall <- valSort[length(valSort)-(maxSaturationCount/2)]
  return(c(lowBall, hiBall))
}

#' Utility method to extract the plate data from a plate, for a given sample name (set of data rows) and parameter name (data column).
#' @param plate the plate object from which to extract data.
#' @param sampleName the name of the sample for which to extract data. Names are specified in the input platemap annotation file.
#' @param parameter the parameter for which to pull data
#' @return returns a data frame with well annotations and corresponding data
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

#' A utility method to report annotations and all data for a given sample
#' @param plate the plate object from which to pull data
#' @param sampleName the name of the sample (compound) for which to pull data.
#' @return a data frame containing annotations and all parameter data for a given sample name
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


#' A utility method to make a copy of a plate object
#' May not be needed since assignments and parameter passing are copying values into a new copy.
#'
#' @param plate the plate to clone
#' @return new plate copy
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


#' Reduces plate data to a set of parameters to keep.
#' @param plate a plate object containing plateData
#' @param paramsToKeep list of parameters to keep/maintain in the plateData, others will be filtered out.
#' @return returns a new plate object with plateData only containing the specified parameteters.
filterParametersOnList <- function(plate, paramsToKeep) {
  df <- plate@plateData
  cnames <- colnames(df)
  dnames <- intersect(paramsToKeep, cnames)
  
  df <- df[,dnames]

  plate@plateData <- df
  return(plate)
}


#' Utility method to replace long parameter names with short abbreviations.
#' The change is made within the plate's plateData object, and the plate's paramAbbrs data field is set.
#' @param plate the plate on which to apply the abbreviations.
#' @param abbrInfo a list object with parameter name keys and corresponding abbreviations as values.
#' @return returns a plate object with plateData that uses abbreviations as column names
applyParameterAbbreviations <- function(plate, abbrInfo) {
  df <- plate@plateData
  params <- colnames(df)
  abbrs <- c()
  for(col in params) {
    if(col=="well_id"){
      abbrs <- c(abbrs, "well_id")
    }else{
      abbr <- abbrInfo[abbrInfo$Statistic == col,2]
      abbrs <- c(abbrs,abbr)
    }
  }

  # set abbrs as colnames
  colnames(df) <- abbrs

  ## if(any(is.na(colnames(df)))){
  ##   drop <- which(is.na(colnames(df)))
  ##   df <- df[,-drop]
  ##   params <- params[-drop]
  ##   abbrs <- abbrs[-drop]
  ## }
  
  # rebuild plate info
  plate@parameters <- params
  plate@statList <- params
  plate@paramAbbrs <- abbrs
  plate@plateData <- df

  return(plate)
}


#' A utility method to replace the input AUC parameter name with a new name.
#' The original text files contain unicode characters that not not well supported.
#' @param aso the aso object to patch AUC parameter names
#' @return returns an aso object with the AUC parameter names patched.
aucParameterPatch <- function(aso) {
  # patch plate data
  for(platename in names(aso@plateSet@plates)) {
    i=1
    plate <- aso@plateSet@plates[[platename]]
    for(n in colnames(plate@plateData)) {
      if(startsWith(n, "Area Under Curve (RFU")) {
        colnames(plate@plateData)[i] <- "Area Under Curve (RFUs)"
        plate@parameters[i] <- "Area Under Curve (RFUs)"
        plate@statList[i] <- "Area Under Curve (RFUs)"
      }
      if(startsWith(n, "Area Under Curve S.D. (RFU")) {
        colnames(plate@plateData)[i] <- "Area Under Curve S.D. (RFUs)"
        plate@parameters[i] <- "Area Under Curve S.D. (RFUs)"
        plate@statList[i] <- "Area Under Curve S.D. (RFUs)"
      }
      i = i + 1
    }

    aso@plateSet@plates[[platename]] <- plate
  }

  # patch plate info
  pInfo <- aso@parameterInfo
  i = 1
  for(i in 1:nrow(pInfo)) {
    n <- pInfo[i,1]
    if(startsWith(n, "Area Under Curve (RFU")) {
      pInfo[i,1] <- "Area Under Curve (RFUs)"
    }
    if(startsWith(n, "Area Under Curve S.D. (RFU")) {
      pInfo[i,1] <- "Area Under Curve S.D. (RFUs)"
    }
    i = i + 1
  }

  aso@parameterInfo <- pInfo
  return(aso)
}


#' retrieve well data based on a collection of specified wells.
#' @param plate a plate object from which to extract a subset of plateData
#' @param wellSet the collection of wells (well ids) to pull data for.
#' @return returns a dataframe object with all parameter data for the specified wells.
getPlateDataByWellSet <- function(plate, wellSet) {
  rows <- match(wellSet, plate@wellIds)
  dataChunk <- plate@plateData[rows,]
  return(dataChunk)
}

#' This function exports a paired t-test result.
#'  @param aso an aso object on which paired ttest has been run
#'  @param ttestResult a dataframe represenint the ttest result
#'  @param fileName the t-test result ouput file name.
exportPairedTTestResult <- function(aso, ttestResult, fileName) {
  # pivot on concentration
  # Sample	Conc	Treatment	Parameter	Condition	TestMethod	T	pValue	log2FoldChange	adjP	-log10(adjP)
  tResSmall <- ttestResult[,c(1,2,4,8,10,9,11)]
  tShortLFC <- reshape2::dcast(tResSmall, Sample + Parameter ~ Conc, value.var='log2FoldChange')
  tShortP <- reshape2::dcast(tResSmall, Sample + Parameter ~ Conc, value.var='pValue')
  tShortAdjP <- reshape2::dcast(tResSmall, Sample + Parameter ~ Conc, value.var='adjP')
  tShortLogFDR <- reshape2::dcast(tResSmall, Sample + Parameter ~ Conc, value.var='-log10(adjP)')

  concCount = ncol(tResSmall)-3
  columnNames = c(colnames(tShortLFC)[1:2],rep(colnames(tShortLFC[3:ncol(tShortLFC)]),4))

  tPivot = tShortLFC
  tPivot = merge(tPivot, tShortP, by.x=c('Sample','Parameter'), by.y=c('Sample','Parameter'), suffixes=c('_LFC', '_pVal'))
  tPivot = merge(tPivot, tShortAdjP, by.x=c('Sample','Parameter'), by.y=c('Sample','Parameter'), suffixes=c('', '_adjP'))
  tPivot = merge(tPivot, tShortLogFDR, by.x=c('Sample','Parameter'), by.y=c('Sample','Parameter'), suffixes=c('', '_-logFDR'))

  colnames(tPivot) <- columnNames

  wb <- openxlsx::createWorkbook()

  ## Add a worksheet
  openxlsx::addWorksheet(wb, "TTest_Results")
  openxlsx::addWorksheet(wb, "TTest_Results_Tall_Format")

  topHeader = c("","",rep("log2FoldChange", concCount),rep("pValue",concCount), rep("adjP",concCount), rep("-log10(adjP)",concCount))
  openxlsx::writeData(wb, sheet=1, x=t(topHeader), startCol=1, startRow=1, rowNames=F, colNames=F)
  openxlsx::writeData(wb, sheet=1, x=tPivot, startCol=1, startRow=2, rowNames=F, colNames=T)
  head(openxlsx::readWorkbook(wb, sheet=1))

  centerStyle <- openxlsx::createStyle(halign = "center")
  colors = c(
    '#D8E4BC',
    '#C5D9F1',
    '#CCC0DA',
    '#D9D9D9'
  )

  startCol = 3
  for(i in c(1:4)) {
    centerColorStyle <- openxlsx::createStyle(halign = "center", fgFill=colors[i])
    prevStart = startCol
    openxlsx::mergeCells(wb, sheet=1, cols = prevStart:(prevStart+concCount-1), rows = 1)
    openxlsx::addStyle(wb, sheet=1, centerColorStyle, rows = 1, cols = prevStart)
    openxlsx::addStyle(wb, sheet=1, centerColorStyle, rows = 2, cols = prevStart:(prevStart+concCount-1))
    startCol = startCol + concCount
  }

  openxlsx::addStyle(wb, sheet=1, centerStyle, rows = 2, cols = 1:2)
  openxlsx::conditionalFormatting(wb, sheet=1, type="colorScale", style = c('#63BE7B','#FFEB84','#F8696B'), rows = 3:nrow(tPivot), cols = 3:(3+concCount-1))

  openxlsx::conditionalFormatting(wb, sheet=1, type="colorScale", style = c('#F8696B', '#EECBC4'), rule=c(0,0.1), rows = 3:nrow(tPivot), cols = (3+2*concCount):((3+(2*concCount)+concCount-1)))
  emptyStyle <- openxlsx::createStyle(bgFill = "#ffffff")

  openxlsx::conditionalFormatting(wb, sheet=1, rows = 3:nrow(tPivot), cols = (3+2*concCount):((3+(2*concCount)+concCount-1)),
                        type = "notContains", rule="", style = emptyStyle)

  openxlsx::conditionalFormatting(wb, sheet=1, rows = 3:nrow(tPivot), cols = (3+2*concCount):((3+(2*concCount)+concCount-1)),
                                  type = "expression", rule=">0.1", style = emptyStyle)

  fileName = format(Sys.time(), "Ttest_Results_%Y%m%d_%H%M.xlsx")

  fileName = constructFileName(baseFileName = "Ttest_Results", plateReadLabel = aso@plateSet@plateNames[2],
                               fileExtension = "xlsx")

  # lets add the unpivoted table
  openxlsx::writeData(wb, sheet=2, x=ttestResult, startCol = 1, startRow = 1)

  openxlsx::saveWorkbook(wb, fileName, overwrite = T)
}


#' A utility method to export transformed data. The transformed data represents
#' change between the ASO treated state and teh control/untreated state.
#' @param aso an aso object from which to export transformed data.
#' @export
exportTransformedData <- function(aso) {
  data <- data.frame(aso@plateSet@transformedPlateData[[1]])

  wellAnn <- aso@plateSet@plates[[1]]@plateAnnotMap

  data <- cbind(wellAnn, data)

  dataList <- list()
  dataList[['Transformed_Data']] <- data

  fileName <- constructFileName(baseFileName = "Transformed_Response_Data", plateReadLabel = aso@plateSet@plateNames[1],
                                fileExtension = "xlsx")

  openxlsx::write.xlsx(dataList, file=fileName)
}


#' Exports a colelction of random forest results into one file, along with Benchmark Dose.
#' @param aso The aso object containing the MachineLearning Results
#' @export
exportMachineLearningResults <- function(aso) {

  fileName <- aso:::constructFileName(baseFileName = "Predictions_RF_Combined", plateReadLabel = "",
                                fileExtension = "xlsx")

  predResults = list()
  bmd <- NULL
  iter = 1

  for(resName in names(aso@mlResults)) {

    res <- aso@mlResults[[resName]]@prediction_means

    print(colnames(res))
    resName <- paste0(resName, "_pred_means")
    predResults[[resName]] <- res

    if(iter == 1) {
      bmd <- getBenchmarkDose(res)
      rownames(bmd) <- rownames(res)
    } else {
      bmd <- cbind(bmd, getBenchmarkDose(res))
    }

    iter = iter + 1
  }

  colnames(bmd) <- names(aso@mlResults)
  bmdList <- list(bench_mark_dose=bmd)
  predResults <- append(bmdList, predResults)

  for(resName in names(aso@mlResults)) {
    res <- aso@mlResults[[resName]]@prediction_sds
    resName <- paste0(resName, "_pred_sds")
    predResults[[resName]] <- res
  }

  for(resName in names(aso@mlResults)) {
    res <- aso@mlResults[[resName]]@predictions
    resName <- paste0(resName, "_pred_full")
    predResults[[resName]] <- res
  }

  openxlsx::write.xlsx(predResults, file=fileName, rowNames=T)
}


getBenchmarkDose <- function(predictionMeans, predCutoff = 0.5) {
  colNames <- colnames(predictionMeans)

  bmdDf <- data.frame(matrix(nrow=nrow(predictionMeans), ncol=1))

  for(row in 1:nrow(predictionMeans)) {
    for(col in 1:ncol(predictionMeans)) {
      if(!is.na(predictionMeans[row,col]) && predictionMeans[row,col] >= predCutoff) {
        bmdDf[row, 1] <- colNames[col]
        break
      }
      # fall through
      bmdDf[row, 1] <- NA
    }
  }

  return(bmdDf)
}


#' Exports prediction heatmaps for each plate read. These are generated based on the aso classes mlResults slot.
#' @param aso The aso object containing Random Forest result.
#' @export
exportRfPredictionHeatmaps <- function(aso) {

  predResults = list()
  bmd <- NULL
  iter = 1

  rfRes = NULL
  timeLabels = c()
  levels = c()
  for(resName in names(aso@mlResults)) {
    res <- aso@mlResults[[resName]]@prediction_means
    if(iter == 1) {
      rfRes = res
    } else {
      rfRes = cbind(rfRes, res)
    }

    timeLabels = c(timeLabels, rep(resName, ncol(res)))
    levels = c(levels, resName)
    iter = iter + 1
  }

  timeLabels <- factor(timeLabels, levels=levels)

  rfhm <- ComplexHeatmap::Heatmap(matrix=as.matrix(rfRes), cluster_rows=F, cluster_columns = F, cluster_column_slices = F, rect_gp = grid::gpar(col = "white", lwd = 2),
                           column_title_side = "top", column_names_side="top", name = 'Prediction',
                          row_title = "ASO", row_names_side="left", column_split = timeLabels, column_gap=unit(5,"mm"))

  fileName <- aso:::constructFileName(paste0(aso@methodParameters$analysis_name, "_Rf_Predictions_Heatmap"),
                          fileExtension = "pdf")

  pdf(file=fileName, width=13, height=6)
  print(rfhm)

  dev.off()

}

