
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


#' filterLowDataParameters
#'
#' @param plate plate on which to filter parameters with low data representation
#' @param thresholdType one of 'well_count' or 'well_percentage'
#' @param missingDataThreshold
#'
#' @return a plate with updated plate data
#' @export
#'
#' @examples
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


replaceBlanksAndNAs <- function(plateSet, blankReplacements, naReplacements) {

  #plateSet <- aso@plateSet
  #blankReplacements <- blankParamsList
  #naReplacements <- naParamList

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

runReferenceQC <- function(plateSet, rootExportDirectory, plateFile) {
  refPlate <- plateSet@plates[[1]]

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

  # add the median absolute z-score as a column in zDf
  rowMedians <- apply(abs(zDf), MARGIN = 1, FUN=median)
  zDf$abs_CV_Median <- unlist(rowMedians)

  # add the frequency GT 1SD
  zDf$freq_GT_1SD <- z1SDsums
  zDf$fract_param_GT_1SD <- signif(zDf$freq_GT_1SD/(zNcol*1.0), digits=3)

  # make an absolute median plate zscore matrix
  zAbsMedianPlateDf <- data.frame(matrix(zDf$abs_CV_Median, nrow=16, byrow = T))
  colnames(zAbsMedianPlateDf) <- as.numeric(c(1:24))
  rownames(zAbsMedianPlateDf) <- LETTERS[1:16]

  rowSpacing = 2

  wb = openxlsx::createWorkbook()
  headerStyle = openxlsx::createStyle(textDecoration = "Bold", fontSize=14)
  naString = "Empty"
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
    print("row number just before cond format")
    print(rowNum)
    openxlsx::conditionalFormatting(wb, sheet=1, type="colorScale", style = c('#ffcf40','#ebf6f7', '#4491fc'), rows = rowNum:(nrow(zMat)+rowNum), cols = 2:(ncol(zMat)+1), rule=c(lowerColorLim, 0.0, upperColorLim))

    # '#4491fc'

    rowNum = rowNum + nrow(zMat) + rowSpacing
  }

  # add the z-score unpivoted matrix
  openxlsx::addWorksheet(wb, "Unpivoted_Zscore_Matrix")
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

  fileName = format(Sys.time(), "Reference_Plate_QC_%Y%m%d_%H%M.xlsx")
  fileName <- paste0(rootExportDirectory, "/", fileName)

  openxlsx::saveWorkbook(wb,file=fileName,overwrite = T)

}


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
  res$Nonempty_Wells <- res$Well_Count - res$Empty_Wells
  res$Good_Count <- res$Nonempty_Wells - res$Missing_Count
  res$Parameter <- rownames(res)

  res <- res[,c(8,4,5,6,1,2,3,7)]

  #colnames(res) <- c("parameter", "good_count", "missing_count", "na_count", "zero_count")

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

  #print(parameter)

  print(colnames(plate@plateData))
  print(parameter)
  print(textValues)
  print(numericValues)

  vals <- plate@plateData[,parameter]

  for(val in textValues) {
    numVal = numericValues[i]
    vals <- gsub(val, numVal, vals)

    plate@plateData[parameter] <- vals
    i = i + 1
  }

  return(plate)
}


harmonizeParametersAcrossPlates <- function(plateSet) {

  commonParams <- c()
  commonParamNames <- c()
  i = 1
  for(plate in plateSet@plates) {
    if(i == 1) {
      commonParams <- colnames(plate@plateData)
      print("initial common params")
      print(commonParams)
      commonParamNames <- plate@parameters
    } else {
      params <- colnames(plate@plateData)
      print("other params")
      print(params)
      paramNames <- plate@parameters
      commonParams <- intersect(commonParams, params)
      commonParamNames <- intersect(commonParamNames, paramNames)
    }
    i = i + 1
  }

  print("Harmonizing.... common params")

  print("Common params:")
  print(commonParams)

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
#'
#' @examples
dataTransform <- function(plateSet, platePair, method = 'log2ratio', bkgrdCorr = F, bkgrdSamples = NULL, bkgrdMode = 'median', firstDataCol = 2) {

  # bkgrndCorrected = F
  #
  # if(bkgrdCorr && !is.null(bkgrdSamples) && length(bkgrdSamples) > 0) {
  #
  #
  #   bkgrndCorrected = T
  # }

  if(length(plateSet@plates) %% 2 == 0 && !is.null(platePair) && length(platePair) == 2) {

      refPlate <- plateSet@plates[[platePair[1]]]
      exptPlate <- plateSet@plates[[platePair[2]]]

      print("hey in data transform")

      # plateData may include wellIds in first column, so for numeric transformation, we need to skip first column using firstDataCol
      refData <- refPlate@plateData[,firstDataCol:ncol(refPlate@plateData)]
      exptData <- exptPlate@plateData[,firstDataCol:ncol(exptPlate@plateData)]

      print("Have ref and expt data")

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


  vals[!is.finite(vals)] <- NA
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


filterParametersOnList <- function(plate, paramsToKeep) {
  df <- plate@plateData
  #print("filtering selected params...")

  cnames <- colnames(df)

  dnames <- setdiff(paramsToKeep, cnames)



  df <- df[,paramsToKeep]

  plate@plateData <- df
  return(plate)
}


applyParameterAbbreviations <- function(plate, abbrInfo) {
  df <- plate@plateData

  # limit DF to statistics in the information dataframe, col 1
  # df <- df[,abbrInfo[,1]]

  params <- colnames(df)

  # need to capture abbreviations in the order of the columns
  abbrs <- c()
  for(col in params) {
    abbr <- abbrInfo[abbrInfo$Statistic == col,2]
    abbrs <- c(abbrs,abbr)
  }

  # set abbrs as colnames
  colnames(df) <- abbrs

  # rebuild plate info
  plate@parameters <- params
  plate@statList <- params
  plate@paramAbbrs <- abbrs
  plate@plateData <- df

  return(plate)
}


aucParameterPatch <- function(aso) {

  # patch plate data
  for(platename in names(aso@plateSet@plates)) {
    i=1
    plate <- aso@plateSet@plates[[platename]]
    for(n in colnames(plate@plateData)) {
      #print(n)
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


getPlateDataByWellSet <- function(plate, wellSet) {
  rows <- match(wellSet, plate@wellIds)
  dataChunk <- plate@plateData[rows,]
  return(dataChunk)
}

exportPairedTTestResult <- function(aso, ttestResult, fileName) {
  # df <- data.frame(matrix(ncol=4, nrow=0))
  # for(resName in names(ttest_result)) {
  #   res <- ttest_result[[resName]]
  #   sample <- unlist(strsplit(resName,split="_"))[1]
  #   resList <- list(sample=sample, condition=resName, tval=res$statistic, pval=res$p.value)
  # }

  # fileName <- paste0(aso@methodParameters[['root_dir']], "/", "Ttest_Results.xlsx")
  # openxlsx::write.xlsx(list(Paired_tTest_Results=ttestResult), fileName)

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
    #print(paste0(prevStart," to ",(prevStart + concCount)))
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
  fileName <- paste0(aso@methodParameters[['root_dir']], "/", fileName)

  # lets add the unpivoted table
  openxlsx::writeData(wb, sheet=2, x=ttestResult, startCol = 1, startRow = 1)

  openxlsx::saveWorkbook(wb, fileName, overwrite = T)

}


exportTransformedData <- function(aso) {

  data <- data.frame(aso@plateSet@transformedPlateData[[1]])

  wellAnn <- aso@plateSet@plates[[1]]@plateAnnotMap

  data <- cbind(wellAnn, data)

  dataList <- list()
  dataList[['Transformed_Data']] <- data

  fileName <- paste0("Transformed_Plate_Data_",format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")

  openxlsx::write.xlsx(dataList, file=fileName)

}



