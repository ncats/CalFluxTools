# The analysis.R file contains the high level method that makes calls on other methods for
# processing data and providing QC analyses. The buildAndProcessCalFluxDataObjects() method sequentially calls
# methods that process CalFluxData data according to an input file that specifies all methods.


#' This method is a convenience method that displays a file chooser for selecting a CalFluxData Paramter Excel file for processing.
#' The method will run data procesing and output diagnostic plots and tables.
#' @returns Returns a CalFluxData object containing analysis parameters from the parameter file, input, refined and transformed plate data.
#' @export
processData <- function() {
  file <- file.choose()
  processCalFluxData(file)
}

#' Wrapper method to process a data set possibly containing multiple file paires
#' processes data, writes file outputs, returns a CalFluxData object with processed data.
#' @param CalFluxDataParameterXlsxFilePath file path to param file
#' @returns processed CalFluxData object
#' @export
processCalFluxData <- function(CalFluxDataParameterXlsxFilePath) {
  initializeLog(logLevel = "TRACE")

  logger::log_info(paste0("Reading input parameter excel file ===> ", CalFluxDataParameterXlsxFilePath))
  # parse parameter and analysis settings, builds initial CalFluxData object.
  CalFluxData <- readCalFluxDataParameterFile(CalFluxDataParameterXlsxFilePath)
  logger::log_success("Finished reading input parameter excel file")

  # set the root dir as the working directory
  origDir <- getwd()
  rootDir <- CalFluxData@methodParameters[["root_dir"]]

  logger::log_info(paste0("Working directory ==> ", rootDir))
  setwd(paste0(rootDir))

  logger::log_info("Loading plate data")
  # loads the list of plate files specified in the parameter file and create file pairs
  # creating a plate set
  CalFluxData <- loadPlates(CalFluxData)
  logger::log_success("Finished loading plate data")

  logger::log_info("Loading plate map well annotations")
  CalFluxData <- loadPlateMaps(CalFluxData)
  logger::log_success("Finished loading plate map well annotations")

  # the CalFluxData has all plate pairs specified
  # loop through all plate pairs in the plate set.
  numberOfPlates <- length(CalFluxData@plateSet@plates)
  logger::log_info(paste0("Number of plate pairs to process: ", numberOfPlates / 2))

  logger::log_info("Initialize export directory...")
  outDir <- initializedExportLocation(CalFluxData)
  logger::log_success(paste0("Initialized export directory... ", outDir))

  platePairIndex <- 1
  paired_analysis <- CalFluxData@methodParameters$analysis_params$Paired_Analysis == "TRUE"
  for (i in 1:numberOfPlates) {
    if (i %% 2 == 1 | !paired_analysis) {
      # set the ouput dir
      currentOutDir <- paste0(outDir, "/", CalFluxData@outputDirs[[i]])
      if (!dir.exists(currentOutDir)) {
        dir.create(currentOutDir)
      }

      setwd(currentOutDir)

      logger::log_info("")
      logger::log_info("###############")
      logger::log_info(paste0("Processing plate pair: ", platePairIndex))
      logger::log_info(paste0("Output directory: ", currentOutDir))
      newCalFluxData <- CalFluxData
      newCalFluxData@pcaResults <- list()
      newCalFluxData@mlResults <- list()
      # assign(x="newCalFluxData", value=CalFluxData)

      # check to see if we have a copy of our CalFluxData object
      # print("Don't have CalFluxData copy????")
      # if(tracemem(newCalFluxData) == tracemem(CalFluxData)) {
      #   print("Same CalFluxData")
      #   print(tracemem(newCalFluxData))
      #   print(tracemem(CalFluxData))
      # } else {
      #   print("Different CalFluxData")
      # }
      if (paired_analysis) {
        keepers <- c(i, i + 1)
      } else {
        keepers <- i
      }
      newPlateSet <- subsetPlateSet(newCalFluxData@plateSet, plateIndicesToKeep = keepers)

      newCalFluxData@plateSet <- newPlateSet

      # print("CalFluxData and newCalFluxData plate count")
      # print(length(CalFluxData@plateSet@plates))
      # print(length(newCalFluxData@plateSet@plates))
      #
      # print("plate names for this iteration:")
      # print(newCalFluxData@plateSet@plateNames)

      # print("number of transformed data frames and t names")
      # print(length(CalFluxData@plateSet@transformedPlateData))
      # print(length(CalFluxData@plateSet@transformationNames))
      #
      #       print("Running a processing iteration, iter = plate read count...")
      #       print(i)
      #       print(length(newCalFluxData@plateSet@plates))
      newCalFluxData <- runDataProcessing(newCalFluxData, paired_analysis)
      # Preserve the original transformation label from the processed pair so
      # later analyses can distinguish timepoints correctly in the full object.
      for (tfName in names(newCalFluxData@plateSet@transformedPlateData)) {
        CalFluxData@plateSet@transformedPlateData[[tfName]] <- newCalFluxData@plateSet@transformedPlateData[[tfName]]
      }

      # collect PCA Result
      pcaResultList <- newCalFluxData@pcaResults
      for (id in names(pcaResultList)) {
        CalFluxData@pcaResults[[id]] <- pcaResultList[[id]]
      }

      # collect Random Forest Result
      rfResultList <- newCalFluxData@mlResults
      for (id in names(rfResultList)) {
        CalFluxData@mlResults[[id]] <- rfResultList[[id]]
      }

      platePairIndex <- platePairIndex + 1
    }
  }

  setwd("..")

  exportMachineLearningResults(CalFluxData)
  exportRfPredictionHeatmaps(CalFluxData)

  setwd(origDir)

  # stop pointing to this analysis log
  idleLog()

  return(CalFluxData)
}

#' This method is a wrapper method that performs all data processing operations according to the supplied CalFluxData Parameter File.
#' The method will output data diagnostic plots and tables as specified in the parameter file. The transformed data will be read for classification analysis.
#' @param CalFluxData a CalFluxData object with loaded plate data and analysis settings.
#' @param paired_analysis boolean specifying if plate pairs are used for baseline correction
#' @returns Returns a CalFluxData objecdt with loaded and transformed data. During processing several output summary files will be exported.
#' @export
runDataProcessing <- function(CalFluxData, paired_analysis) {
  logger::log_info("Starting Data Processing and Analysis methods")

  # AUC parameter names in the plate data file tend to have Japanese Kanji encoded as unicode.
  # This method replaces those parameter names with a standardized text, just a patch
  CalFluxData <- aucParameterPatch(CalFluxData)

  logger::log_info("Masking empty and masked wells.")
  # this method uses well annotations from the plate map to convert data from 'empty' wells
  # or wells that shouldn't be use, from numeric values to NaN (not-a-number values)
  CalFluxData <- maskEmptyWells(CalFluxData)
  logger::log_success("Masked empty and masked wells.")

  logger::log_info("Filtering to selected parameters")
  # users may select to only use some of the input parameters
  # this method removes data for parameters that should not be used (based on the parameter file)
  # only the key parameter's data will move on.
  logger::log_info("Selected parameters:")
  ## CalFluxData <- filterToSelectedParameters(CalFluxData)
  logger::log_success("Filtered to selected parameters.")
  # Users can specify a short abbreviation to replace the full parameter names.
  # The parameter file can hold these abbreviations and set these abbreviations to be used in output instead of the longer parameter names.
  CalFluxData <- setParameterAbbreviations(CalFluxData)

  # apply value imputations and make categorical results numeric - if specified
  # some parameters don't have numeric values, but rather text values that describe the parameter.
  # this step will optionally apply a mapping or conversion from text values to discrete numeric values.
  CalFluxData <- applyCategoricalToNumeric(CalFluxData)
  
  print("replace blanks and NAs")


  logger::log_info("Imputing/replacing blanks and NAs")
  # apply optional N/A and blank value replacements
  # some parameters can have values of 'N/A' or 'Blank'.
  # The lab has asked for a way to replace these values. The drop-in value or techinque for replacement is specified in the paramter input file.
  # replacement of these values is optional for each parameter.
  CalFluxData <- replaceBlanksAndNAValues(CalFluxData)
  logger::log_success("Finished imputing/replacing blanks and NAs")


  print("completeness")

  logger::log_info("QC for data completeness.")
  # assess data completeness for each parameter. This holds information on how many plate values
  # remain empty, zero, have 'blank' values meaning no data, or have been marked as 'masked'
  # after configured blank and N/A replacements have been applied.
  CalFluxData <- assessDataCompleteness(CalFluxData)
  logger::log_success("Finished QC for data completeness.")


  print("completeness filter")


  logger::log_info("Applying data coverage filters")
  # the parameter file will specify how complete a parameter's data has to be in order to keep that parameter in the dataset.
  # if a given parameter has very sparse data, it may be best to exclude that parameter for downstream analysis.
  # A user threshold determines how much missing data can be tolerated.
  CalFluxData <- applyDataCoverageFilters(CalFluxData)
  logger::log_success("Finished applying data coverage filters")

  print("export completeness filter")

  # export of the data coverage tables
  # this method will create an excel file that provides information on the data coverage assessment.
  exportDataCoverageReport(CalFluxData)


  print("harmonize")

  # harmonize parameters - make sure all plates in pairs have the same parameters (data cols)
  # the experiments compare a CalFluxData treatment plate read (plate data set) to an untreated (time=0) control plate read.
  # This method makes sure that after possibly filtering  out parameters, plate data sets (T=0 and experimental plate read)
  # both have the same parameters reported, in commmon.
  CalFluxData <- harmonizeParameters(CalFluxData)

  print("BL QA")

  logger::log_info("Running reference baseline QA")
  # This method runs Quality Control checks on the reference plate data (T=0 plate)
  # This will report on things like parameter variability, and wells that tend to be outliers, perahps having bad data (a bad well)
  if (paired_analysis) {
    CalFluxData <- runQcForReferencePlate(CalFluxData)
    logger::log_success("Finished reference baseline QA")
  }

  print("transform")


  # run transformations on plate set plate-pairs
  # this compares the experimental condition data to the reference or control data.
  # This produces a new data frame/table that will report on the effect of treatment.
  # Currently we report the log2(experimental_condition_value/control_value), a log base 2 fold change.

  CalFluxData <- transformData(CalFluxData, paired_analysis)
  print("export plate views")

  # create optional plate view pdf.
  # this will show plate views for each paramter, in plate-format.
  # These can reveal wells that tend to be outliers.
  if (paired_analysis) {
    exportPlateViews(CalFluxData)
  }

  print("export transformed data")
  exportTransformedData(CalFluxData)
  print("export bar charts")
  # create optional bar-chart
  # This exports the log2FoldChange (treatment effect) as bar charts.
  # Barcharts show treatment effect on the y-axis, and CalFluxData concentration on the x-axis.
  exportBarCharts(CalFluxData)

  print("ttest")

  if (paired_analysis) {
    # this runs a paired t-test to report on which wells show different values after treatment, for each parameter.
    # The method stores the result in a CalFluxData object.
    ttDf <- runPairedTTest(CalFluxData)

    # exports the paired t-test results.
    exportPairedTTestResult(CalFluxData, ttDf, "file")
  } else {
    runTTest(CalFluxData)
  }

  print("z prime")

  logger::log_info("Starting z-prime factor analysis of controls")

  zFactorXlsx(CalFluxData)

  logger::log_success("Finished z-prime factor analysis of controls")
  fullPCA <- runPCA(CalFluxData, dataType = "transformed")
  controlPCA <- runPCA(CalFluxData, dataType = "controls")

  # capture PCA results
  plateReadName <- CalFluxData@plateSet@plateNames[2]
  pcaResult <- new("pcaResult")
  pcaResult@allWellsPCA <- fullPCA
  pcaResult@controlWellsPCA <- controlPCA
  CalFluxData@pcaResults[[plateReadName]] <- pcaResult

  plotPCA(CalFluxData = CalFluxData, pcaList = fullPCA, pcaType = 1, dataName = "all_wells")
  plotPCA(CalFluxData = CalFluxData, pcaList = controlPCA, pcaType = 1, dataName = "control_wells")
  plotPCA(CalFluxData = CalFluxData, pcaList = fullPCA, pcaType = 2, dataName = "parameters_all_wells")
  plotPCA(CalFluxData = CalFluxData, pcaList = controlPCA, pcaType = 2, dataName = "parameters_control_wells")

  print("start random forest")
  # run random forest
  CalFluxData <- calFluxDataMachineLearning(CalFluxData)

  print("Analysis Done")

  # returns the CalFluxData data object
  return(CalFluxData)
}


#' This method loads plate data according the the plate files specified within the CalFluxData parameter file.
#' One plate object is created for each input plate file and the plate pairs are stored within the returned CalFluxData object.
#' @param CalFluxData the input CalFluxData file generated by readCalFluxDataParameterFile
#' @returns returns a CalFluxData object containing a plateset object with loaded plate data.
loadPlates <- function(CalFluxData) {
  fileList <- CalFluxData@methodParameters[["plate_file_list"]]
  plateSet <- new("plateset")
  plates <- list()
  dir <- CalFluxData@methodParameters[["root_dir"]]

  for (platename in names(fileList)) {
    file <- fileList[[platename]]
    plate <- read_calflux_data(filename = paste0(dir, file))
    plateSet <- addPlate(plateSet, plate, platename)
  }
  ## add the plateset
  CalFluxData@plateSet <- plateSet
  return(CalFluxData)
}

#' This function loads plate annotations, plate maps as specified in the CalFluxData object after initializing with parameter file
#' @param CalFluxData the input CalFluxData file generated by readCalFluxDataParameterFile
#' @returns returns a CalFluxData object containing a plateset object with loaded plate data and plate map annotations.
loadPlateMaps <- function(CalFluxData) {
  if (is.null(CalFluxData)) {
    print("Null CalFluxData object. Please use readCalFluxDataParameterFile() to initialize the CalFluxData file prior to loadPlateMaps()")
    return()
  }

  fileList <- CalFluxData@methodParameters[["plate_map_list"]]
  dir <- CalFluxData@methodParameters[["root_dir"]]

  if (is.null(fileList) || is.null(dir)) {
    print("Null root directory or plate file list in paramters.
    Please check the parameters file for the root directory and plate list information.
    Then reload the updated parameter file using readCalFluxDataParameterFile(CalFluxData) function.")
    return(CalFluxData)
  }

  for (platename in names(fileList)) {
    file <- fileList[[platename]]
    plate <- CalFluxData@plateSet@plates[[platename]]
    plate <- read_calflux_metadata(paste0(dir, file), plate)
    CalFluxData@plateSet@plates[[platename]] <- plate
  }

  return(CalFluxData)
}


#' Method updates parameter abbreviations to those specified in the parameter file
#' @param CalFluxData CalFluxData object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the CalFluxData object with parameter abbreviations set to use for all downstream reports.
setParameterAbbreviations <- function(CalFluxData) {
  keyCols <- c("Statistic", "Suggested_Abbreviation")

  if (!all(keyCols %in% colnames(CalFluxData@parameterInfo))) {
    print("Parameter information doesn't have key column names 'Statistic' and 'Suggested_Abbreviation'.
          Please update the parameter file to include these columns.
          Note that column names need to match these names exactly, including case.")
  }

  abbrInfo <- CalFluxData@parameterInfo[, keyCols]

  for (platename in names(CalFluxData@plateSet@plates)) {
    plate <- CalFluxData@plateSet@plates[[platename]]
    plate <- applyParameterAbbreviations(plate, abbrInfo)
    CalFluxData@plateSet@plates[[platename]] <- plate

  }
  return(CalFluxData)
}


####################
#####
##### high level methods working on the CalFluxData object
#####
####################

#' Converts well vales to NaN for wells that are annotated in the platemap as "Empty".
#' Downstream methods will not be impacted by these well values.
#' @param CalFluxData CalFluxData object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the CalFluxData object with specified wells masked.
maskEmptyWells <- function(CalFluxData) {
  for (plateName in names(CalFluxData@plateSet@plates)) {
    plate <- CalFluxData@plateSet@plates[[plateName]]
    plateMap <- plate@plateAnnotMap
    df <- plate@plateData

    # set data to empty, and treated as NAN
    df[plateMap$Compound == "Empty", ] <- "Empty"

    # set masked data
    df[plateMap$Mask == 1, ] <- "Masked"

    plate@plateData <- df
    CalFluxData@plateSet@plates[[plateName]] <- plate
  }

  return(CalFluxData)
}

#' Reduces the plate data object to only contain data for the subset of selected parameters.
#' @param CalFluxData CalFluxData object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the CalFluxData object with working set of plate data reduced to parameters specified in the parameter file.
filterToSelectedParameters <- function(CalFluxData) {
  pInfo <- CalFluxData@parameterInfo
  paramsToKeep <- unlist(pInfo$Statistic[pInfo$Use == 1])

  for (platename in names(CalFluxData@plateSet@plates)) {
    plate <- CalFluxData@plateSet@plates[[platename]]
    plate <- filterParametersOnList(plate, paramsToKeep)
    CalFluxData@plateSet@plates[[platename]] <- plate
  }
  return(CalFluxData)
}


#' Converts categorical data to numeric values according to specified rules in the input parameter file.
#' Note that this method will only run on parameters that are flagged as having categorical data.
#' @param CalFluxData CalFluxData object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the CalFluxData object categorical parameters converted to numeric values as specified in the parameter file.
applyCategoricalToNumeric <- function(CalFluxData) {
  # get parameters to convert
  paramsToMap <- CalFluxData@parameterInfo[!is.na(CalFluxData@parameterInfo$cat_to_num_map), ]

  for (platename in names(CalFluxData@plateSet@plates)) {
    plate <- CalFluxData@plateSet@plates[[platename]]

    for (i in 1:nrow(paramsToMap)) {
      abbrParam <- paramsToMap[i, 2]
      keys <- paramsToMap[i, "cat_to_num_map"]
      keys <- unlist(strsplit(keys, split = ","))
      for (j in 0:length(keys)) {
        keys[j] <- trimws(keys[j])
      }

      vals <- paramsToMap[i, "cat_to_num_map2"]

      vals <- unlist(strsplit(vals, split = ","))
      for (k in 0:length(vals)) {
        vals[j] <- trimws(vals[j])
      }
      vals <- as.numeric(vals)

      plate <- encodeDiscreteParameters(plate, abbrParam, keys, vals)
      CalFluxData@plateSet@plates[[platename]] <- plate
    }
  }
  return(CalFluxData)
}

#' Replaces 'Blank' or Empty data and N/A data for each parameter according to specified rules for each parameter.
#' @param CalFluxData CalFluxData object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the CalFluxData object categorical parameters converted to numeric values as specified in the parameter file.
replaceBlanksAndNAValues <- function(CalFluxData) {
  paramInfo <- CalFluxData@parameterInfo[CalFluxData@parameterInfo$Use == 1, ]
  naParams <- paramInfo[paramInfo$NA_Replace != "N", ]
  naParamList <- naParams$NA_Replace
  names(naParamList) <- naParams$Suggested_Abbreviation

  blankParams <- paramInfo[paramInfo$Blank_Replace != "N", ]
  blankParamList <- blankParams$Blank_Replace
  names(blankParamList) <- blankParams$Suggested_Abbreviation

  refinedPlateSet <- replaceBlanksAndNAs(CalFluxData@plateSet, blankParamList, naParamList)
  CalFluxData@plateSet <- refinedPlateSet

  return(CalFluxData)
}

#' Runs an analysis on the CalFluxData object to report on data coverage for each parameter.
#' The missing data report is entered into the CalFluxData object.
#' @param CalFluxData CalFluxData object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the CalFluxData object containing the missing data report in the dataCoverageTables slot
assessDataCompleteness <- function(CalFluxData) {
  dataCoverage <- list()

  plates <- CalFluxData@plateSet@plates
  pInfo <- CalFluxData@parameterInfo[CalFluxData@parameterInfo$Use == 1, c(1, 2)]

  for (platename in names(plates)) {
    plate <- plates[[platename]]
    missingValReport <- missingDataReport(plate = plate, plateSize = plate@format)

    # lets append the parameter names
    missingValReport <- merge(pInfo, missingValReport, by.x = "Suggested_Abbreviation", by.y = "Parameter", all.x = F, all.y = T, sort = F)

    missingValReport <- missingValReport[, c(2, 1, 3:(ncol(missingValReport)))]

    colnames(missingValReport)[1] <- "Param_Name"
    colnames(missingValReport)[2] <- "Parameter"

    ## Adding a step for filtering by coefficient of variation
    paramCV <- calculateParamCV(plate = plate, plateSize = plate@format)
    missingValReport <- missingValReport %>% dplyr::left_join(paramCV, by = "Parameter")
  
    dataCoverage[[platename]] <- missingValReport
  }

  CalFluxData@plateSet@dataCoverageTables <- dataCoverage

  return(CalFluxData)
}


#' This method optionally removes parameters that have a large amount of missing data, beyond specified levels.
#' @param CalFluxData CalFluxData object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the CalFluxData object with plate data reduced to parameters passing a minium data limit as specified in the parameter file.
applyDataCoverageFilters <- function(CalFluxData) {
  # for each plate loop over paramters to check their coverage and apply limits
  # get lower coverage limit for each parameter
  pInfo <- CalFluxData@parameterInfo[CalFluxData@parameterInfo$Use == 1, c("Statistic", "Suggested_Abbreviation", "Min_Data_Coverage_PCT", "Max_CV")]

  # pSize (plate size) has to be reduced to the number of non-empty wells.
  pSize <- CalFluxData@plateSet@dataCoverageTables[[1]]$Keeper_Wells[1]
  pInfo$max_data_loss <- pSize - ceiling((pInfo$Min_Data_Coverage_PCT / 100.0) * pSize)

  pInfo <- pInfo[,c("Suggested_Abbreviation","Max_CV","max_data_loss")]

  plateSet <- CalFluxData@plateSet
  dataCoverage <- plateSet@dataCoverageTables

  for (n in names(dataCoverage)) {
    dcov <- dataCoverage[[n]]
    dcov <- merge(dcov, pInfo, by.x = "Parameter", by.y = "Suggested_Abbreviation", sort = F)
    dcov$keep <- T
    dcov$keep[dcov$Missing_Count > dcov$max_data_loss] <- F
    dcov$keep[dcov$CV > dcov$Max_CV] <- F

    if ("Statistic" %in% colnames(dcov)) {
      dcov <- subset(dcov, select = c(-Statistic))
    }
    dataCoverage[[n]] <- dcov
  }

  CalFluxData@plateSet@dataCoverageTables <- dataCoverage

  plates <- CalFluxData@plateSet@plates
  dataCoverage <- CalFluxData@plateSet@dataCoverageTables
  for (platename in names(plates)) {
    plate <- plates[[platename]]
    dcov <- dataCoverage[[platename]]
    colsToKeep <- unlist(dcov[dcov$keep, 1])
    plate@plateData <- plate@plateData[, colsToKeep, drop = FALSE]
    plate@paramAbbrs <- colnames(plate@plateData)
    plate@parameters <- dcov$Param_Name[dcov$keep == TRUE]
    plates[[platename]] <- plate
  }

  CalFluxData@plateSet@plates <- plates

  return(CalFluxData)
}


#' Exports the data coverage report that lists parameters and information on missing data.
#' @param CalFluxData the CalFluxData object containing pre-computed data coverage results
exportDataCoverageReport <- function(CalFluxData) {
  rootDir <- CalFluxData@methodParameters[["root_dir"]]
  fileName <- constructFileName(
    baseFileName = "Plate_Read_Data_Coverage",
    plateReadLabel = CalFluxData@plateSet@plateNames[1],
    fileExtension = "xlsx"
  )

  tabs <- CalFluxData@plateSet@dataCoverageTables
  # openxlsx::write.xlsx(tabs, file = paste0(rootDir,fileName,".xlsx"))
  openxlsx::write.xlsx(tabs, file = fileName)
}

#' Makes sure that the reference plate read and the treatment plate read have the same parameters
#' after possibly filtering based on data loss.
#' @param CalFluxData the CalFluxData object having plate read pairs for comparison
harmonizeParameters <- function(CalFluxData) {
  CalFluxData@plateSet <- harmonizeParametersAcrossPlates(CalFluxData@plateSet)
  return(CalFluxData)
}

#' Runs quality control on the reference (control) plate read, prior to treatment.
#' This collects information on parameter variation (standard deviation, and coefficient of variation)
#' and z-scores for each parameter and each well help to identy outlier/bad wells that we might want to 'mask'
#' @param CalFluxData the CalFluxData object having a platepair object with a loaded reference plate
runQcForReferencePlate <- function(CalFluxData) {
  runReferenceQC(CalFluxData@plateSet, CalFluxData@methodParameters$root_dir, CalFluxData@methodParameters$plate_file_list[[1]])
  return(CalFluxData)
}

#' Transforms the plate data using the configured transformation method
#' This produces a single table that reports on the comparison between the reference or control state
#' versus the CalFluxData treated state.
#' @param CalFluxData a CalFluxData object having plate data (a platepair object) to transform
#' @param paired_analysis boolean specifying if plate pairs are used for baseline correction
#' @return a CalFluxData object with platepair object now containing the transformed data
transformData <- function(CalFluxData, paired_analysis) {
  transformationMethod <- CalFluxData@methodParameters$analysis_params$Transformation
  if (is.null(transformationMethod) || is.na(transformationMethod) || trimws(transformationMethod) == "") {
    transformationMethod <- "log2ratio"
  }
  transformationMethod <- tolower(trimws(transformationMethod))

  i <- 1
  plateNames <- c()
  for (plateName in names(CalFluxData@plateSet@plates)) {
    plate <- CalFluxData@plateSet@plates[[plateName]]
    plateNames <- c(plateNames, plateName)
    if (paired_analysis) {
      if (i %% 2 == 0) {
        CalFluxData@plateSet <- dataTransform(CalFluxData@plateSet, platePair = plateNames, method = transformationMethod, firstDataCol = 1)
        plateNames <- c()
      }
    } else {
      tfLabel <- paste0("untransformed", i)
      tfd <- plate@plateData
      CalFluxData@plateSet@transformedPlateData[[tfLabel]] <- tfd
    }
    i <- i + 1
  }
  return(CalFluxData)
}

#' Exports views of the plate data, for each parameter, as a pdf file.
#' The exported file will include the date and time of file creation.
#' @param CalFluxData a CalFluxData object containing plate data and having transformed data
exportPlateViews <- function(CalFluxData) {
  plateSet <- CalFluxData@plateSet
  i <- 1
  plateNames <- c()
  ptoPlot <- c()

  for (plateName in names(CalFluxData@plateSet@plates)) {
    plate <- CalFluxData@plateSet@plates[[plateName]]
    plateNames <- c(plateNames, plateName)

    # reports on each pair of plate reads
    if (i %% 2 == 0) {
      ptoPlot <- plateSet@plates[[plateNames[1]]]@parameters
      ptoPlot <- intersect(ptoPlot, plateSet@plates[[plateNames[2]]]@parameters)
      for (tName in plateSet@transformationNames) {
        if (nrow(plateSet@transformedPlateData[[tName]]) > 0) {
          ptoPlot <- intersect(ptoPlot, colnames(plateSet@transformedPlateData[[tName]]))
        }
      }
      if (length(ptoPlot) == 0) {
        plateNames <- c()
        i <- i + 1
        next
      }
      plotgrid <- trellisPlateSetViewsPairViews(plateSet,
        platePair = plateNames, paramsToPlot = ptoPlot, showRowColumnLabels = F,
        maxSatCount = 10, showLegend = F, tMethod = "log2ratio",
        ggplot = T, elementDividers = F
      )
      plateNames <- c()
    }
    i <- i + 1
  }
  rootDir <- CalFluxData@methodParameters[["root_dir"]]
  fileName <- constructFileName(
    baseFileName = "Plate_Coverage_Heatmaps",
    plateReadLabel = CalFluxData@plateSet@plateNames[1],
    fileExtension = "pdf"
  )

  if (!exists("plotgrid")) {
    return(invisible(NULL))
  }

  pdf(fileName, height = length(ptoPlot) * 2, width = 8.5)
  gridExtra::grid.arrange(plotgrid)
  dev.off()
}

#' This function exports a series of barcharts for each CalFluxData and each parameter into a pdf file.
#' @param CalFluxData a CalFluxData object having transformed data.
exportBarCharts <- function(CalFluxData) {
  samples <- unique(CalFluxData@plateSet@plates[[1]]@plateAnnotMap$Compound)
  samples <- samples[samples != ""]

  ### need to refer to input for control compound name/tag
  samples <- samples[!(samples %in% c("Veh1", "Veh2"))]

  # need to adjust to work over multiple plate pairs in plate set...
  plateset <- CalFluxData@plateSet
  params <- colnames(CalFluxData@plateSet@transformedPlateData[[length(CalFluxData@plateSet@transformedPlateData)]])
  plotgrid <- getBarChartTrellis(plateSet = plateset, samples = samples, parameters = params, samplesIn = "rows", tMethod = "log2Ratio")

  gridDim <- dim(plotgrid)
  rootDir <- CalFluxData@methodParameters[["root_dir"]]

  fileName <- constructFileName(baseFileName = "Response_Bar_Charts", plateReadLabel = CalFluxData@plateSet@plateNames[1], fileExtension = "pdf")
  pdf(fileName, height = gridDim[1] * 2, width = gridDim[2] * 2)
  gridExtra::grid.arrange(plotgrid)
  dev.off()
}

#' Performes a paired t-test to compare CalFluxData treated samples from the reference/control state.
#' @param CalFluxData CalFluxData object having loaded plate data.
#' @return a dataframe containing stat results for each parameter, for each well.
runPairedTTest <- function(CalFluxData) {
  plateset <- CalFluxData@plateSet
  plateNames <- names(plateset@plates)
  # return if the CalFluxData does not have an even number of plates (plate reads)
  if (length(plateNames) %% 2 != 0) {
    return(CalFluxData)
  }
  results <- list()
  lfcVals <- list()

  i <- 1

  for (name in plateNames) {
    ## get unique compounds, plate data and metadata from the reference plate
    if (i %% 2 == 1) {
      refPlate <- plateset@plates[[name]]
      plateMap <- plateset@plates[[1]]@plateAnnotMap

      compounds <- unique(plateMap$Compound)
      compounds <- compounds[!is.na(compounds)]
      compounds <- compounds[compounds != ""]
      compounds <- compounds[compounds != "Empty"]
      compounds <- compounds[compounds != "Masked"]
    } else {
      exptPlate <- plateset@plates[[name]]
      ## Nested for() loops to compute t-tests for each parameter for each compound at each concentration
      for (compound in compounds) {
        currCov <- plateMap[plateMap$Compound == compound, ]
        concs <- unique(currCov$Concentration)
        nonZero <- sum(concs != 0)
        if (nonZero == 0) {
          next
        }

        concs <- concs[concs != 0]
        for (conc in concs) {
          wells <- currCov[currCov$Concentration == conc, ]$Well
          refData <- getPlateDataByWellSet(plate = refPlate, wells)
          exptData <- getPlateDataByWellSet(plate = exptPlate, wells)

          if (!is.data.frame(refData) || !is.data.frame(exptData) ||
              ncol(refData) == 0 || ncol(exptData) == 0) {
            next
          }

          if (ncol(refData) != ncol(exptData)) {
            next
          }

          for (param in colnames(refData)) {
            d1 <- refData[, param]
            d2 <- exptData[, param]

            data <- data.frame(d1)
            data$d2 <- d2

            # need to convert to numeric
            data <- data.frame(lapply(data, FUN = coercePlateValuesToNumeric))
            data <- na.omit(data)
            meanExpt <- mean(data[, 2], na.rm = T) + 0.001
            meanRef <- mean(data[, 1], na.rm = T) + 0.001
            if (is.na(meanExpt)) {
              meanExpt <- 0.001
            }
            if (is.na(meanRef)) {
              meanRef <- 0.001
            }

            lfc <- log(mean(data[, 2] + 0.001, na.rm = T) / mean(data[, 1] + 0.001, na.rm = T), 2)

            if (nrow(data) > 2) {
              resultName <- paste0(compound, "_", conc, "_", param)

              # 0 variance is not tolerated well
              if (!(sd(data[, 2] - data[, 1]) < 1e-15)) {
                res <- t.test(x = data[, 2], y = data[, 1], paired = TRUE, alternative = "two.sided")
                results[[resultName]] <- res
              } else {
                ## equal variance case ...create result with NA values
                res <- list()
                res[["method"]] <- "paired.ttest.0.vaiance"
                res[["statistic"]] <- NaN
                res[["p.value"]] <- NaN
                results[[resultName]] <- data.frame(res)
              }
              lfcVals[[resultName]] <- lfc
            }
          }
        }
      }
    }
    i <- i + 1
  }
  df <- data.frame(matrix(ncol = 9, nrow = 0))
  for (resName in names(results)) {
    res <- results[[resName]]
    nameVals <- unlist(strsplit(resName, split = "_"))
    sample <- nameVals[1]
    dose <- nameVals[2]
    param <- paste(nameVals[3:length(nameVals)], collapse = "_")
    lfc <- lfcVals[[resName]]
    resList <- list(sample = sample, dose = dose, treatment = paste0(sample, "_", dose), param = param, condition = resName, method = res$method, tval = res$statistic, pval = res$p.value, log2FoldChange = lfc)
    df <- rbind(df, resList)
  }
  colnames(df) <- c("Sample", "Conc", "Treatment", "Parameter", "Condition", "TestMethod", "T", "pValue", "log2FoldChange")

  df <- df[order(df$pValue), ]
  df$adjP <- p.adjust(unlist(df$pValue), method = "BH")
  df$Conc <- as.numeric(df$Conc)
  df <- df[order(df$Sample, df$Parameter, df$Conc), ]
  # add -log(adjP)
  df$`-log10(adjP)` <- -1 * log(df$adjP, 10)

  return(df)
}

#' Generic T test function for multiple experimental designs
#' @param CalFluxData CalFluxData object having loaded plate data.
#' @return a dataframe containing stat results for each parameter, for each well.
runTTest <- function(CalFluxData) {
  plateset <- CalFluxData@plateSet
  plateNames <- names(plateset@plates)
  results <- list()
  lfcVals <- list()
  i <- 1
  posControlKey <- CalFluxData@methodParameters$analysis_params$Positive_Control_Key
  negControlKey <- CalFluxData@methodParameters$analysis_params$Negative_Control_Key
  sampleKey <- CalFluxData@methodParameters$analysis_params$Test_Sample_Key
  
  for (name in plateNames) {
    ## get unique compounds, plate data and metadata from the reference plate

    ## refPlate <- plateset@plates[[name]]
    plateMap <- plateset@plates[[name]]@plateAnnotMap

    compounds <- unique(plateMap$Compound)
    compounds <- compounds[!is.na(compounds)]
    compounds <- compounds[compounds != ""]
    compounds <- compounds[compounds != "Empty"]
    compounds <- compounds[compounds != "Masked"]

    exptPlate <- plateset@plates[[name]]
    ## Nested for() loops to compute t-tests for each parameter for each compound at each concentration
    for (compound in compounds) {
      currCov <- plateMap[plateMap$Compound == compound & plateMap$WellType == sampleKey, ]
      controls <- plateMap[plateMap$WellType == posControlKey,]
      controlWells <- controls$Well
      concs <- unique(currCov$Concentration)
      if(all(is.na(concs))){
        next
      }else{
        concs <- concs[which(!is.na(concs))]
      }
      nonZero <- sum(concs != 0)
      if (nonZero == 0) {
        next
      }

      concs <- concs[concs != 0]
      for (conc in concs) {
        wells <- currCov[currCov$Concentration == conc, ]$Well
        refData <- getPlateDataByWellSet(plate = exptPlate, controlWells)
        exptData <- getPlateDataByWellSet(plate = exptPlate, wells)

        ## if (ncol(refData) != ncol(exptData)) {
        ##   next
        ## }

        for (param in colnames(exptData)) {
          d1 <- refData[, param]
          d2 <- exptData[, param]

          ### data <- data.frame(d1)
          ### data$d2 <- d2
          data <- list(d1,d2)

          # need to convert to numeric
          data <- lapply(data, FUN = coercePlateValuesToNumeric)
          data <- lapply(data,na.omit)
          meanExpt <- mean(data[[2]], na.rm = T) + 0.001
          meanRef <- mean(data[[1]], na.rm = T) + 0.001
          if (is.na(meanExpt)) {
            meanExpt <- 0.001
          }
          if (is.na(meanRef)) {
            meanRef <- 0.001
          }

          lfc <- log(mean(data[[2]] + 0.001, na.rm = T) / mean(data[[1]] + 0.001, na.rm = T), 2)
          
          if (length(data[[1]]) > 2 & length(data[[2]]) > 2) {
            resultName <- paste0(compound, "_", conc, "_", param)

            # 0 variance is not tolerated well
            if (!(sd(data[[2]] - data[[1]]) == 0)) {
              res <- t.test(x = data[[2]], y = data[[1]], alternative = "two.sided")
              results[[resultName]] <- res
            } else {
              ## equal variance case ...create result with NA values
              res <- list()
              res[["method"]] <- "ttest.0.variance"
              res[["statistic"]] <- NaN
              res[["p.value"]] <- NaN
              results[[resultName]] <- data.frame(res)
            }
            lfcVals[[resultName]] <- lfc
          }
        }
      }
    }

    i <- i + 1
  }
  df <- data.frame(matrix(ncol = 9, nrow = 0))
  for (resName in names(results)) {
    res <- results[[resName]]
    nameVals <- unlist(strsplit(resName, split = "_"))
    sample <- nameVals[1]
    dose <- nameVals[2]
    param <- paste(nameVals[3:length(nameVals)], collapse = "_")
    lfc <- lfcVals[[resName]]
    resList <- list(sample = sample, dose = dose, treatment = paste0(sample, "_", dose), param = param, condition = resName, method = res$method, tval = res$statistic, pval = res$p.value, log2FoldChange = lfc)
    df <- rbind(df, resList)
  }
  colnames(df) <- c("Sample", "Conc", "Treatment", "Parameter", "Condition", "TestMethod", "T", "pValue", "log2FoldChange")

  df <- df[order(df$pValue), ]
  df$adjP <- p.adjust(unlist(df$pValue), method = "BH")
  df$Conc <- as.numeric(df$Conc)
  df <- df[order(df$Sample, df$Parameter, df$Conc), ]
  # add -log(adjP)
  df$`-log10(adjP)` <- -1 * log(df$adjP, 10)

  return(df)
}

#' Utility analysis method to generate z-prime factor data.
#' @param CalFluxData CalFluxData object on which to perform the z-prime factor analysis. z-prime = 1- 3*(abs(posCtrlMean-negCtrlMean))/(posCtrlSD + negCtrlSD)
#' @param dataType Either transformed or raw data.
#' @returns a z-prime factor result table with positive and negative control means, SDs, CVs and the z-prime factors for each parameter.
#' @export
zFactor <- function(CalFluxData, dataType = "transformed") {
  # The @ is used to dereference
  # Here we have the CalFluxData object, dereference the plateset object, and finally get the transformedPlateData
  # it's designed as a list or dictionary
  plateTransformedData <- CalFluxData@plateSet@transformedPlateData[[length(CalFluxData@plateSet@transformedPlateData)]]

  # an R list object is dereferenced using double brackets
  # you can refer to a member of the list by name or by index. *R indexing starts at 1
  if (dataType == "transformed") {
    testData <- plateTransformedData
  } else {
    testData <- CalFluxData@plateSet@plates[[1]]@plateData
  }
  # the platemap holds well annotations, it's a dataframe
  # this gets the plateset from the CalFluxData object, plate list, takes the first plate, and then
  # the plateAnnotationMap field.
  plateMap <- CalFluxData@plateSet@plates[[1]]@plateAnnotMap

  # an alternative to viewing a table is using the 'Global Envionment' in the upper right window.
  # if the variable is a data.frame, click on the table icon on the far right to view the data.

  # Combines plateMap and transformedData
  tableWithAnnotation <- cbind(plateMap, testData)

  # Created a data frame that does not contain empty wells
  splicedData <- tableWithAnnotation[tableWithAnnotation$Compound != "Empty", ]
  splicedData <- splicedData[splicedData$Mask != 1, ]

  posControlKey <- CalFluxData@methodParameters$analysis_params$Positive_Control_Key
  negControlKey <- CalFluxData@methodParameters$analysis_params$Negative_Control_Key

  # Create a data frame that only contains positive controls
  PosCtrl <- splicedData[splicedData$WellType == posControlKey, ]
  # unique(PosCtrl$WellType)

  # Create a data frame that only contains negative controls
  NegCtrl <- splicedData[splicedData$WellType == negControlKey, ]
  # unique(NegCtrl$WellType)

  # make means and sds data frame for both controls
  numericPosCtrl <- data.frame(sapply(PosCtrl[, (ncol(plateMap) + 1):ncol(PosCtrl)], coercePlateValuesToNumeric))
  allUPosCtrl <- colMeans(numericPosCtrl)
  allSDPosCtrl <- sapply(numericPosCtrl, sd)

  numericNegCtrl <- data.frame(sapply(NegCtrl[, (ncol(plateMap) + 1):ncol(NegCtrl)], coercePlateValuesToNumeric))
  allUNegCtrl <- colMeans(numericNegCtrl)
  allSDNegCtrl <- sapply(numericNegCtrl, sd)

  zFactor <- 1 - ((3 * (allSDPosCtrl + allSDNegCtrl)) / (abs(allUPosCtrl - allUNegCtrl)))

  coeffVarPosCtrl <- allSDPosCtrl / abs(allUPosCtrl)
  coeffVarNegCtrl <- allSDNegCtrl / abs(allUNegCtrl)

  # make a data frame with all means, sds, and zFactor and cbind them together
  zFactorTable <- cbind(
    data.frame(allUPosCtrl), data.frame(allSDPosCtrl), data.frame(coeffVarPosCtrl),
    data.frame(allUNegCtrl), data.frame(allSDNegCtrl), data.frame(coeffVarNegCtrl),
    data.frame(zFactor)
  )

  return(zFactorTable)
}


#' Runs a z-prime factor analysis on transformed and raw data results. z-prime = 1- 3*(abs(posCtrlMean-negCtrlMean))/(posCtrlSD + negCtrlSD)
#' Exports a file containing z-prime factor, mean, SD and CV of each parameter using both transformed and raw data.
#' @param CalFluxData a CalFluxData object containing transformed data on which to report z-prime on.
#' @export
zFactorXlsx <- function(CalFluxData) {
  fileName <- constructFileName(baseFileName = "zPrime_Control_Results", plateReadLabel = CalFluxData@plateSet@plateNames[1], fileExtension = "xlsx")

  # Make a list
  zPrimeList <- list()

  # First call to zFactor calculation to calculate for transformed data
  zPrimeList[["transformedDataResults"]] <- zFactor(CalFluxData)

  # Second call to zFactor to calculate for raw data
  zPrimeList[["rawDataResults"]] <- zFactor(CalFluxData, "experimental")

  fileName <- constructFileName(baseFileName = "zPrimeFactorResults", plateReadLabel = CalFluxData@plateSet@plateNames[1], fileExtension = "xlsx")

  # Write to an Excel file
  openxlsx::write.xlsx(zPrimeList,
    file = fileName,
    rowNames = TRUE
  )
}


#' Runs a PCA on CalFluxData data
#' @param CalFluxData CalFluxData object containing data
#' @param dataType c("transformed', 'raw', 'controls') to indicate if the data should be all transformed well data, all raw well data,
#' or well data only from control wells.
#' @param compoundIDList an optional vector of compound ids to use for PCA
#' @param scale should the PCA be scaled. Default is TRUE.
#' @returns a list object containing 4 fields. 'Wells PCA' and 'Parameters PCA' contain the prcomp objects for running PCA on wells or parameters.
#' The Annotations element is a data frame containing well annotations. The 'Parameter Names' element is a data frame containing parameter info.
#' @export
runPCA <- function(CalFluxData, dataType = "transformed", compoundIDList = NULL, scale = T) {
  plateTransformedData <- CalFluxData@plateSet@transformedPlateData
  plateMap <- CalFluxData@plateSet@plates[[1]]@plateAnnotMap
  pcaList <- list()

  failPCA <- function(reason, reducedPlateMap = NULL, parameterNames = NULL) {
    pcaList[["Wells PCA"]] <- NULL
    pcaList[["Parameters PCA"]] <- NULL
    pcaList[["Annotations"]] <- reducedPlateMap
    pcaList[["Parameter Names"]] <- parameterNames
    pcaList[["PCA Available"]] <- FALSE
    pcaList[["PCA Failure Reason"]] <- reason

    return(pcaList)
  }

  if (dataType == "transformed") {
    testData <- plateTransformedData[[length(plateTransformedData)]]
  } else if (dataType == "raw") {
    testData <- CalFluxData@plateSet@plates[[2]]@plateData
  } else if (dataType == "controls") {
    posControlKey <- CalFluxData@methodParameters$analysis_params$Positive_Control_Key
    negControlKey <- CalFluxData@methodParameters$analysis_params$Negative_Control_Key
    controlKeys <- c(posControlKey, negControlKey)

    testData <- plateTransformedData[[length(plateTransformedData)]]
    testData <- testData[plateMap$WellType %in% controlKeys, ]
    plateMap <- plateMap[plateMap$WellType %in% controlKeys, ]
  } else {
    return(NULL)
  }

  tableWithAnnotation <- cbind(plateMap, testData)

  tableWithAnnotation$Compound[tableWithAnnotation$WellType == "Positive Control"] <- "Positive Control"
  tableWithAnnotation$Compound[tableWithAnnotation$WellType == "Negative Control"] <- "Negative Control"

  splicedData <- tableWithAnnotation[tableWithAnnotation$Compound != "Empty", ]
  splicedData <- splicedData[splicedData$Mask != 1, ]
  rownames(splicedData) <- splicedData$Well

  reducedPlateMap <- splicedData[, 1:ncol(plateMap)]
  parameterNames <- colnames(testData)

  if (nrow(splicedData) == 0) {
    return(failPCA("No rows remain after filtering empty and masked wells.", reducedPlateMap, parameterNames))
  }
  
  # Match the random forest preprocessing: prefer dropping incomplete feature
  # columns over dropping wells with any missing exported parameter.
  testDataSplice <- splicedData[, (ncol(plateMap) + 1):ncol(splicedData), drop = FALSE]
  testDataSplice <- data.frame(lapply(testDataSplice, coercePlateValuesToNumeric))
  
  if (ncol(testDataSplice) == 0 || nrow(testDataSplice) == 0) {
    return(failPCA("No numeric parameter data remain after preprocessing.", reducedPlateMap, parameterNames))
  }

  completeFeatureMask <- colSums(is.na(testDataSplice)) == 0
  droppedFeatureCount <- sum(!completeFeatureMask)
  if (droppedFeatureCount > 0) {
    logger::log_info("Dropping {droppedFeatureCount} PCA parameter columns with missing values across selected wells.")
  }
  testDataSpliceCompleteFeatures <- testDataSplice[, completeFeatureMask, drop = FALSE]
  parameterNames <- colnames(testDataSpliceCompleteFeatures)

  if (ncol(testDataSpliceCompleteFeatures) == 0) {
    return(failPCA("No complete numeric parameter columns remain after applying PCA feature completeness filter.", reducedPlateMap, parameterNames))
  }

  # This is usually all TRUE after the feature filter above, but keeps PCA safe
  # if numeric coercion or future preprocessing leaves row-level missingness.
  completeWellMask <- complete.cases(testDataSpliceCompleteFeatures)
  testDataSpliceCompleteFeatures <- testDataSpliceCompleteFeatures[completeWellMask, , drop = FALSE]
  reducedPlateMap <- reducedPlateMap[completeWellMask, , drop = FALSE]

  if (nrow(testDataSpliceCompleteFeatures) == 0) {
    return(failPCA("No complete wells remain after numeric coercion and filtering.", reducedPlateMap, parameterNames))
  }

  varianceMask <- apply(testDataSpliceCompleteFeatures, 2, var, na.rm = TRUE)
  varianceMask[is.na(varianceMask)] <- 0
  testDataSpliceVarComplete <- testDataSpliceCompleteFeatures[, varianceMask != 0, drop = FALSE]
  parameterNames <- colnames(testDataSpliceVarComplete)

  if (ncol(testDataSpliceVarComplete) == 0) {
    return(failPCA("All retained parameters have zero variance across selected wells.", reducedPlateMap, parameterNames))
  }

  # For the parameter PCA, reuse the same complete feature matrix so PCA wells
  # match the wells plot while incomplete parameters are omitted up front.
  testDataSpliceT <- t(testDataSpliceVarComplete)
  if (ncol(testDataSpliceT) == 0 || nrow(testDataSpliceT) == 0) {
    return(failPCA("No parameter matrix remains after transposition and NA filtering.", reducedPlateMap, parameterNames))
  }

  # Transposition can still create zero-variance well columns; remove those.
  varianceMaskT <- apply(testDataSpliceT, 2, var, na.rm = TRUE)
  varianceMaskT[is.na(varianceMaskT)] <- 0
  testDataSpliceTvar <- testDataSpliceT[, varianceMaskT != 0, drop = FALSE]

  if (ncol(testDataSpliceTvar) == 0) {
    return(failPCA("All wells have zero variance across retained parameters after transposition.", reducedPlateMap, parameterNames))
  }

  pcaCalFluxData <- prcomp(as.matrix(testDataSpliceVarComplete), center = TRUE, scale. = scale, retx = TRUE)
  pcaCalFluxDataT <- prcomp(testDataSpliceTvar, center = TRUE, scale. = scale, retx = TRUE)

  pcaList[["Wells PCA"]] <- pcaCalFluxData
  pcaList[["Parameters PCA"]] <- pcaCalFluxDataT

  # add something in the list to add the annotation
  pcaList[["Annotations"]] <- reducedPlateMap
  pcaList[["Parameter Names"]] <- parameterNames
  pcaList[["PCA Available"]] <- TRUE
  pcaList[["PCA Failure Reason"]] <- NULL

  return(pcaList)
}


#' Plot PCA results
#'
#' @import ggfortify
#' @import ggplot2
#' @param CalFluxData The CalFluxData object containing pcaResults
#' @param pcaList A list object containing PCA results
#' @param pcaType 1 = wells pca, 2 = parameters pca
#' @param plotLoadings a boolean value determining if PCA loadings should be plotted.
#' @param dataName an optional string to tag on the data results.
#' @export
plotPCA <- function(CalFluxData, pcaList, pcaType = 1, plotLoadings = F, dataName = "") {
  plotTitle <- ""

  if (is.null(pcaList) || isFALSE(pcaList[["PCA Available"]])) {
    reason <- if (is.null(pcaList)) "PCA result is missing." else pcaList[["PCA Failure Reason"]]
    logger::log_warn("Skipping PCA plot for {dataName}: {reason}")
    return(NULL)
  }

  maxOverlap <- 10
  if (dataName == "control_wells") {
    maxOverlap <- Inf
  }
  labelSize <- 4
  pointSize <- 1.4
  repelBoxPadding <- 0.12
  repelPointPadding <- 0.08
  repelMaxTime <- 3
  repelMaxIter <- 20000
  if (pcaType == 2) {
    maxOverlap <- Inf
    labelSize <- 3.5
    pointSize <- 1.6
    repelBoxPadding <- 0.08
    repelPointPadding <- 0.05
    repelMaxTime <- 5
    repelMaxIter <- 40000
  }

  maxLegendEntries <- 40
  pcaColorLegendEntryCount <- 0
  pcaColorLegendLabel <- "Compound / Concentration"

  makePcaColorGroup <- function(annotationData, includeConcentration = TRUE) {
    compound <- trimws(as.character(annotationData$Compound))
    concentration <- trimws(as.character(annotationData$Concentration))

    compound[is.na(compound) | compound == ""] <- "Unannotated"
    concentration[is.na(concentration) | concentration == ""] <- NA_character_

    colorGroup <- if (!includeConcentration) {
      compound
    } else {
      ifelse(
        is.na(concentration),
        compound,
        paste0(compound, " (", concentration, ")")
      )
    }

    factor(colorGroup, levels = unique(colorGroup))
  }

  assignPcaColorGroup <- function(annotationData) {
    colorGroup <- makePcaColorGroup(annotationData)
    pcaColorLegendLabel <<- "Compound / Concentration"
    pcaColorLegendEntryCount <<- nlevels(colorGroup)

    if (pcaColorLegendEntryCount > maxLegendEntries) {
      logger::log_warn(
        "PCA plot for {dataName} has {pcaColorLegendEntryCount} compound/concentration color groups; using compound-only colors."
      )
      colorGroup <- makePcaColorGroup(annotationData, includeConcentration = FALSE)
      pcaColorLegendLabel <<- "Compound"
      pcaColorLegendEntryCount <<- nlevels(colorGroup)
    }

    colorGroup
  }

  if (pcaType == 1) {
    plotTitle <- "Wells_PCA"

    pca_prop_var <- (pcaList[[pcaType]]$sdev^2 / sum(pcaList[[pcaType]]$sdev^2))
    pcAxisLabels <- paste0(c("PC1", "PC2"), " (", round(pca_prop_var[1:2] * 100, 2), "%)")

        if (!plotLoadings) {
          pointData <- cbind(as.data.frame(pcaList[[pcaType]]$x[, 1:2, drop = FALSE]), pcaList$Annotations)
          pointData$PcaColorGroup <- assignPcaColorGroup(pointData)
          pcaPlot <- ggplot2::ggplot(pointData, aes(x = PC1, y = PC2, color = PcaColorGroup)) +
            ggplot2::geom_point(size = pointSize) +
            ggrepel::geom_text_repel(
              data = pointData,
              aes(x = PC1, y = PC2, label = Well),
              inherit.aes = FALSE,
              size = labelSize,
              max.overlaps = maxOverlap,
              box.padding = repelBoxPadding,
              point.padding = repelPointPadding,
              min.segment.length = 0,
              max.time = repelMaxTime,
              max.iter = repelMaxIter
            ) +
            ggplot2::labs(x = pcAxisLabels[1], y = pcAxisLabels[2], color = pcaColorLegendLabel)
        } else {
          pcaAnnotations <- pcaList$Annotations
          pcaAnnotations$PcaColorGroup <- assignPcaColorGroup(pcaAnnotations)
          pointData <- cbind(as.data.frame(pcaList[[pcaType]]$x[, 1:2, drop = FALSE]), pcaAnnotations)
            pcaPlot <- suppressWarnings(
              ggplot2::autoplot(pcaList[[pcaType]], data = pcaAnnotations, scale = 0, color = "PcaColorGroup", loadings = T, label = F)
            )
          pcaPlot <- pcaPlot +
            ggrepel::geom_text_repel(
              data = pointData,
              aes(x = PC1, y = PC2, label = Well),
              inherit.aes = FALSE,
              size = labelSize,
              max.overlaps = maxOverlap,
              box.padding = repelBoxPadding,
              point.padding = repelPointPadding,
              min.segment.length = 0,
              max.time = repelMaxTime,
              max.iter = repelMaxIter
            ) +
            ggplot2::labs(color = pcaColorLegendLabel)
        }
      } else {
      plotTitle <- "Parameters_PCA"

      ## labels <- data.frame(pcaList[["Parameter Names"]])
      ## colnames(labels) <- "param"
      labels <- data.frame(param = rownames(pcaList[[pcaType]]$x))
      pointData <- cbind(as.data.frame(pcaList[[pcaType]]$x[, 1:2, drop = FALSE]), labels)
      pca_prop_var <- (pcaList[[pcaType]]$sdev^2 / sum(pcaList[[pcaType]]$sdev^2))
      pcAxisLabels <- paste0(c("PC1", "PC2"), " (", round(pca_prop_var[1:2] * 100, 2), "%)")
      
      pcaPlot <- ggplot2::ggplot(pointData, aes(x = PC1, y = PC2)) +
          ggplot2::geom_point(
            size = pointSize
          ) +
        ggrepel::geom_text_repel(
          data = pointData,
          aes(x = PC1, y = PC2, label = param),
          inherit.aes = FALSE,
          size = labelSize,
          max.overlaps = maxOverlap,
          box.padding = repelBoxPadding,
          point.padding = repelPointPadding,
          min.segment.length = 0,
          max.time = repelMaxTime,
          max.iter = repelMaxIter
        ) +
        ggplot2::labs(x = pcAxisLabels[1], y = pcAxisLabels[2])
    }

  pcaPlot <- pcaPlot +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 22),
      axis.title = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 19),
      legend.text = ggplot2::element_text(size = 17)
    )

  fileName <- paste0(plotTitle, "_", dataName)
  fileName <- constructFileName(baseFileName = fileName, plateReadLabel = CalFluxData@plateSet@plateNames[1], fileExtension = "pdf")

  pdf(file = fileName, width = 11, height = 8)
  print(pcaPlot)
  dev.off()

  return(pcaPlot)
}


#' generatees random forest predictions for CalFluxData objects
#' @param CalFluxData a CalFluxData object containing transformed data that's ready for analysis
#' @param masking list of wells to optionally mask
#' @returns creates a prediction heatmap and dot plot, returns a Heatmap object.
#' @import dplyr
#' @export
calFluxDataMachineLearning <- function(CalFluxData, masking = NULL) {
  ml <- new("MachineLearning")

  plateNames <- CalFluxData@plateSet@plateNames
  plateName <- plateNames[length(plateNames)]

  negControlKey <- CalFluxData@methodParameters$analysis_params$Negative_Control_Key
  posControlKey <- CalFluxData@methodParameters$analysis_params$Positive_Control_Key
  sampleKey <- CalFluxData@methodParameters$analysis_params$Test_Sample_Key
  
  # Subsetting data
  data_imputed <- CalFluxData@plateSet@transformedPlateData[[length(CalFluxData@plateSet@transformedPlateData)]]
  sample_info <- CalFluxData@plateSet@plates[[1]]@plateAnnotMap

  data_imputed <- data_imputed[which(sample_info$Compound != "Empty" & sample_info$Mask != 1), , drop = FALSE]

  # filter empty wells and masked wells
  sample_info <- sample_info %>%
    dplyr::filter(Compound != "Empty") %>%
    dplyr::filter(Mask != 1)

  # Build a data table that contains required data (can be updated)
  # pca_res <- prcomp(data_imputed, scale.=TRUE)
  # pca_df <- pca_res$x[,1:2] %>%
  #   as.data.frame() %>%
  #   mutate(WellType = sample_info$WellType) %>%
  #   mutate(Compound = sample_info$Compound) %>%
  #   mutate(Well = sample_info$Well) %>%
  #   mutate(Concentration = as.character(sample_info$Concentration))
  #
  #
  # if(masking != ''){
  #   pca_df = pca_df[pca_df$Well != masking,]
  # }

  # Subset data to get all required training data
  # data_labels <- pca_df$WellType
  data_labels <- sample_info$WellType
  training_rows <- which(data_labels == posControlKey | data_labels == negControlKey)
  training_labels <- factor(data_labels[training_rows])
  training_data <- data_imputed[training_rows, ] %>% as.data.frame()
  training_data$label <- training_labels
  concentration_labels <- as.numeric(sample_info$Concentration[training_rows])

  # Drop concentration labels
  # new_training_data = cbind(concentration_labels, training_data)
  new_training_data <- training_data

  # Generate new labels that are numeric (between 0 and 1)
  cont_training_data <- new_training_data
  updated_labels <- data.frame()
  for (i in 1:nrow(cont_training_data)) {
    if (cont_training_data$label[i] == posControlKey) {
      updated_labels[i, 1] <- as.numeric(1)
    } else {
      updated_labels[i, 1] <- as.numeric(0)
    }
  }

  # Remove old label column and add new label column
  colnames(updated_labels) <- "label"
  cont_training_data <- subset(cont_training_data, select = -label)
  cont_training_data <- cbind(cont_training_data, updated_labels)

  cont_training_data <-
    cont_training_data %>%
    dplyr::mutate_if(is.character, coercePlateValuesToNumeric)

  featureCols <- setdiff(colnames(cont_training_data), "label")
  trainingFeatures <- cont_training_data[, featureCols, drop = FALSE]
  featureObservedMask <- colSums(!is.na(trainingFeatures)) > 0
  droppedFeatureNames <- featureCols[!featureObservedMask]
  if (length(droppedFeatureNames) > 0) {
    logger::log_warn(
      "Dropping {length(droppedFeatureNames)} RF feature columns with no observed control values: {paste(droppedFeatureNames, collapse = ', ')}"
    )
  }

  trainingFeatures <- trainingFeatures[, featureObservedMask, drop = FALSE]
  featureMeans <- vapply(trainingFeatures, function(featureValues) {
    mean(featureValues, na.rm = TRUE)
  }, numeric(1))

  missingTrainingCellCount <- sum(is.na(trainingFeatures))
  if (missingTrainingCellCount > 0) {
    logger::log_info(
      "Mean-imputing {missingTrainingCellCount} missing RF control/training values using control feature means."
    )
  }
  for (featureName in names(featureMeans)) {
    missingRows <- is.na(trainingFeatures[[featureName]])
    if (any(missingRows)) {
      trainingFeatures[[featureName]][missingRows] <- featureMeans[[featureName]]
    }
  }

  cont_training_data <- cbind(
    trainingFeatures,
    label = cont_training_data$label
  )
  cont_training_data <- cont_training_data[complete.cases(cont_training_data), , drop = FALSE]
  if(length(unique(cont_training_data$label))==1){
    stop("No positive and/or negative controls found. Check the parameter input file for misspellings")
  }
  if (nrow(cont_training_data) < 2) {
    stop("Not enough complete control wells remain after mean-imputing missing values for random forest training.")
  }
  if (ncol(cont_training_data) <= 1) {
    stop("No numeric parameters with observed control values remain for random forest training.")
  }

  # Use a stable seed so repeated runs on the same input produce the same model
  # and downstream heatmaps. Allow an override from the analysis parameter file.
  randomSeed <- 12345L
  seedParam <- CalFluxData@methodParameters$analysis_params$Random_Seed
  if (!is.null(seedParam)) {
    parsedSeed <- suppressWarnings(as.integer(seedParam))
    if (!is.na(parsedSeed)) {
      randomSeed <- parsedSeed
    }
  }

  # Train model
  for (i in c("rf")) {
    if(i == "rf"){
      mtry <- ceiling(ncol(cont_training_data)/3)
      ntree = 1000
      tunegrid <- expand.grid(.mtry = mtry)
    }

    set.seed(randomSeed)
    resampleSeeds <- vector(mode = "list", length = 11)
    for (seedIndex in 1:10) {
      resampleSeeds[[seedIndex]] <- randomSeed + seedIndex
    }
    resampleSeeds[[11]] <- randomSeed + 11

    new_model <- suppressWarnings(
      caret::train(label ~ .,
        data = cont_training_data,
        method = i,
        tuneGrid = tunegrid,
        ntree = ntree,
        trControl = caret::trainControl(method = "cv", number = 10, seeds = resampleSeeds)
      )
    )
    # capture model
    ml@model <- new_model

    plotVariableImportance(new_model,plateName=plateName)
    
    # Subset data for testing
    testing_rows <- which(data_labels == sampleKey & sample_info$Compound != "PBS")
    testing_data <- data_imputed[testing_rows, ] %>% as.data.frame()
    clean_conc_labels <- as.numeric(sample_info$Concentration[testing_rows])

    # don't include concentration as a factor
    # new_testing_data = cbind(clean_conc_labels, testing_data)
    # colnames(new_testing_data)[1] = "concentration_labels"
    new_testing_data <- testing_data

    new_testing_data <-
      new_testing_data %>%
      dplyr::mutate_if(is.character, coercePlateValuesToNumeric)
    trainingFeatureNames <- setdiff(colnames(cont_training_data), "label")
    new_testing_data <- new_testing_data[, trainingFeatureNames, drop = FALSE]
    missingTestingCellCount <- sum(is.na(new_testing_data))
    if (missingTestingCellCount > 0) {
      logger::log_info(
        "Mean-imputing {missingTestingCellCount} missing RF prediction values using control feature means."
      )
    }
    for (featureName in trainingFeatureNames) {
      missingRows <- is.na(new_testing_data[[featureName]])
      if (any(missingRows)) {
        new_testing_data[[featureName]][missingRows] <- featureMeans[[featureName]]
      }
    }
    completeTestingRows <- complete.cases(new_testing_data)
    if (any(!completeTestingRows)) {
      skippedWells <- sample_info$Well[testing_rows][!completeTestingRows]
      logger::log_warn(
        "Skipping {length(skippedWells)} prediction rows with missing values after RF mean imputation: {paste(skippedWells, collapse = ', ')}"
      )
    }
    # Running the random forest
    if (any(completeTestingRows)) {
      new_test_labels <- stats::predict(new_model, newdata = new_testing_data[completeTestingRows, , drop = FALSE])
    } else {
      new_test_labels <- numeric(0)
    }

    new_predictions <- data.frame(
      Compound = sample_info$Compound[testing_rows],
      Concentration = sample_info$Concentration[testing_rows],
      Index = rownames(testing_data)
    )
    new_test_labels <- data.frame(new_test_labels, Index = rownames(new_testing_data)[completeTestingRows])

    new_predictions <- new_predictions %>%
      dplyr::left_join(new_test_labels, by = "Index") %>%
      dplyr::rename("class"="new_test_labels")
    
    # capture well-level predictions
    ml@predictions <- new_predictions

    # Generating new data frames with numerical data
    compound_list <- unique(new_predictions$Compound)

    prediction_mean <- data.frame(check.names = FALSE)
    prediction_sd <- data.frame(check.names = FALSE)

    for (i in compound_list) {
      # create concentration list for each compound
      conc_list <- sort(unique(as.numeric(new_predictions[new_predictions$Compound == i, "Concentration"])))

      for (j in conc_list) {
        # create class list for each concentration of each compound
        class_df <- new_predictions[new_predictions$Compound == i & new_predictions$Concentration == j, "class"] %>%
          as.numeric() %>%
          as.data.frame()

        # calculate average prediction and standard deviation
        mean_class <- mean(class_df$., na.rm=TRUE)
        sd_class <- sd(class_df$., na.rm=TRUE)

        prediction_mean[i, as.character(j)] <- mean_class
        prediction_sd[i, as.character(j)] <- sd_class
      }
    }

    prediction_mean <- normalizePredictionConcentrationColumns(prediction_mean)
    prediction_sd <- normalizePredictionConcentrationColumns(prediction_sd)

    # capture prediction means and SDs
    ml@prediction_means <- prediction_mean
    ml@prediction_sds <- prediction_sd

    # Generating Heatmap, exported as pdf
    pred_heatmap <- buildPredictionHeatmap(
      predictionMatrix = as.matrix(prediction_mean),
      rect_gp = grid::gpar(col = "white", lwd = 2),
      column_title = "Concentration", column_title_side = "bottom", name = "Prediction",
      row_title = "Compound", cluster_rows = FALSE, cluster_columns = FALSE,
      show_column_dend = FALSE
    )

    fileName <- constructFileName(baseFileName = "CalFluxData_Tox_Heatmap", plateReadLabel = plateName, fileExtension = ".pdf")

    if (!is.null(pred_heatmap)) {
      pdf(fileName)
      print(pred_heatmap)
      dev.off()
    }

    # Making the dotplots with error bars, exported as pdf
    pred_plot_mean <- t(prediction_mean) %>% as.data.frame()
    pred_plot_mean <- setNames(cbind(rownames(pred_plot_mean), pred_plot_mean,
      row.names = NULL
    ), c("Concentration", colnames(pred_plot_mean)))

    pred_plot_sd <- t(prediction_sd) %>% as.data.frame()
    pred_plot_sd <- setNames(cbind(rownames(pred_plot_sd), pred_plot_sd,
      row.names = NULL
    ), c("Concentration", colnames(pred_plot_sd)))
    pred_plot_sd <- subset(pred_plot_sd, select = -Concentration)


    for (i in seq_along(unique(colnames(pred_plot_sd)))) {
      if (!grepl("sd", colnames(pred_plot_sd)[i])) {
        colnames(pred_plot_sd)[i] <- paste0("sd", colnames(pred_plot_sd)[i])
      }
    }

    compoundPlotList <- list()

    # concentration needs to be a character vector or factor
    orderedConcentrations <- colnames(prediction_mean)
    new_predictions$Concentration <- factor(as.character(new_predictions$Concentration), levels = orderedConcentrations)

    uniqueCompoundNames <- unique(new_predictions$Compound)


    for (i in seq_along(uniqueCompoundNames)) {
      compoundName <- uniqueCompoundNames[i]

      concDotPlot <- ggplot(
        data = new_predictions[new_predictions$Compound == compoundName, ],
        aes(x = Concentration, y = class)
      ) +
        geom_dotplot(binaxis = "y", stackdir = "center") +
        stat_summary(
          fun.data = mean_sdl, fun.args = list(mult = 1),
          geom = "errorbar", color = "red", width = 0.2
        ) +
        ggtitle(compoundName) + # aes(x = forcats::fct_inorder(Concentration)) + xlab("Concentration") +
        coord_cartesian(ylim = c(-0.1, 1.1)) +
        scale_y_continuous(breaks = seq(0, 1, 0.25)) +
        geom_point(size = 2) +
        ylab("Prediction")

      compoundPlotList[[i]] <- concDotPlot
    }

    fileName <- constructFileName(baseFileName = "CalFluxData_Tox_DotPlot", plateReadLabel = plateName, fileExtension = ".pdf")

    if (length(compoundPlotList) > 0) {
      pdf(fileName)
      for (firstToPlot in seq(1, length(compoundPlotList), by = 6)) {
        lastToPlot <- min(firstToPlot + 5, length(compoundPlotList))
        suppressMessages(do.call(gridExtra::grid.arrange, c(compoundPlotList[firstToPlot:lastToPlot], nrow = 3, ncol = 2)))
      }
      dev.off()
    } else {
      logger::log_warn("Skipping RF prediction dotplot export because no prediction plots were generated.")
    }

    pred_plot_df <- cbind(pred_plot_mean, pred_plot_sd)
    importanceTable <- getVariableImportanceTable(new_model)

    fileName <- constructFileName(baseFileName = "MachineLearning_Predictions", plateReadLabel = plateName, fileExtension = "xlsx")

    exportTabs <- list(
      Predictions = pred_plot_df,
      Variable_Importance = importanceTable
    )

    openxlsx::write.xlsx(exportTabs,
      file = fileName,
      rowNames = TRUE
    )
  }
  CalFluxData@mlResults[[plateName]] <- ml

  return(CalFluxData)
}


evaluateControlWellPCA <- function(CalFluxData) {
  pcaRes <- CalFluxData@pcaResults
  posCtrlKey <- CalFluxData@methodParameters$analysis_params$Positive_Control_Key
  negCtrlKey <- CalFluxData@methodParameters$analysis_params$Negative_Control_Key

  for (id in names(pcaRes)) {
    print(id)
    pca <- pcaRes[[id]]@controlWellsPCA

    if (is.null(pca) || isFALSE(pca[["PCA Available"]])) {
      reason <- if (is.null(pca)) "PCA result is missing." else pca[["PCA Failure Reason"]]
      logger::log_warn("Skipping control well PCA evaluation for {id}: {reason}")
      next
    }

    posCntl <- pca$`Wells PCA`$x[pca$Annotations$WellType == posControlKey, 1]
    negCntl <- pca$`Wells PCA`$x[pca$Annotations$WellType == negControlKey, 1]

    posCntl <- posCntl + min(posCntl)
    negCntl <- negCntl + min(negCntl)

    zScoreNeg <- abs((negCntl - median(negCntl)) / sd(negCntl))
    zScorePos <- abs((posCntl - median(posCntl)) / sd(posCntl))

    zScoreNeg <- zScoreNeg[zScoreNeg > 1.5]
    zScorePos <- zScorePos[zScorePos > 1.5]

    print(length(zScorePos))
    print(zScorePos)
    print(length(zScoreNeg))
    print(zScoreNeg)
  }
}

plotVariableImportance <- function(model, plateName){
  importance <- varImp(model, scale = TRUE)
  fileName <- constructFileName(baseFileName = "CalFluxData_Variable_Importance",
                                             plateReadLabel = plateName, fileExtension = ".pdf")
  pdf(fileName)
  print(plot(importance))
  dev.off()
}

getVariableImportanceTable <- function(model) {
  importance <- caret::varImp(model, scale = TRUE)
  importanceTable <- importance$importance

  if (is.null(importanceTable) || nrow(importanceTable) == 0) {
    return(data.frame())
  }

  importanceTable <- data.frame(
    Parameter = rownames(importanceTable),
    importanceTable,
    row.names = NULL,
    check.names = FALSE
  )

  scoreCols <- setdiff(colnames(importanceTable), "Parameter")
  if (length(scoreCols) > 0) {
    primaryScore <- scoreCols[1]
    importanceTable <- importanceTable[order(importanceTable[[primaryScore]], decreasing = TRUE), , drop = FALSE]
  }

  return(importanceTable)
}
