# The analysis.R file contains the high level method that makes calls on other methods for
# processing data and providing QC analyses. The buildAndProcessASOs() method sequentially calls
# methods that process ASO data according to an input file that specifies all methods.


#' This method is a convenience method that displays a file chooser for selecting an ASO Paramter Excel file for processing.
#' The method will run data procesing and output diagnostic plots and tables.
#' @returns Returns an aso object containing analysis parameters from the parameter file, input, refined and transformed plate data.
#' @export
processData <- function() {
  file <- file.choose()
  aso:::buildAndProcessASO(file)
}


#' This method is a wrapper method that performs all data processing operations according to the supplied ASO Parameter File.
#' The method will output data diagnostic plots and tables as specified in the parameter file. The transformed data will be read for classification analysis.
#' @param asoParamterXlsxFilePath a file path and file name for the ASO parameter xlsx file. All information required to process aso data is contained in this file.
#' @returns Returns an aso objecdt with loaded and transformed data. During processing several output summary files will be exported.
#' @export
runDataProcessing <- function(asoParameterXlsxFilePath) {

  # parse parameter and analysis settings, builds initial aso object.
  aso <- aso:::readAsoParameterFile(asoParameterXlsxFilePath)

  # loads the list of plate files specified in the parameter file and create file pairs
  # creating a plate set
  aso <- aso:::loadPlates(aso)

  # AUC parameter names in the plate data file tend to have Japanese Kanji encoded as unicode.
  # This method replaces those parameter names with a standardized text, just a patch
  aso <- aso:::aucParameterPatch(aso)

  # loads plate map files that identify the samples and conditions associated with each plate well.
  aso <- aso:::loadPlateMaps(aso)

  # this method uses well annotations from the plate map to convert data from 'empty' wells
  # or wells that shouldn't be use, from numeric values to NaN (not-a-number values)
  aso <- aso:::maskEmptyWells(aso)

  # users may select to only use some of the input parameters
  # this method removes data for parameters that should not be used (based on the parameter file)
  # only the key parameter's data will move on.
  aso <- aso:::fiterToSelectedParameters(aso)

  # Users can specify a short abbreviation to replace the full parameter names.
  # The parameter file can hold these abbreviations and set these abbreviations to be used in output instead of the longer parameter names.
  aso <- aso:::setParameterAbbreviaions(aso)

  # apply value imputations and make categorical results numeric - if specified
  # some parameters don't have numeric values, but rather text values that describe the parameter.
  # this step will optionally apply a mapping or conversion from text values to discrete numeric values.
  aso <- aso:::applyCategoricalToNumeric(aso)

  # assess data completeness for each parameter. This holds information on how many plate values
  # are empty, zero, have 'blank' values meaning no data, or have been marked as 'masked'
  aso <- aso:::assessDataCompleteness(aso)

  # the parameter file will specify how complete a parameter's data has to be in order to keep that parameter in the dataset.
  # if a given parameter has very sparse data, it may be best to exclude that parameter for downstream analysis.
  # A user threshold determines how much missing data can be tolerated.
  aso <- aso:::applyDataCoverageFilters(aso)

  # export of the data coverage tables
  # this method will create an excel file that provides information on the data coverage assessment.
  aso:::exportDataCoverageReport(aso)

  # apply optional N/A and blank value replacements
  # some parameters can have values of 'N/A' or 'Blank'.
  # The lab has asked for a way to replace these values. The drop-in value or techinque for replacement is specified in the paramter input file.
  # replacement of these values is optional for each parameter.
  aso <- aso:::replaceBlanksAndNAValues(aso)

  # harmonize parameters - make sure all plates in pairs have the same parameters (data cols)
  # the experiments compare an ASO treatment plate read (plate data set) to an untreated (time=0) control plate read.
  # This method makes sure that after possibly filtering  out parameters, plate data sets (T=0 and experimental plate read)
  # both have the same parameters reported, in commmon.
  aso <- aso:::harmonizeParameters(aso)

  # This method runs Quality Control checks on the reference plate data (T=0 plate)
  # This will report on things like parameter variability, and wells that tend to be outliers, perahps having bad data (a bad well)
  aso <- aso:::runQcForReferencePlate(aso)

  # now impute existing missing data as specified in input parameters
  # for now, drop standard impute function
  # aso <- aso:::imputeData(aso)

  # run transformations on plate set plate-pairs
  # this compares the experimental condition data to the reference or control data.
  # This produces a new data frame/table that will report on the effect of treatment.
  # Currently we report the log2(experimental_condition_value/control_value), a log base 2 fold change.
  aso <- aso:::transformData(aso)

  # create optional plate view pdf.
  # this will show plate views for each paramter, in plate-format.
  # These can reveal wells that tend to be outliers.
  aso:::exportPlateViews(aso)

  # create optional bar-chart
  # This exports the log2FoldChange (treatment effect) as bar charts.
  # Barcharts show treatment effect on the y-axis, and ASO concentration on the x-axis.
  aso:::exportBarCharts(aso)

  # this runs a paired t-test to report on which wells show different values after treatment, for each parameter.
  # The method stores the result in an aso object.
  ttDf <- aso:::runPairedTTest(aso)

  # exports the paired t-test results.
  aso:::exportPairedTTestResult(aso, ttDf, "file")

  print("Analyis Done")

  aso:::zFactorXlsx(aso)

  aso:::exportTransformedData(aso)

  # returns the aso data object
  return(aso)
}


#' This method loads plate data according the the plate files specified within the ASO parameter file.
#' One plate object is created for each input plate file and the plate pairs are stored within the returned ASO object.
#' @param aso the input ASO file generated by readAsoParameterFile
#' @returns returns an aso object containing a plateset object with loaded plate data.
loadPlates <- function(aso) {
  fileList <- aso@methodParameters[['plate_file_list']]
  plateSet = new("plateset")
  plates <- list()
  dir <- aso@methodParameters[['root_dir']]

  for(platename in names(fileList)) {
    #print(paste0("adding new plate with name: ",platename))
    file <- fileList[[platename]]
    plate <- aso::read_stemonix_data(filename=paste0(dir,file))
    plateSet <- aso::addPlate(plateSet, plate, platename)
    #print(paste0("added plate from: ",dir,file))
  }
  # add the plateset
  aso@plateSet <- plateSet
  return(aso)
}

#' This function loads plate annotations, plate maps as specified in the aso object after initializing with parameter file
#' @param aso the input ASO file generated by readAsoParameterFile
#' @returns returns an aso object containing a plateset object with loaded plate data and plate map annotations.
loadPlateMaps <- function(aso) {

  if(is.null(aso)) {
    print("Null aso object. Please use readAsoParameterFile() to initialize the aso file prior to loadPlateMaps()")
    return()
  }

  fileList <- aso@methodParameters[['plate_map_list']]
  dir <- aso@methodParameters[['root_dir']]

  if(is.null(fileList) || is.null(dir)) {
    print("Null root directory or plate file list in paramters.
    Please check the parameters file for the root directory and plate list information.
    Then reload the updated parameter file using readAsoParameterFile(aso) function.")
    return(aso)
  }

  for(platename in names(fileList)) {
    file <- fileList[[platename]]
    plate <- aso@plateSet@plates[[platename]]
    plate <- aso:::read_stemonix_metadata(paste0(dir,file), plate)
    aso@plateSet@plates[[platename]] <- plate
  }

  return(aso)
}


#' Method updates parameter abbreviations to those specified in the parameter file
#' @param aso aso object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the aso object with parameter abbreviations set to use for all downstream reports.
setParameterAbbreviaions <- function(aso) {

  keyCols = c('Statistic','Suggested_Abbreviation')

  if(!all(keyCols %in% colnames(aso@parameterInfo))) {
    print("Parameter information doesn't have key column names 'Statistic' and 'Suggested_Abbreviation'.
          Please update the parameter file to include these columns.
          Note that column names need to match these names exactly, including case.")
  }

  abbrInfo <- aso@parameterInfo[,keyCols]

  for(platename in names(aso@plateSet@plates)) {
    plate <- aso@plateSet@plates[[platename]]
    plate <- applyParameterAbbreviations(plate, abbrInfo)
    aso@plateSet@plates[[platename]] <- plate
  }
  return(aso)
}


####################
#####
##### high level methods working on the ASO object
#####
####################

#' Converts well vales to NaN for wells that are annotated in the platemap as "Empty".
#' Downstream methods will not be impacted by these well values.
#' @param aso aso object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the aso object with specified wells masked.
maskEmptyWells <- function(aso) {

  for(plateName in names(aso@plateSet@plates)) {
    plate <- aso@plateSet@plates[[plateName]]
    plateMap <- plate@plateAnnotMap
    df <- plate@plateData

    # set data to empty, and treated as NAN
    df[plateMap$Compound == "Empty",] <- "Empty"

    # set masked data
    df[plateMap$Mask == 1,] = "Masked"

    plate@plateData <- df
    aso@plateSet@plates[[plateName]] <- plate
  }

  return(aso)
}

#' Reduces the plate data object to only contain data for the subset of selected parameters.
#' @param aso aso object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the aso object with working set of plate data reduced to parameters specified in the parameter file.
fiterToSelectedParameters <- function(aso) {
  pInfo <- aso@parameterInfo
  paramsToKeep <- unlist(pInfo$Statistic[pInfo$Use == 1])

  for(platename in names(aso@plateSet@plates)) {
    plate <- aso@plateSet@plates[[platename]]
    plate <- filterParametersOnList(plate, paramsToKeep)
    aso@plateSet@plates[[platename]] <- plate
  }
  return(aso)
}


#' Converts categorical data to numeric values according to specified rules in the input parameter file.
#' Note that this method will only run on parameters that are flagged as having categorical data.
#' @param aso aso object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the aso object categorical parameters converted to numeric values as specified in the parameter file.
applyCategoricalToNumeric <- function(aso) {

  # get parameters to convert
  paramsToMap <- aso@parameterInfo[!is.na(aso@parameterInfo$cat_to_num_map),]

  for(platename in names(aso@plateSet@plates)) {
    plate <- aso@plateSet@plates[[platename]]

    for(i in 1:nrow(paramsToMap)) {
      abbrParam <- paramsToMap[i,2]
      keys <- paramsToMap[i,'cat_to_num_map']
      keys <- unlist(strsplit(keys,split=","))
      for(j in 0:length(keys)) {
        keys[j] <- trimws(keys[j])
      }

      vals <- paramsToMap[i,'cat_to_num_map2']

      vals <- unlist(strsplit(vals, split=","))
      for(k in 0:length(vals)) {
        vals[j] <- trimws(vals[j])
      }
      vals <- as.numeric(vals)

      plate <- aso::encodeDiscreteParameters(plate, abbrParam, keys, vals)
      aso@plateSet@plates[[platename]] <- plate
    }
  }
  return(aso)
}

#' Replaces 'Blank' or Empty data and N/A data for each parameter according to specified rules for each parameter.
#' @param aso aso object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the aso object categorical parameters converted to numeric values as specified in the parameter file.
replaceBlanksAndNAValues <- function(aso) {
  paramInfo <- aso@parameterInfo[aso@parameterInfo$Use == 1, ]
  naParams <- paramInfo[paramInfo$NA_Replace != 'N',]
  naParamList <- naParams$NA_Replace
  names(naParamList) <- naParams$Suggested_Abbreviation

  blankParams <- paramInfo[paramInfo$Blank_Replace != 'N',]
  blankParamList <- blankParams$Blank_Replace
  names(blankParamList) <- blankParams$Suggested_Abbreviation

  refinedPlateSet <- replaceBlanksAndNAs(aso@plateSet, blankParamList, naParamList)
  aso@plateSet <- refinedPlateSet

  return(aso)
}

#' Runs an analysis on the aso object to report on data coverage for each parameter.
#' The missing data report is entered into the aso object.
#' @param aso aso object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the aso object containing the missing data report in the dataCoverageTables slot
assessDataCompleteness <- function(aso) {
  dataCoverage <- list()

  plates <- aso@plateSet@plates
  pInfo <- aso@parameterInfo[aso@parameterInfo$Use == 1, c(1,2)]

  print("pInfo... use params for assessments...data cov")
  print(dim(pInfo))

  for(platename in names(plates)) {
    plate <- plates[[platename]]
    missingValReport <- missingDataReport(plate=plate, plateSize=plate@format)

    # lets append the parameter names
    missingValReport <- merge(pInfo, missingValReport, by.x='Suggested_Abbreviation', by.y='Parameter', all.x=F, all.y=T, sort=F)

    missingValReport <- missingValReport[,c(2,1,3:(ncol(missingValReport)))]

    colnames(missingValReport)[1] <- 'Param_Name'
    colnames(missingValReport)[2] <- 'Parameter'
    dataCoverage[[platename]] <- missingValReport
  }

  aso@plateSet@dataCoverageTables <- dataCoverage

  return(aso)
}


#' This method optionally removes parameters that have a large amount of missing data, beyond specified levels.
#' @param aso aso object initialized with parameter file, plate data and plate maps loaded.
#' @returns returns the aso object with plate data reduced to parameters passing a minium data limit as specified in the parameter file.
applyDataCoverageFilters <- function(aso) {
  # for each plate loop over paramters to check their coverage and apply limits

  # get lower coverage limit for each parameter
  pInfo <- aso@parameterInfo[aso@parameterInfo$Use == 1, c('Statistic', 'Suggested_Abbreviation', 'Min_Data_Coverage_PCT')]

  # pSize (plate size) has to be reduced to the number of non-empty wells.
  pSize <- aso@plateSet@dataCoverageTables[[1]]$Keeper_Wells[1]
  pInfo$max_data_loss <- pSize - ceiling((pInfo$Min_Data_Coverage_PCT/100.0) * pSize)

  plateSet <- aso@plateSet
  dataCoverage <- plateSet@dataCoverageTables

  for(n in names(dataCoverage)) {
    dcov <- dataCoverage[[n]]
    dcov <- merge(dcov, pInfo, by.x = 'Parameter', by.y = 'Suggested_Abbreviation', sort=F)
    dcov$keep <- T
    dcov$keep[dcov$Missing_Count > dcov$max_data_loss] <- F

    if('Statistic' %in% colnames(dcov)) {
      dcov <- subset(dcov, select=c(-Statistic))
    }
    dataCoverage[[n]] <- dcov
  }

  aso@plateSet@dataCoverageTables <- dataCoverage

  plates <- aso@plateSet@plates
  dataCoverage <-  aso@plateSet@dataCoverageTables
  for(platename in names(plates)) {
    plate <- plates[[platename]]
    dcov <- dataCoverage[[platename]]
    colsToKeep <- unlist(dcov[dcov$keep,1])
    plate@plateData <- plate@plateData[,colsToKeep]
    plate@paramAbbrs <- colnames(plate@plateData)
    plate@parameters <- dcov$Param_Name[dcov$keep == TRUE]
    plates[[platename]] <- plate
  }

  aso@plateSet@plates <- plates

  return(aso)
}


#' Exports the data coverage report that lists parameters and information on missing data.
#' @param aso the aso object containing pre-computed data coverage results
exportDataCoverageReport <- function(aso) {
  rootDir <- aso@methodParameters[['root_dir']]
  fileName = format(Sys.time(), "Plate_Data_Coverage_Status_Reports_%Y%m%d_%H%M")
  tabs <- aso@plateSet@dataCoverageTables
  openxlsx::write.xlsx(tabs, file = paste0(rootDir,fileName,".xlsx"))
}

#' Makes sure that the reference plate read and the treatment plate read have the same parameters
#' after possibly filtering based on data loss.
#' @param aso the aso object having plate read pairs for comparison
harmonizeParameters <- function(aso) {
  aso@plateSet <- harmonizeParametersAcrossPlates(aso@plateSet)
  return(aso)
}

#' Runs quality control on the reference (control) plate read, prior to treatment.
#' This collects information on parameter variation (standard deviation, and coefficient of variation)
#' and z-scores for each parameter and each well help to identy outlier/bad wells that we might want to 'mask'
#' @param aso the aso object having a platepair object with a loaded reference plate
runQcForReferencePlate <- function(aso) {
  runReferenceQC(aso@plateSet, aso@methodParameters$root_dir, aso@methodParameters$plate_file_list[[1]])
  return(aso)
}

#' Performs missing data imputation, replacing missing data according to a supplied method name.
#' Note that this method is not currently used in typical pre-processing.
#' @param aso an aso object that has a platepair and processing parameters
#' @return the aso object now having missing values imputed
imputeData <- function(aso) {
  params <- aso@methodParameters$analysis_params
  impMethod <- params[['Imputation_Method']]
  if(impMethod == 'per_param_pct_min') {
    fractionMin = as.numeric(params[['Imputation_Fraction_Min']])
    imputeCols <- aso:::getColumnAbbrToImpute(aso)
    aso@plateSet <- aso:::imputePercentMin(aso@plateSet, pctMin = fractionMin, imputeCols)
  }
  return(aso)
}

#' helper function to determine which parameters should undergo imputation.
#' @param aso an aso object having plate data loaded ans parameters to control the imputation process
#' @return a list object with two fields incicating which columns to impute for zeros or NAs,
getColumnAbbrToImpute <- function(aso) {
  pInfo <- aso@parameterInfo
  zeroReplaceCols <- pInfo$Suggested_Abbreviation[pInfo$Zero_Replace == 1]
  naReplaceCols <- pInfo$Suggested_Abbreviation[pInfo$NA_Replace == 1]
  imputeCols <- list()
  imputeCols[['zero_impute']] <- zeroReplaceCols
  imputeCols[['na_impute']] <- naReplaceCols
  return(imputeCols)
}

#' Transforms the plate data, currently taking a log base 2 transformation of the ratio of the data
#' This produces a single table that reports on the comparison between the reference or control state
#' versus the aso treated state.
#' @param aso an aso object having plate data (a platepair object) to transform
#' @return an aso object with platepair object now containing the transformed data
transformData <- function(aso) {
  i = 1
  plateNames = c()

  for(plateName in names(aso@plateSet@plates)) {
    plate <- aso@plateSet@plates[[plateName]]
    plateNames <- c(plateNames, plateName)
    if(i %% 2 == 0) {
      aso@plateSet <- dataTransform(aso@plateSet, platePair=plateNames, method = 'log2ratio', firstDataCol = 1)
      plateNames = c()
    }
    i = i + 1
  }
  return(aso)
}

#' Exports views of the plate data, for each parameter, as a pdf file.
#' The exported file will include the date and time of file creation.
#' @param aso an aso object containing plate data and having transformed data
exportPlateViews <- function(aso) {

  plateSet <- aso@plateSet
  i = 1
  plateNames = c()
  ptoPlot = c()

  for(plateName in names(aso@plateSet@plates)) {
    plate <- aso@plateSet@plates[[plateName]]
    plateNames <- c(plateNames, plateName)

    # reports on each pair of plate reads
    if(i %% 2 == 0) {
      ptoPlot <- plate@paramAbbrs
      plotgrid <- trellisPlateSetViewsPairViews(plateSet, platePair=plateNames, paramsToPlot=ptoPlot, showRowColumnLabels = F,
                                                maxSatCount = 10, showLegend = F, tMethod = 'log2ratio',
                                                ggplot=T, elementDividers = F)
      plateNames = c()
    }
    i = i + 1
  }
  rootDir <- aso@methodParameters[['root_dir']]
  fileName = format(Sys.time(), "Plate_Data_Coverage_Status_Reports_%Y%m%d_%H%M")
  fileName = paste0(rootDir,fileName,".pdf")
  pdf(fileName, height = length(ptoPlot)*2, width = 8.5)
  gridExtra::grid.arrange(plotgrid)
  dev.off()
}

#' This function exports a series of barcharts for each ASO and each parameter into a pdf file.
#' @param aso an aso object having transformed data.
exportBarCharts <- function(aso) {
  samples <- unique(aso@plateSet@plates[[1]]@plateAnnotMap$Compound)
  samples <- samples[samples != ""]

  ### need to refer to input for control compound name/tag
  samples <- samples[!(samples %in% c("Veh1", "Veh2"))]

  # need to adjust to work over multiple plate pairs in plate set...
  plateset <- aso@plateSet
  params <- colnames(aso@plateSet@transformedPlateData[[1]])
  plotgrid <- aso:::getBarChartTrellis(plateSet = plateset, samples=samples, parameters=params, samplesIn = 'rows', tMethod = 'log2Ratio')
  gridDim <- dim(plotgrid)
  rootDir <- aso@methodParameters[['root_dir']]

  fileName = format(Sys.time(), "Sample_Bar_Charts_%Y%m%d_%H%M")
  fileName = paste0(rootDir,fileName,".pdf")

  pdf(fileName, height = gridDim[1] * 2, width = gridDim[2] * 2)
  gridExtra::grid.arrange(plotgrid)
  dev.off()
}

#' performs a statistical analysis
#' @param aso an aso object having transformed data
#' @param analysis the name of the analysis type, one of c('paired-t-test'), paired-t-test is the default.
statAnalyis <- function(aso, analysis="paired-t-test") {
  if(analysis == 'paired-t-test') {
    aso = runPairedTTest(aso)
  }
  return(aso)
}

#' Performes a paired t-test to compare ASO treated samples from the reference/control state.
#' @param an aso object having loaded plate data.
#' @return a dataframe containing stat results for each parameter, for each well.
runPairedTTest <- function(aso) {
  plateset <- aso@plateSet
  plateNames <- names(plateset@plates)
  # return if the aso does not have an even number of plates (plate reads)
  if(length(plateNames) %% 2 != 0) {
    return(aso)
  }
  results <- list()
  lfcVals <- list()

  i = 1

  for(name in plateNames) {

    if(i %% 2 == 1) {

      refPlate <- plateset@plates[[name]]
      plateMap <- plateset@plates[[1]]@plateAnnotMap

      compounds <- unique(plateMap$Compound)
      compounds <- compounds[!is.na(compounds)]
      compounds <- compounds[compounds != ""]
      compounds <- compounds[compounds != "Empty"]
      compounds <- compounds[compounds != "Masked"]

    } else {

      exptPlate <- plateset@plates[[name]]

      for(compound in compounds) {
        currCov <- plateMap[plateMap$Compound == compound,]
        concs <- unique(currCov$Concentration)
        nonZero <- sum(concs != 0)
        if(nonZero == 0) {
          next
        }

        concs <- concs[concs != 0]
        for(conc in concs) {
          wells <- currCov[currCov$Concentration == conc,]$Well
          refData <- aso:::getPlateDataByWellSet(plate = refPlate, wells)
          exptData <- aso:::getPlateDataByWellSet(plate = exptPlate, wells)

          if(ncol(refData) != ncol(exptData)) {
            next
          }

          for(param in colnames(refData)) {
            d1 <- refData[,param]
            d2 <- exptData[,param]

            data <- data.frame(d1)
            data$d2 <- d2

            # need to convert to numeric
            data <- data.frame(lapply(data, FUN=as.numeric))
            data <- na.omit(data)
            meanExpt = mean(data[,2], na.rm=T) + 0.001
            meanRef = mean(data[,1], na.rm=T) + 0.001
            if(is.na(meanExpt)) {
              meanExpt = 0.001
            }
            if(is.na(meanRef)) {
              meanRef = 0.001
            }

            lfc <- log(mean(data[,2]+0.001,na.rm=T)/mean(data[,1]+0.001, na.rm=T),2)

            if(nrow(data) > 2) {
              resultName <- paste0(compound,"_",conc,"_",param)

              # 0 variance is not tolerated well
              if(!(sd(data[,2]-data[,1]) == 0)) {
                res <- t.test(x=data[,2], y=data[,1], paired = TRUE, alternative = "two.sided")
                results[[resultName]] <- res
              } else {
                ## equal variance case ...create result with NA values
                res <- list()
                res[['method']] <- 'paired.ttest.0.vaiance'
                res[['statistic']] <- NaN
                res[['p.value']] <- NaN
                results[[resultName]] <- data.frame(res)
              }
              lfcVals[[resultName]] <- lfc
            }
          }
        }
      }
    }
    i = i + 1
  }

  df <- data.frame(matrix(ncol=9, nrow=0))
  for(resName in names(results)) {
    res <- results[[resName]]
    #print(res)
    nameVals <- unlist(strsplit(resName,split="_"))
    sample <- nameVals[1]
    dose <- nameVals[2]
    param <- paste(nameVals[3:length(nameVals)], collapse="_")
    lfc <- lfcVals[[resName]]
    resList <- list(sample=sample, dose=dose, treatment=paste0(sample,'_',dose), param=param, condition=resName, method=res$method, tval=res$statistic, pval=res$p.value, log2FoldChange=lfc)
    df <- rbind(df,resList)
  }
  colnames(df) <- c("Sample", "Conc", "Treatment", "Parameter", "Condition", "TestMethod", "T", "pValue", "log2FoldChange")

  df <- df[order(df$pValue),]
  df$adjP <- p.adjust(unlist(df$pValue), method="BH")
  df$Conc <- as.numeric(df$Conc)
  df <- df[order(df$Sample, df$Parameter, df$Conc),]
  # add -log(adjP)
  df$`-log10(adjP)` <- -1*log(df$adjP,10)

  return(df)
}

zFactor <- function(aso, dataType = 'transformed', masking = ""){
  # The @ is used to dereference
  # Here we have the aso object, dereference the plateset object, and finally get the transformedPlateData
  # it's designed as a list or dictionary
  plateTransformedData <- aso@plateSet@transformedPlateData

  # an R list object is dereferenced using double brackets
  # you can refer to a member of the list by name or by index. *R indexing starts at 1
  if(dataType == 'transformed'){
    testData <- plateTransformedData[['60min_vs_Ref_(log2ratio)']]
  } else {
    testData <- aso@plateSet@plates[[2]]@plateData
  }
  # the platemap holds well annotations, it's a dataframe
  # this gets the plateset from the aso object, plate list, takes the first plate, and then
  # the plateAnnotationMap field.
  plateMap <- aso@plateSet@plates[[1]]@plateAnnotMap

  # an alternative to viewing a table is using the 'Global Envionment' in the upper right window.
  # if the variable is a data.frame, click on the table icon on the far right to view the data.

  # Combines plateMap and transformedData
  tableWithAnnotation <- cbind(plateMap, testData)

  # Created a data frame that does not contain empty wells
  splicedData <- tableWithAnnotation[tableWithAnnotation$Compound != 'Empty',]

  if(masking != ""){
    splicedData = splicedData[splicedData$Well != masking,]
  }

  # Create a data frame that only contains positive controls
  PosCtrl = splicedData[splicedData$WellType == "Positive Control",]
  # unique(PosCtrl$WellType)

  # Create a data frame that only contains negative controls
  NegCtrl = splicedData[splicedData$WellType == "Negative Control",]
  # unique(NegCtrl$WellType)

  # make means and sds data frame for both controls
  numericPosCtrl <- data.frame(sapply(PosCtrl[,(ncol(plateMap)+1):ncol(PosCtrl)], as.numeric))
  allUPosCtrl <- colMeans(numericPosCtrl)
  allSDPosCtrl <- sapply(numericPosCtrl, sd)

  numericNegCtrl <- data.frame(sapply(NegCtrl[,(ncol(plateMap)+1):ncol(NegCtrl)], as.numeric))
  allUNegCtrl <- colMeans(numericNegCtrl)
  allSDNegCtrl <- sapply(numericNegCtrl, sd)

  zFactor = 1 - ((3*(allSDPosCtrl+allSDNegCtrl))/(abs(allUPosCtrl - allUNegCtrl)))

  coeffVarPosCtrl = allSDPosCtrl/abs(allUPosCtrl)
  coeffVarNegCtrl = allSDNegCtrl/abs(allUNegCtrl)

  # make a data frame with all means, sds, and zFactor and cbind them together
  zFactorTable <- cbind(data.frame(allUPosCtrl), data.frame(allSDPosCtrl), data.frame(coeffVarPosCtrl),
        data.frame(allUNegCtrl), data.frame(allSDNegCtrl), data.frame(coeffVarNegCtrl),
        data.frame(zFactor))

  return(zFactorTable)
}


zFactorXlsx <- function(aso, fileName = 'zPrimeResults.xlsx'){
  # Make a list
  zPrimeList <- list()

  # First call to zFactor calculation to calculate for transformed data
  zPrimeList[['transformedDataResults']] <- aso:::zFactor(aso)

  # Second call to zFactor to calculate for raw data
  zPrimeList[['rawDataResults']] <- aso:::zFactor(aso, 'experimental')

  # Write to an Excel file
  openxlsx::write.xlsx(zPrimeList,
                       file = paste0(format(Sys.time(), 'zPrimeResults_%Y%m%d_%H%M'), '.xlsx'),
                       rowNames = TRUE)

  return(zPrimeList)
}


runPca = function(aso, dataType = 'transformed', compoundIDList = NULL, scale=T){
  plateTransformedData = aso@plateSet@transformedPlateData

  plateMap <- aso@plateSet@plates[[1]]@plateAnnotMap

  if(dataType == 'transformed'){
    testData <- plateTransformedData[['60min_vs_Ref_(log2ratio)']]
  } else if (dataType == "raw"){
    testData <- aso@plateSet@plates[[2]]@plateData
  } else {
    testData = plateTransformedData[[1]]
    testData = testData[plateMap$Compound %in% compoundIDList,]
    plateMap = plateMap[plateMap$Compound %in% compoundIDList,]
  }

  tableWithAnnotation <- cbind(plateMap, testData)

  for(i in 1:nrow(tableWithAnnotation)){
    if(tableWithAnnotation[i,"Compound"] == "M1_IDT"){
      tableWithAnnotation[i,"Compound"] = "Positive Control"
    }
    if(tableWithAnnotation[i,"Compound"] == "Veh") {
      tableWithAnnotation[i,"Compound"] = "Negative Control"
    }
  }

  splicedData <- tableWithAnnotation[tableWithAnnotation$Compound != 'Empty',]

  reducedPlateMap = splicedData[, 1:ncol(plateMap)]

  #create dataframe with test data that does not include plate map
  testDataSplice = splicedData %>% dplyr:::select(ncol(plateMap):last_col())
  testDataSpliceVar = testDataSplice[ , which(apply(testDataSplice, 2, var) != 0)]
  #transpose
  testDataSpliceT = t(testDataSplice)
  #transposition caused columns with zero variance, remove those
  testDataSpliceTvar = testDataSpliceT[ , which(apply(testDataSpliceT, 2, var) != 0)]

  pcaASO = prcomp(na.omit(testDataSpliceVar), center = TRUE, scale. = scale, retx = TRUE)
  pcaASOT = prcomp(na.omit(testDataSpliceTvar), center = TRUE, scale. = scale, retx = TRUE)

  pcaList = list()
  pcaList[['Wells PCA']] = pcaASO
  pcaList[['Parameters PCA']] = pcaASOT

  #add something in the list to add the annotation
  pcaList[['Annotations']] = reducedPlateMap
  pcaList[['Parameter Names']] = colnames(testData)


  return(pcaList)

}

plotPCA = function(pcaList, pcaType = 1, plotLoadings=F){

  if(pcaType == 1){
    plotTitle = 'Wells PCA'

    labels = pcaList[['Annotations']]$Compound

    pca_prop_var = (pcaList[[pcaType]]$sdev^2/sum(pcaList[[pcaType]]$sdev^2))

    if(!plotLoadings) {
      pcaPlot = ggplot(data.frame(pcaList[[pcaType]]$x), aes(x=PC1, y=PC2, color = labels)) +
        geom_point(size = 3) +
        labs(title = plotTitle) +
        xlab(paste0("PC1 (", round(pca_prop_var[1]*100, digits = 2), "% of var.)")) +
        ylab(paste0("PC2 (", round(pca_prop_var[2]*100, digits = 2), "% of var.)"))
    } else {

      pcaProj <- pcaList[[pcaType]]$x
      pcLoadings <- pcaList[[pcaType]]$rotation

      xL <- unlist(pcLoadings[,1])
      yL <- unlist(pcLoadings[,2])
      loadData <- data.frame(xL, yL)

      pcaPlot = ggplot(data.frame(pcaList[[pcaType]]$x), aes(x=PC1, y=PC2, color = labels)) +
        geom_point(size = 3) +
        labs(title = plotTitle) +
        xlab(paste0("PC1 (", round(pca_prop_var[1]*100, digits = 2), "% of var.)")) +
        ylab(paste0("PC2 (", round(pca_prop_var[2]*100, digits = 2), "% of var.)"))

      pcaPlot2 <- pcaPlot + geom_point(data=loadData, aes(x=xL,y=yL, color="blue"))


    }

  } else {
    plotTitle = 'Parameters PCA'

    labels = pcaList[['Parameter Names']]

    pcaPlot = ggplot(data.frame(pcaList[[pcaType]]$x), aes(x=PC1, y=PC2, color = labels)) +
      geom_point(size = 3) + labs(title = plotTitle) +
      xlab(paste0("PC1 (", round(pca_prop_var[1]*100, digits = 3), "% of var.)")) +
      ylab(paste0("PC2 (", round(pca_prop_var[2]*100, digits = 3), "% of var.)"))


    pcaPlot = pcaPlot()

  }



  pdf(file = paste0(format(Sys.time(),'PCA_%Y%m%d_%H%M_%S'),'.pdf'))
  print(pcaPlot)
  dev.off()

  return(pcaPlot)

}

#' generatees random forest predictions for ASOs
#' @param aso an aso object containing transformed data that's ready for analysis
#' @param masking list of wells to optionally mask
#' @returns creates a prediction heatmap and dot plot, returns a Heatmap object.
#' @export
asoRandomForest = function(aso, masking = ''){
  #Subsetting data
  data_imputed<-aso@plateSet@transformedPlateData[[1]]
  sample_info <- aso@plateSet@plates[[1]]@plateAnnotMap

  data_imputed <- data_imputed[which(sample_info$Compound!="Empty"),]
  sample_info <- sample_info %>%
    filter(Compound!="Empty")
  data_imputed <- data_imputed %>% select_if(~ !any(is.na(.)))

  #Build a data table that contains required data (can be updated)
  pca_res <- prcomp(data_imputed, scale.=TRUE)
  pca_df <- pca_res$x[,1:2] %>%
    as.data.frame() %>%
    mutate(WellType = sample_info$WellType) %>%
    mutate(Compound = sample_info$Compound) %>%
    mutate(Well = sample_info$Well) %>%
    mutate(Concentration = as.character(sample_info$Concentration))

  if(masking != ''){
    pca_df = pca_df[pca_df$Well != masking,]
  }

  #Subset data to get all required training data
  data_labels <- pca_df$WellType
  training_rows <- which(data_labels == "Positive Control" | data_labels == "Negative Control")
  training_labels <- factor(data_labels[training_rows])
  training_data <- data_imputed[training_rows,] %>% as.data.frame
  training_data$label <-training_labels
  concentration_labels <- as.numeric(pca_df$Concentration[training_rows])
  new_training_data = cbind(concentration_labels, training_data)

  #Generate new labels that are numeric (between 0 and 1)
  cont_training_data = new_training_data
  updated_labels = data.frame()
  for(i in 1:length(cont_training_data$concentration_labels)){
    if(cont_training_data$label[i] == "Positive Control"){
      updated_labels[i,1] = as.numeric(1)
    } else {
      updated_labels[i,1] = as.numeric(0)
    }
  }

  #Remove old label column and add new label column
  colnames(updated_labels) = "label"
  cont_training_data = subset(cont_training_data, select = -label)
  cont_training_data = cbind(cont_training_data, updated_labels)

  #Train model
  new_model <- caret::train(label ~ .,
                     data =  cont_training_data,
                     method = "rf",
                     trControl = caret::trainControl(method = "cv",number=10))

  #Subset data for testing
  testing_rows = which(data_labels == 'Test' & pca_df$Compound != 'PBS')
  testing_data <-  data_imputed[testing_rows,] %>% as.data.frame
  clean_conc_labels = as.numeric(pca_df$Concentration[testing_rows])
  new_testing_data = cbind(clean_conc_labels, testing_data)
  colnames(new_testing_data)[1] = "concentration_labels"

  #Running the random forest
  new_test_labels <- stats::predict(new_model, newdata = new_testing_data)
  new_predictions <- data.frame(aso=pca_df$Compound[testing_rows],Concentration = pca_df$Concentration[testing_rows],
                                class=new_test_labels)

  #Generating new data frames with numerical data
  compound_list = unique(new_predictions$aso)

  prediction_mean = data.frame()
  prediction_sd = data.frame()

  for(i in compound_list){
    #create concentration list for each compound
    conc_list = unique(new_predictions[new_predictions$aso == i, 'Concentration'])

    for(j in conc_list){
      #create class list for each concentration of each compound
      class_df = new_predictions[new_predictions$aso == i & new_predictions$Concentration == j, 'class'] %>%
        as.numeric %>%
        as.data.frame()

      #calculate average prediction and standard deviation
      mean_class = mean(class_df$.)
      sd_class = sd(class_df$.)

      prediction_mean[i,j] = mean_class
      prediction_sd[i,j] = sd_class

    }
  }

  #Generating Heatmap, exported as pdf
  pred_heatmap = ComplexHeatmap::Heatmap(as.matrix(prediction_mean), rect_gp = grid::gpar(col = "white", lwd = 2),
                         column_title = "Concentration", column_title_side = "bottom", name = 'Prediction',
                         row_title = "ASO", cluster_rows = FALSE, show_column_dend = FALSE,
                         column_order = order(as.numeric(gsub("column", "", colnames(prediction_mean)))))
  pred_heatmap
  pdf("ASOHeatmap.pdf")
  print(pred_heatmap)
  dev.off()

  #Making the dotplots with error bars, exported as pdf
  pred_plot_mean = t(prediction_mean) %>% as.data.frame()
  pred_plot_mean = setNames(cbind(rownames(pred_plot_mean), pred_plot_mean,
                                  row.names = NULL), c("Concentration", colnames(pred_plot_mean)))

  pred_plot_sd = t(prediction_sd) %>% as.data.frame()
  pred_plot_sd = setNames(cbind(rownames(pred_plot_sd), pred_plot_sd,
                                row.names = NULL), c("Concentration", colnames(pred_plot_sd)))
  pred_plot_sd = subset(pred_plot_sd, select = -Concentration)


  for(i in 1:length(unique(colnames(pred_plot_sd)))){
    if(!grepl("sd", colnames(pred_plot_sd)[i])){
      colnames(pred_plot_sd)[i] = paste0("sd",colnames(pred_plot_sd)[i])
    }
  }

  aso_plot_list = list()
  for(i in 1:length(unique(new_predictions$aso))){

    concDotPlot = ggplot(data = new_predictions[new_predictions$aso == paste0('ASO', i),],
                         aes(x = Concentration, y = class)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center') +
      stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                   geom="errorbar", color="red", width=0.2) +
      ggtitle(paste0('ASO', i)) + aes(x = forcats::fct_inorder(Concentration)) + xlab("Concentration") +
      coord_cartesian(ylim=c(-0.1, 1.1)) + scale_y_continuous(breaks=seq(0, 1, 0.25)) +
      geom_point(size=2) + ylab('Prediction')

    aso_plot_list[[i]] = concDotPlot

  }

  pdf("ASOConcPlots.pdf")
  do.call(gridExtra::grid.arrange, c(aso_plot_list[1:6], nrow=3, ncol=2))
  do.call(gridExtra::grid.arrange, c(aso_plot_list[7:12], nrow=3, ncol=2))
  dev.off()

  #Exporting data table with all numeric values for the heatmap and dotplots
  pred_plot_df = cbind(pred_plot_mean, pred_plot_sd)

  openxlsx::write.xlsx(pred_plot_df,
                       file = paste0(format(Sys.time(),'RandomForestPredictions_%Y%m%d_%H%M'), '.xlsx'),
                       rowNames = TRUE)

  return(pred_heatmap)

}

