

analyze <- function() {
  file <- file.choose()
  aso:::runAnaylsis(file)
}



runAnaylsis <- function(asoParameterXlsxFilePath) {

  # parse parameter and analysis settings
  aso <- aso:::read_aso_parameter_file(asoParameterXlsxFilePath)

  # load the list of plate files and create file pairs
  # creating plate set
  aso <- aso:::loadPlates(aso)

  # auc parameters tend to have Japanese Kanji in the plate data file, and strange unicode in the excel file.
  aso <- aso:::aucParameterPatch(aso)

  # allParamsList['plate_map_list'] <- plateMaps
  aso <- aso:::loadPlateMaps(aso)

  # filter to only specified parameters
  aso <- aso:::fiterToSelectedParameters(aso)

  # apply parameter abbreviations
  aso <- aso:::setParameterAbbreviaions(aso)

  # apply value imputations and make categorical results numeric - if specified
  aso <- aso:::applyCategoricalToNumeric(aso)

  print("HEYYYYY before assessDataCompleteness")

  # assess data coverage and create optional output file for each plate.
  aso <- aso:::assessDataCompleteness(aso)

  # filter parameters for low data coverage
  aso <- aso:::applyDataCoverageFilters(aso)

  print("data dim after filters...")
  print(dim(aso@plateSet@plates[[1]]@plateData))

  # port of the data coverage tables
  aso:::exportDataCoverageReport(aso)

  # harmonize parameters - make sure all plates in pairs have the same parameters (data cols)
  aso <- aso:::harmonizeParameters(aso)

  print("data dim after Harmonize...")
  print(dim(aso@plateSet@plates[[1]]@plateData))


  print("before transform")

  # run transformations on plate set plate-pairs
  aso <- aso:::transformData(aso)

  print("after transform")

  print("before export plate views")
  # create optional plate view pdf.
  aso:::exportPlateViews(aso)

  # create optional bar-chart
  aso:::exportBarCharts(aso)

  ttDf <- aso:::runPairedTTest(aso)

  aso:::exportPairedTTestResult(aso, ttDf, "file")

  print("Analyis Done")

  aso:::exportTransformedData(aso)

  return(aso)
}


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
  #print("whats going on?")
  return(aso)
}


loadPlateMaps <- function(aso) {
  fileList <- aso@methodParameters[['plate_map_list']]

  dir <- aso@methodParameters[['root_dir']]

  for(platename in names(fileList)) {

    file <- fileList[[platename]]

    plate <- aso@plateSet@plates[[platename]]

    if(is.null(plate)) {
      #print("NULL PLATE!!!!!!!!!!!!!!!!!!!!!!!!")
    } else {
      #print("PLATE NOT NULL :)")
    }

    plate <- aso:::read_stemonix_metadata(paste0(dir,file), plate)
    aso@plateSet@plates[[platename]] <- plate
  }

  return(aso)
}

setParameterAbbreviaions <- function(aso) {

  print("setting parameter abbr")
  abbrInfo <- aso@parameterInfo[,c(1:2)]

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

applyCategoricalToNumeric <- function(aso) {

  print("categorical to numeric parameters")

  # get parameters to convert
  paramsToMap <- aso@parameterInfo[!is.na(aso@parameterInfo$cat_to_num_map),]

  print(dim(paramsToMap))
  print("params to map..")
  print(paramsToMap)

  for(platename in names(aso@plateSet@plates)) {

    print("Plate Name To Process")
    print(platename)

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
    missingValReport <- merge(pInfo, missingValReport, by.x='Suggested_Abbreviation', by.y='parameter', all.x=F, all.y=T, sort=F)

    missingValReport <- missingValReport[,c(2,1,3:(ncol(missingValReport)))]

    colnames(missingValReport)[1] <- 'param_name'
    colnames(missingValReport)[2] <- 'parameter'

    #missingValReport <- subset(missingValReport, select=-c(Statistic))

    dataCoverage[[platename]] <- missingValReport
  }

  aso@plateSet@dataCoverageTables <- dataCoverage

  return(aso)
}


applyDataCoverageFilters <- function(aso) {
  # for each plate loop over paramters to check their coverage and apply limits

  # get lower coverage limit for each parameter
  pInfo <- aso@parameterInfo[aso@parameterInfo$Use == 1, c('Statistic', 'Suggested_Abbreviation', 'Min_Data_Coverage_PCT')]
  pSize <- aso@methodParameters[['plate_format']]
  pInfo$max_data_loss <- pSize - ceiling((pInfo$Min_Data_Coverage_PCT/100.0) * pSize)

  # pInfo$max_data_loss <- 10

  #pInfo <- pInfo[,c(2,ncol(pInfo))]

  plateSet <- aso@plateSet
  dataCoverage <- plateSet@dataCoverageTables

  for(n in names(dataCoverage)) {
    #print(n)
    dcov <- dataCoverage[[n]]
    #print(colnames(dcov))
    dcov <- merge(dcov, pInfo, by.x = 'parameter', by.y = 'Suggested_Abbreviation', sort=F)
    #print(colnames(dcov))
    dcov$keep <- T
    dcov$keep[dcov$missing_count > dcov$max_data_loss] <- F
    #print(colnames(dcov))

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
    plate@parameters <- dcov$param_name[dcov$keep == "TRUE"]
    plates[[platename]] <- plate
  }

  aso@plateSet@plates <- plates

  return(aso)
}


exportDataCoverageReport <- function(aso) {

  rootDir <- aso@methodParameters[['root_dir']]
  fileName = format(Sys.time(), "Plate_Data_Coverage_Status_Reports_%Y%m%d_%H%M")
  tabs <- aso@plateSet@dataCoverageTables

  openxlsx::write.xlsx(tabs, file = paste0(rootDir,fileName,".xlsx"))

}

harmonizeParameters <- function(aso) {
  aso@plateSet <- harmonizeParametersAcrossPlates(aso@plateSet)
  return(aso)
}


transformData <- function(aso) {

  i = 1
  plateNames = c()

  for(plateName in names(aso@plateSet@plates)) {
    plate <- aso@plateSet@plates[[plateName]]
    plateNames <- c(plateNames, plateName)

    if(i %% 2 == 0) {

      print(plateNames)
      aso@plateSet <- dataTransform(aso@plateSet, platePair=plateNames, method = 'log2ratio', firstDataCol = 1)

      plateNames = c()
    }
    i = i + 1


  }

  return(aso)
}


exportPlateViews <- function(aso) {

  plateSet <- aso@plateSet

  i = 1
  plateNames = c()

  ptoPlot = c()

  for(plateName in names(aso@plateSet@plates)) {
    plate <- aso@plateSet@plates[[plateName]]
    plateNames <- c(plateNames, plateName)

    if(i %% 2 == 0) {

      print(plateNames)

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


statAnalyis <- function(aso, analysis="paired-t-test") {
  if(analysis == 'paired-t-test') {
    aso = runPairedTTest(aso)
  }
  return(aso)
}


runPairedTTest <- function(aso) {
  plateset <- aso@plateSet

  plateNames <- names(plateset@plates)

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

            lfc <- log(mean(data[,2],na.rm=T)/mean(data[,1], na.rm=T),2)

            if(nrow(data) > 2) {
              resultName <- paste0(compound,"_",conc,"_",param)

              # if(resultName == 'ASO1_30_PkRate') {
              #   print(resultName)
              #   print(data)
              #   print(lfc)
              # }

              # all 0 is ok by paired ttest.
              # but 0 variance is not tolerated well
              if(sum(data) == 0 || !(sd(data[,2]-data[,1]) == 0)) {
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



