

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

  # assess data coverage and create optional output file for each plate.
  aso <- aso:::assessDataCompleteness(aso)

  # filter parameters for low data coverage
  aso <- aso:::applyDataCoverageFilters(aso)

    # export of the data coverage tables
  # aso:::exportDataCoverageReport(aso)

  # harmonize parameters - make sure all plates in pairs have the same parameters (data cols)

  # run transformations on plate set plate-pairs

  # create optional plate view pdf.

  # create optional bar-chart

  print("Analyis Done")

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

  #writeLines("\n\n\nLoading Plate Maps")
  #print(dir)
  #writeLines("/n/n/n")


  for(platename in names(fileList)) {

    file <- fileList[[platename]]

    #print(paste0("platename :",platename))
    #print(paste0("map file :",file))

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

  for(platename in names(aso@plateSet@plates)) {
    plate <- aso@plateSet@plates[[platename]]

    for(i in 0:nrow(paramsToMap)) {

      abbrParam <- paramsToMap[i,2]

      keys <- paramsToMap[i,'cat_to_num_map']

      #print(keys)

      keys <- unlist(strsplit(keys,split=","))
      for(j in 0:length(keys)) {
        keys[j] <- trimws(keys[j])
      }

      vals <- paramsToMap[i,'cat_to_num_map2']

      #print(vals)

      vals <- unlist(strsplit(vals, split=","))
      for(k in 0:length(vals)) {
        vals[j] <- trimws(vals[j])
      }
      vals <- as.numeric(vals)

      #print(keys)
      #print(vals)
      # now take care of this param on this plate
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

  for(platename in names(plates)) {
    plate <- plates[[platename]]
    missingValReport <- missingDataReport(plate=plate, plateSize=plate@format)

    # lets append the parameter names
    missingValReport <- merge(pInfo, missingValReport, by.x='Suggested_Abbreviation', by.y='parameter', all.x=F, all.y=T, sort=F)

    missingValReport <- missingValReport[,c(2,1,3:(ncol(missingValReport)))]

    colnames(missingValReport)[1] <- 'param_name'
    colnames(missingValReport)[2] <- 'parameter'

    dataCoverage[[platename]] <- missingValReport

  }

  aso@plateSet@dataCoverageTables <- dataCoverage

  return(aso)
}


exportDataCoverageReport <- function(aso) {
  return(aso)
}

applyDataCoverageFilters <- function(aso) {
  # for each plate loop over paramters to check their coverage and apply limits

  # get lower coverage limit for each parameter
  pInfo <- aso@parameterInfo[aso@parameterInfo$Use == 1, c('Statistic', 'Suggested_Abbreviation', 'Min_Data_Coverage_PCT')]
  pSize <- aso@methodParameters[['plate_format']]
  pInfo$max_data_loss <- pSize - ceiling((pInfo$Min_Data_Coverage_PCT/100.0) * pSize)

  pInfo$max_data_loss <- 10

  pInfo <- pInfo[,c(2,ncol(pInfo))]

  plateSet <- aso@plateSet
  dataCoverage <- plateSet@dataCoverageTables

  for(n in names(dataCoverage)) {
    print(n)
    dcov <- dataCoverage[[n]]
    print(colnames(dcov))
    dcov <- merge(dcov, pInfo, by.x = 'parameter', by.y = 'Suggested_Abbreviation', sort=F)
    print(colnames(dcov))
    dcov$keep <- T
    dcov$keep[dcov$missing_count > dcov$max_data_loss] <- F
    print(colnames(dcov))
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
    plate@parameters <- dcov$param_name[dcov$keep]
    plates[[platename]] <- plate
  }

  aso@plateSet@plates <- plates

  return(aso)
}

