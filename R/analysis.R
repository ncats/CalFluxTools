

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

  # filter parameters for low data coverage

  # harmonize parameters - make sure all plates in pairs have the same parameters (data cols)

  # run transformations on plate set plate-pairs

  # create optional plate view pdf.

  # create optional bar-chart

  print("Analyis Done")

}


loadPlates <- function(aso) {
  fileList <- aso@methodParameters[['plate_file_list']]
  plateSet = new("plateset")
  plates <- list()

  dir <- aso@methodParameters[['root_dir']]

  for(platename in names(fileList)) {
    print(paste0("adding new plate with name: ",platename))
    file <- fileList[[platename]]
    plate <- aso::read_stemonix_data(filename=paste0(dir,file))
    plateSet <- aso::addPlate(plateSet, plate, platename)
    print(paste0("added plate from: ",dir,file))
  }

  # add the plateset
  aso@plateSet <- plateSet
  #print("whats going on?")
  return(aso)
}


loadPlateMaps <- function(aso) {
  fileList <- aso@methodParameters[['plate_map_list']]

  dir <- aso@methodParameters[['root_dir']]

  writeLines("\n\n\nLoading Plate Maps")
  print(dir)
  writeLines("/n/n/n")


  for(platename in names(fileList)) {

    file <- fileList[[platename]]

    print(paste0("platename :",platename))
    print(paste0("map file :",file))

    plate <- aso@plateSet@plates[[platename]]

    if(is.null(plate)) {
      print("NULL PLATE!!!!!!!!!!!!!!!!!!!!!!!!")
    } else {
      print("PLATE NOT NULL :)")
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

      print(keys)

      keys <- unlist(strsplit(keys,split=","))
      for(j in 0:length(keys)) {
        keys[j] <- trimws(keys[j])
      }

      vals <- paramsToMap[i,'cat_to_num_map2']

      print(vals)

      vals <- unlist(strsplit(vals, split=","))
      for(k in 0:length(vals)) {
        vals[j] <- trimws(vals[j])
      }
      vals <- as.numeric(vals)

      print(keys)
      print(vals)
      # now take care of this param on this plate
      plate <- aso::encodeDiscreteParameters(plate, abbrParam, keys, vals)
      aso@plateSet@plates[[platename]] <- plate
    }

  }

  return(aso)

}
