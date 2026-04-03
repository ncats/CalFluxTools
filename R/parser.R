##' Reads in data from stemonix platform and converts to plate class
##' @param filename (string) path to file to read in
##' @return plate class object
##' @author Andrew Patt
##' @export
read_stemonix_data <- function(filename) {
  raw_data <- readr::read_file(filename, locale = readr::locale(encoding = "ISO-8859-1"))
  raw_data <- strsplit(raw_data,split='\t')[[1]]
  raw_data <- sapply(raw_data,function(x) {
    if(identical(x,"")) {
      return("Blank")
    } else {
      return(x)
    }
  })
  raw_data <- unlist(sapply(raw_data,strsplit,split="\r\n"))
  names(raw_data) <- NULL

  # Using 'Statistic =' to demarcate our different sections
  statistic_dividers <- which(grepl("Statistic =",raw_data))

  # Extract the header information which is structured differently from the plate data
  header <- raw_data[1:(statistic_dividers[1]-1)]

  # Extract plate name from file name in header
  plate_id = header[1]
  plate_id = unlist(strsplit(plate_id,"\\",fixed=TRUE))
  plate_id = plate_id[length(plate_id)]
  plate_id = gsub(".fmd","",plate_id,fixed=TRUE)

  # Loop through the remaining plates
  parsed_plate_list <- list()
  for(i in 1:(length(statistic_dividers)-1)){
    temp_plate <- raw_data[statistic_dividers[i]:(statistic_dividers[i+1]-1)]
    temp_plate_parsed <- FLIPRTools:::parse_individual_plate(temp_plate)
    parsed_plate_list <- rlist::list.append(parsed_plate_list,temp_plate_parsed)
  }

  stat_list <- sapply(parsed_plate_list, function(x){
    return(x[1])
  })
  stat_list <- unlist(stat_list)
  well_id = parsed_plate_list[[1]][[2]][,1]
  plate_data_raw <- data.frame(well_id = well_id)
  for(i in parsed_plate_list){
    plate_data_raw <- cbind(plate_data_raw,i[[2]][,2])
  }

  colnames(plate_data_raw) <- c("well_id",stat_list)

  out <- new("plate", statList = stat_list, parameters = stat_list, wellIds = well_id,
             plateData = plate_data_raw, plateId = plate_id)
  return(out)
}

##' Internal function for parsing vector corresponding to a single plate
##' @param plate the plate statistic list
##' @return parsed plate data
##' @author Andrew Patt
parse_individual_plate <- function(plate){
  statistic = plate[1]
  statistic = gsub("Statistic = ","",statistic)

  plate_rows <- list()
  temp_row <- c()

  # Using blank entries to demarcate row beginnings
  for(i in plate){
    if(i!=""){
      temp_row <-c(temp_row,i)
    }else{
      plate_rows <- rlist::list.append(plate_rows,temp_row)
      temp_row <- c()
    }
  }

  # Build wellId vector. Assuming statistic, start sample and end sample
  # are the only metadata included on the individual plate level
  wellId_columns <- plate_rows[[1]][6:length(plate_rows[[1]])]
  wellId_columns <- sapply(wellId_columns,
                            function(x){ifelse(nchar(x)<2,
                                               return(paste0("0",x)),
                                               return(x))})

  wellId_rows <- sapply(plate_rows[2:length(plate_rows)],function(x) return(x[1]))

  wellIds <- c()
  for(i in wellId_rows){
    for(j in wellId_columns){
      wellIds <- c(wellIds,paste0(i,j))
    }
  }

  plate_rows <- lapply(plate_rows[2:length(plate_rows)], function(x){
    out <- x[-1]
    return(out)
  })
  plate_rows <- unlist(plate_rows)
  out_frame<-data.frame(ids = wellIds, data = plate_rows)
  return(list(statistic,out_frame))
}

##' reads platemap information
##' @param filename input plate map file name
##' @param plate an initialized plate object
##' @return returns a plate with data loaded.
##' @author Andrew Patt
read_stemonix_metadata <- function(filename, plate){
  raw_data <- read.csv(filename)
  raw_data$Well <- sapply(raw_data$Well, function(x){
    rownum <- readr::parse_number(x)
    col <- sub("^([[:alpha:]]*).*", "\\1", x)
    if(rownum < 10){
      return(paste0(col,"0",rownum))
    }else{
      return(x)
    }
  })
  plate@plateAnnotMap <- raw_data
  return(plate)
}

#' Reads an input FLIPRData parameter Excel file and returns a FLIPRData object.
#' @param filename excel FLIPRData parmeter file
#' @return returns a FLIPRData object ready for analysis. The FLIPRData file will not have plate data but rather just instructions on data
#' analysis and input data files. This is typically the first function to run to generate an initialized FLIPRData object.
#' @export
readFLIPRDataParameterFile <- function(filename) {
  paramsDf <- openxlsx::read.xlsx(filename, sheet="Parameter_Info")
  analysisDf <- openxlsx::read.xlsx(filename, sheet="Analysis_Settings", colNames = F)
  FLIPRData <- parseAnalysisSettings(analysisDf)
  FLIPRData@parameterInfo = paramsDf

  return(FLIPRData)
}


parseAnalysisSettings <- function(analysisDf) {
  analysisName = ""
  rootDir = ""
  #create output directory

  plateFormat = 384
  plateFiles <- list()
  plateMaps <- list()
  analysisParams <- list()

  FLIPRData = new("FLIPRData")
  for(i in 1:nrow(analysisDf)) {
    if(analysisDf[i,1] == "Root_Directory") {
      rootDir = trimws(analysisDf[i,2])

      # set proper path delimiter
      rootDir <- gsub("\\\\", "/", rootDir)

      # make user we have a path delim at the end
      rootDir <- paste0(rootDir, "/")

    } else if(analysisDf[i,1] == "Plate_Label") {
      for(j in (i+1):nrow(analysisDf)) {
        if(analysisDf[j,1] == "Processing") {
          i = j-1
          break
        } else {
          plateFiles[[trimws(analysisDf[j,1])]] <- trimws(analysisDf[j,2])
          plateMaps[[trimws(analysisDf[j,1])]] <- trimws(analysisDf[j,3])
          FLIPRData@outputDirs <- c(FLIPRData@outputDirs, trimws(analysisDf[j,4]))
        }
      }
    } else if(analysisDf[i,1] == "Processing") {
      analysisParams <- parseProcessingParams(analysisDf[(i+1):nrow(analysisDf),1:2])
    } else if(analysisDf[i,1] == "Plate_Format") {
      plateFormat <- as.numeric(trimws(analysisDf[i,2]))
    } else if(analysisDf[i,1] == "Analysis_Name") {
      analysisName = trimws(analysisDf[i,2])
    }
  }

  allParamsList <- list()
  allParamsList[['analysis_name']] <- analysisName
  allParamsList[['root_dir']] <- rootDir
  #Add an output directory in this list as well, can use that output directory in any file creation
  allParamsList[['plate_format']] <- plateFormat
  allParamsList[['plate_file_list']] <- plateFiles
  allParamsList[['plate_map_list']] <- plateMaps
  allParamsList[['analysis_params']] <- analysisParams
  FLIPRData@methodParameters = allParamsList
  return(FLIPRData)
}

parseProcessingParams <- function(processingDf) {
  params <- list()
  for(i in 1:nrow(processingDf)) {
    key <- trimws(processingDf[i,1])
    val <- trimws(processingDf[i,2])
    if(key != "" && val != "") {
      params[key] <- val
    }
  }
  return(params)
}


