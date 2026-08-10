##' Reads in FLIPR data and converts to plate class
##' @param filename (string) path to file to read in
##' @return plate class object
##' @author Andrew Patt
##' @export
read_FLIPR_data <- function(filename) {
  raw_data <- readr::read_file(filename, locale = readr::locale(encoding = "ISO-8859-1"))
  raw_data <- unlist(strsplit(raw_data, split = "\r\n|\n"))
  raw_data <- gsub("\r$", "", raw_data)
  raw_data <- sub("\t+$", "", raw_data)
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
  for(i in 1:length(statistic_dividers)){
    if(i < length(statistic_dividers)){
      temp_plate <- raw_data[statistic_dividers[i]:(statistic_dividers[i+1]-1)]
    } else {
      temp_plate <- raw_data[statistic_dividers[i]:length(raw_data)]
    }
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
  statistic = trimws(statistic)

  trim_edge_blanks <- function(x){
    while(length(x) > 0 && x[1] == ""){
      x <- x[-1]
    }
    while(length(x) > 0 && tail(x, 1) == ""){
      x <- x[-length(x)]
    }
    return(x)
  }

  # Some sparse statistic blocks lose their trailing tabs during preprocessing,
  # leaving rows like "A" or "B" with no tab-delimited values at all.
  data_row_idx <- grep("^[A-P](\t|$)", plate)
  if(length(data_row_idx) != 16){
    stop("Malformed statistic block for ", statistic,
         ": expected 16 plate rows, found ", length(data_row_idx))
  }

  header_idx <- data_row_idx[1] - 1
  if(header_idx < 1){
    stop("Malformed statistic block for ", statistic, ": missing column header row")
  }

  header_fields <- strsplit(plate[header_idx], split = "\t", fixed = TRUE)[[1]]
  header_fields <- trim_edge_blanks(header_fields)
  wellId_columns <- sapply(header_fields, function(x){
    if(nchar(x) < 2){
      return(paste0("0",x))
    } else {
      return(x)
    }
  })

  if(length(wellId_columns) != 24){
    stop("Malformed statistic block for ", statistic,
         ": expected 24 columns, found ", length(wellId_columns))
  }

  plate_rows <- lapply(plate[data_row_idx], function(line){
    fields <- strsplit(line, split = "\t", fixed = TRUE)[[1]]

    row_label <- fields[1]
    row_values <- fields[-1]
    expected_values <- length(wellId_columns)

    if(length(row_values) > expected_values){
      stop("Malformed statistic block for ", statistic,
           ": expected at most ", expected_values, " values in row ",
           row_label, ", found ", length(row_values))
    }

    if(length(row_values) < expected_values){
      row_values <- c(row_values, rep("", expected_values - length(row_values)))
    }

    row_values[row_values == ""] <- "Blank"
    c(row_label, row_values)
  })

  wellId_rows <- sapply(plate_rows,function(x) return(x[1]))

  wellIds <- c()
  for(i in wellId_rows){
    for(j in wellId_columns){
      wellIds <- c(wellIds,paste0(i,j))
    }
  }

  plate_rows <- lapply(plate_rows, function(x){
    out <- x[-1]
    return(out)
  })
  plate_rows <- unlist(plate_rows)

  if(length(plate_rows) != length(wellIds)){
    stop("Malformed statistic block for ", statistic,
         ": expected ", length(wellIds), " values, found ", length(plate_rows))
  }

  out_frame<-data.frame(ids = wellIds, data = plate_rows)
  return(list(statistic,out_frame))
}

##' reads platemap information
##' @param filename input plate map file name
##' @param plate an initialized plate object
##' @return returns a plate with data loaded.
##' @author Andrew Patt
read_FLIPR_metadata <- function(filename, plate){
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
