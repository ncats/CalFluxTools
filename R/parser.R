##' Reads in data from stemonix platform and converts to plate class 
##' @param filename (string) path to file to read in
##' @return plate class object
##' @author Andrew Patt
##' @export
read_stemonix_data <- function(filename){
  raw_data <- readr::read_file(filename)
  raw_data <- strsplit(raw_data,split="\t")[[1]]
  raw_data <- sapply(raw_data,function(x){
    if(identical(x,"")){
      return("Blank")
    } else{
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
    temp_plate_parsed <- parse_individual_plate(temp_plate)
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

  out <- new("plate", statList = stat_list, wellIds = well_id,
             plateData = plate_data_raw, plateId = plate_id)
  return(out)
}

##' Internal function for parsing vector corresponding to a single plate
##' @param plate 
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


##' @param filename 
##' @param plate 
##' @return 
##' @author Andrew Patt
read_stemonix_metadata <- function(filename,plate){
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
