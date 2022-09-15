
#' Add a plate to a plateset
#'
#' @param plateset a plateset S4 bject
#' @param plate an plate S4 object to add to the plateset
#'
#'
#' @export
addPlate <- function(plateset, plate) {
  plateset@plates[[plate@plateId]] <- plate
  plateset@plateIdList <- c(plateset@plateIdList, plate@plateId)
  return(plateset)
}


