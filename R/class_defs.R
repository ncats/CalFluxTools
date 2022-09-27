#S4 Plate Class

#' @export
setClass("plate", slots=list(
  plateId="character",
  statList="character",
  format="numeric",
  wellIds = "vector",
  plateData = "data.frame",
  plateDataMap = "list",
  plateAnnotMap = "data.frame",
  parameters = "vector"
)
)

#' @export
setClass("plateset", slots=list(
  setname = "character",
  plateIdList = "vector",
  plateReplicateSets = "vector",
  plates = "list"
)
)


