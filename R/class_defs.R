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
  parameters = "vector",
  paramAbbrs = "vector",
  imputationFlag = "vector",
  imputationMethod = "character",
  qualityFlags = "list"
)
)


# corrPlateData = "data.frame",
# correctionProcesses = "vector",

#' @export
setClass("plateset", slots=list(
  setname = "character",
  plateIdList = "vector",
  plateReplicateSets = "vector",
  plates = "list",
  plateNames = "vector",
  transformedPlateData = "list",
  transformationNames = "vector",
  dataCoverageTables = "list"
)
)

#' @export
setClass("aso", slots=list(
  plateSet = 'plateset',
  parameterInfo = "data.frame",
  methodParameters = "list",
  outputDirs = "vector"
))


