
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
  outputDirs = "vector",
  pcaResults = "list",
  rfResults = "list"
))


#' @export
#' @import caret
setClass("randomForest", slots=list(
  model = "train",
  predictions = "data.frame",
  prediction_means = "data.frame",
  prediction_sds = "data.frame"
))

#' @export
setClass("pcaResult", slots=list(
  controlWellsPCA = "list",
  allWellsPCA = "list"
))



