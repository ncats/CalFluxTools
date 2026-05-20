
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
setClass("FLIPRData", slots=list(
  plateSet = 'plateset',
  parameterInfo = "data.frame",
  methodParameters = "list",
  outputDirs = "vector",
  pcaResults = "list",
  mlResults = "list"
))


# Register caret's S3 model object for use in S4 slots.
setOldClass(c("train", "train.formula"))

#' @export
#' @import caret
setClass("MachineLearning", slots=list(
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


