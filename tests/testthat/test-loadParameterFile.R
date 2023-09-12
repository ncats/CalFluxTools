
test_that("loading parameter file works", {
  # set working dir and and load the package example param file
  setwd(system.file("extdata", package="aso"))
  paramFile = system.file("extdata", "FLIPR_Analysis_parameters.xlsx", package="aso")
  aso <- aso::readAsoParameterFile(paramFile)

  # verify contents
  testthat::expect_equal(dim(aso@parameterInfo), c(63,11), expected.label = "aso@parameterInfo")
  testthat::expect_equal(length(aso@methodParameters), 6)
  parameterInfoFields <- colnames(aso@parameterInfo)
  testthat::expect_equal("Statistic" %in% parameterInfoFields, TRUE, expected.label = "have 'Statistic'")
  testthat::expect_equal("Suggested_Abbreviation" %in% parameterInfoFields, TRUE, expected.label = "have 'Suggested_Abbreviation'")
  testthat::expect_equal("Use" %in% parameterInfoFields, TRUE, expected.label = "have 'Use' flag info")
})
