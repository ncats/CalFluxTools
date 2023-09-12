test_that("Transform Data", {
  asoFile = system.file("extdata", "sample_aso_coverage_fitered.Rdata", package="aso")
  load(asoFile)
  aso <- aso:::replaceBlanksAndNAValues(aso)
  aso <- aso:::harmonizeParameters(aso)

  aso <- aso::transformData(aso)
  testthat::expect_equal(length(aso@plateSet@transformedPlateData), 1, expected.label = "Transformed data count is 1")
  testthat::expect_equal(dim(aso@plateSet@transformedPlateData[[1]]), c(384, 16), expected.label = "Transformed data dim 384 x 16")
  testthat::expect_equal(aso@plateSet@transformedPlateData[[1]][50,3], -2.0, tolerance=0.05, expected.label = "Transformed data value [50,3], -2.046055")
})
