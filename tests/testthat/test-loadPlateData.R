test_that("plate loading works", {


  # load ASO stub object
  setwd(system.file("extdata", package="aso"))
  asoFile = system.file("extdata", "sample_aso_only_params_loaded.Rdata", package="aso")
  load(asoFile)

  # load plates
  aso <- aso:::loadPlates(aso)
  testthat::expect_s4_class(aso@plateSet, "plateset")
  testthat::expect_equal(length(aso@plateSet@plates), 2, expected.label = "length of plates list")
  testthat::expect_equal(dim(aso@plateSet@plates[[1]]@plateData),c(384,63), expected.label = "plate 1 dimension")
  testthat::expect_equal(dim(aso@plateSet@plates[[2]]@plateData),c(384,63), expected.label = "plate 2 dimension")
  testthat::expect_equal(length(aso@plateSet@plates[[1]]@parameters), 62, expected.label = "parameter list size")
  testthat::expect_equal(aso@plateSet@plates[[1]]@parameters[4], "Mean Peak Rate (PpM)", expected.label = "parameter 4 name equality")
  testthat::expect_equal(aso@plateSet@plates[[1]]@plateData[8,2], "15.45", expected.label = "plate 1 [8,2] == 15.45")
})
