test_that("load plate map", {

  # load ASO stub object
  setwd(system.file("extdata", package="aso"))
  asoFile = system.file("extdata", "sample_aso_only_params_loaded.Rdata", package="aso")
  load(asoFile)
  aso <- aso:::loadPlates(aso)

  # load platemap
  aso <- aso:::loadPlateMaps(aso)
  testthat::expect_equal(dim(aso@plateSet@plates[[1]]@plateAnnotMap), c(384, 6), expected.label = "dim plateAnnotMap")
  annotNames <- colnames(aso@plateSet@plates[[1]]@plateAnnotMap)
  testthat::expect_equal("Well" %in% annotNames, TRUE, expected.label = "have 'Well' IDs")
  testthat::expect_equal("Compound" %in% annotNames, TRUE, expected.label = "have 'Compound' IDs")
  testthat::expect_equal("Concentration" %in% annotNames, TRUE, expected.label = "have 'Concentration' info")
  testthat::expect_equal("WellType" %in% annotNames, TRUE, expected.label = "have 'WellType' info")
  testthat::expect_equal("Mask" %in% annotNames, TRUE, expected.label = "have 'Mask' info")
})
