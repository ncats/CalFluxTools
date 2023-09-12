test_that("filter selected parameters", {

  # load our base file

  asoFile = system.file("extdata", "sample_aso.Rdata", package="aso")
  load(asoFile)

  plateBefore <- aso@plateSet@plates[[1]]
  aso <- aso:::fiterToSelectedParameters(aso)
  plateAfter <- aso@plateSet@plates[[1]]
  sizeDiff <- ncol(plateBefore@plateData) - ncol(plateAfter@plateData)
  # verify a reduced parameter count
  testthat::expect_equal(sizeDiff, 46, expected.label = "reduced param count (46)")
  # verify dimensions of reduced plate data
  testthat::expect_equal(dim(plateAfter@plateData), c(384, 17), expected.label = "filtered data dim")
})
