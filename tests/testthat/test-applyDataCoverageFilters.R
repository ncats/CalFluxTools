test_that("apply data coverage filters", {

  asoFile = system.file("extdata", "sample_aso.Rdata", package="aso")
  load(asoFile)
  aso <- aso:::fiterToSelectedParameters(aso)
  aso <- aso:::setParameterAbbreviaions(aso)
  aso <- aso:::applyCategoricalToNumeric(aso)

  # assess data completeness
  aso <- aso:::assessDataCompleteness(aso)
  dataCoverageTables <- aso@plateSet@dataCoverageTables
  testthat::expect_equal(length(dataCoverageTables), 2, expected.label = "number of data coverage tables (2)")
  completeDataCount1 <- sum(dataCoverageTables[[1]]$Good_Count == dataCoverageTables[[1]]$Keeper_Wells)
  completeDataCount2 <- sum(dataCoverageTables[[2]]$Good_Count == dataCoverageTables[[2]]$Keeper_Wells)
  testthat::expect_equal(completeDataCount1, 16, expected.label = "ref plate complete data param count")
  testthat::expect_equal(completeDataCount2, 4, expected.label = "expt plate complete data param count")

  # apply data coverage filter
  aso <-aso:::applyDataCoverageFilters(aso)
  plateRefCoverage <- aso@plateSet@dataCoverageTables[[1]]
  plateExptCoverage <- aso@plateSet@dataCoverageTables[[2]]
  p1KeepSum <- sum(plateRefCoverage$keep)
  p2KeepSum <- sum(plateExptCoverage$keep)
  testthat::expect_equal(c(p1KeepSum, p2KeepSum), c(16,16), expected.label = "keeper params = 16 and 16")
  testthat::expect_equal(dim(aso@plateSet@plates[[1]]@plateData), c(384,16), expected.label = "filtered ref plate data dim")
  testthat::expect_equal(dim(aso@plateSet@plates[[2]]@plateData), c(384,16), expected.label = "filtered expt plate data dim")
})
