test_that("run reference plate QC", {

  currDir <- getwd()
  asoFile = system.file("extdata", "sample_aso_coverage_fitered.Rdata", package="aso")
  load(asoFile)
  aso <- aso:::replaceBlanksAndNAValues(aso)
  aso <- aso:::harmonizeParameters(aso)

  # create a temp dir
  setwd(system.file("extdata", package="aso"))
  path = paste0(getwd(), "/tmp")
  dir.create(path="./tmp", mode ="0777")
  setwd(path)

  # run reference QC to create QC file
  tempQCFile <- aso:::runQcForReferencePlate(aso)

  testthat::expect_equal(file.exists(tempQCFile), TRUE, expected.label = "reference QC file exists")
  qcDF <- openxlsx::read.xlsx(tempQCFile, sheet=2)
  testthat::expect_equal(dim(qcDF), c(384,28), expected.label = "Unpivoted QC dims (384, 28)")

  setwd(currDir)
  unlink(path, recursive = T)
})


