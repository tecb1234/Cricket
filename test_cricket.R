library(testthat)

source("process_cricsheet.R")

test_that("City to Country mapping", {
    
  expect_equal(as.character(geocode("Sydney", output = "more")$country), "Australia")
  
})

test_that("string manipulations", {
  
  expect_equal("file", get_filename("folder/file.yaml"))
})


