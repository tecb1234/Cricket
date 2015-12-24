library(testthat)

#source("process_cricsheet.R")

test_that("City to Country mapping", {
    
  expect_equal(as.character(geocode("Sydney", output = "more")$country), "Australia")
  
})

test_that("string manipulations", {
  
  expect_equal("file", get_filename("folder/file.yaml"))
})

test_that("home, away and neutral manipulations",{
  expect_equal(c("home", "away"), check_neutral(c("home", "away")))
  expect_equal(c("away", "home"), check_neutral(c("away", "home")))
  expect_equal(c("neutral","neutral"), check_neutral(c("away", "away")))
})


test_that("clean col names", {
  type1 <- c("0", "1", "batsman")
  type2 <- c("0", "1", "runs", "batsman")
  
  expect_equal("batsman", clean_col_names(type1))
  expect_equal("runs-batsman", clean_col_names(type2))
})