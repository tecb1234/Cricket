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
  type3 <- c("0", "10", "batsman")
  
  expect_equal("batsman", clean_col_names(type1))
  expect_equal("runs-batsman", clean_col_names(type2))
  expect_equal("batsman", clean_col_names(type3))
})

test_that("wicket calculations", {
  single_no_wicket <- c(NA)
  single_wicket <- c("caught")
  
  expect_equal(1, get_wickets(single_no_wicket))
  expect_equal(1, get_wickets(single_wicket))
  
  test1 <- c(NA, NA, "caught")
  test2 <- c(NA, "caught", NA)
  test3 <- c("caught", NA, NA)
  test4 <- c(NA, "caught", "caught", NA)
  expect_equal(c(1,1,1), get_wickets(test1))
  expect_equal(c(1,1,2), get_wickets(test2))
  expect_equal(c(1,2,2), get_wickets(test3))
  expect_equal(c(1,1,2,3), get_wickets(test4))
})