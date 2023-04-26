library(tidyverse)
test_that("get_gini returns 0 when all values are the same", {
  # create sample data with all values the same
  df <- tibble(
    iso = rep(LETTERS[1:5], each = 5),
    population = rep(1, 25),
    fe = rep(1, 25)
  )
  
  # get gini result
  gini_result <- get_gini(df, var = "fe")
  
  # expected result
  expected_result <- tibble(
    iso = c("A", "B", "C", "D", "E"),
    gini = rep(0, 5)
  )
  
  # check if the results match
  expect_equal(gini_result, expected_result)
})
