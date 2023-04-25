test_that("format_final_energy_downscaled_shape function", {
  input_df <- tibble(
    MODEL = rep("TestModel", 4),
    SCENARIO = rep("TestScenario", 4),
    ISO = rep("USA", 4),
    VARIABLE = c("Final Energy|Industry", "Final Energy|Agriculture", "Final Energy|Transportation", "Final Energy|Residential"),
    UNIT = rep("EJ", 4),
    `2020` = c(10, 5, 15, 20),
    `2030` = c(12, 6, 18, 24)
  )
  
  output_df <- format_final_energy_downscaled_shape(input_df)
  
  expected_df <- tibble(
    model = rep("TestModel", 4),
    scenario = rep("TestScenario", 4),
    iso = rep("USA", 4),
    variable = c("Final Energy|Industry", "Final Energy|Industry", "Final Energy|Transportation", "Final Energy|Residential"),
    unit = rep("EJ", 4),
    year = rep(c(2020, 2030), 2),
    value = c(15, 18, 15, 18, 20, 24)
  )
  
  expect_equal(output_df, expected_df)
})
