# Unit Test
test_that("GetDepthofDeficit_lognormal function", {
  expect_equal(
    GetDepthofDeficit_lognormal(nu = 1, sigma = 1, thres = 2, ret = "DoD"),
    0.6442716,
    tolerance = 1e-7
  )
  expect_equal(
    GetDepthofDeficit_lognormal(nu = 1, sigma = 1, thres = 2, ret = "share"),
    0.2659855,
    tolerance = 1e-7
  )
})

# Smoke Test
test_that("GetDepthofDeficit_lognormal smoke test", {
  result <- GetDepthofDeficit_lognormal(nu = 1, sigma = 1, thres = 2, ret = "DoD")
  expect_true(!is.na(result))
})
