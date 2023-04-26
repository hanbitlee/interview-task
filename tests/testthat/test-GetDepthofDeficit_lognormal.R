# Smoke Test
test_that("GetDepthofDeficit_lognormal smoke test", {
  result <- GetDepthofDeficit_lognormal(nu = 1, sigma = 1, thres = 2, ret = "DoD")
  expect_true(!is.na(result))
})
