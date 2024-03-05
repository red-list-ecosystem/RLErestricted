test_that("AOO grid is a spatial object", {
  expect_s3_class(create_AOO_grid(glaciers_on_volcanos),"sf")
})
