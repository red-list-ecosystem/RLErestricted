test_that("AOO grid is a spatial object", {
  glaciers_on_volcanos <- tropical_glaciers |>
    filter(ecosystem_name %in% "Volcanos de Peru y Chile") |>
    st_transform(crs = 32719)
  AOO_test_grid <- create_AOO_grid(glaciers_on_volcanos)
  expect_s3_class(AOO_test_grid,"sf")
})
