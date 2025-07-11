test_that("output create_scale", {
  result <- create_scale(col_range = c(-10,10))

  expect_true(all(c("colors", "breaks") %in% names(result)))
})

X <- HCGSN256$D2[,1:2]

test_that("output spline_matrix", {
  result <- spline_matrix(X)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(X))
})

test_that("output XP_IM", {
  result <- XP_IM(X)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(X) + 3)
})

test_that("output IM", {
  y <- epochdata |>
    filter(subject == 1, time == 1, epoch == 1) |>
    pull(signal)
  result <- IM(X, y)

  expect_true(is.list(result))
  expect_named(result, c("Y_hat", "beta_hat"), ignore.order = TRUE)
})


test_that("recompute_3d", {
  XX <- HCGSN256$D3[,1:3]
  M <- point_mesh(dim = 2)
  result <- recompute_3d(X, XX, M)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("x", "y", "z"), ignore.order = FALSE)
  expect_equal(nrow(result), nrow(M$D2))
})

test_that("pick_data behavior", {
  result <- pick_data(epochdata, sensor_rg = "E1", time_rg = 1:5)
  manual_pick <- epochdata |>
    filter(sensor == "E1", time %in% 1:5)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(manual_pick))
  expect_equal(result$signal[2], manual_pick$signal[2])
})


test_that("pick_region behavior", {
  result <- pick_region(coords = HCGSN256$D2, hemisphere = "midline",
                        ROI = HCGSN256$ROI)
  result2 <- pick_region(coords = HCGSN256$D2,
                         hemisphere = c("midline", "left", "right"),
                        ROI = HCGSN256$ROI)

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$x == 0))
  expect_equal(nrow(result2), nrow(HCGSN256$D2))
})
