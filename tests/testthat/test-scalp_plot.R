test_that("scalp_plot without errors", {
  edata <- epochdata |>
    dplyr::filter(subject == 2 & time == 10 & epoch == 1)
  expect_silent({
    open3d()
    on.exit(close3d())
    scalp_plot(edata, amplitude = "signal")
  })
})
