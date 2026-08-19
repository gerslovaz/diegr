test_that("scalp_plot without errors", {
  edata <- epochdata |>
    dplyr::filter(subject == 2 & time == 10 & epoch == 1)
  expect_silent({
    open3d()
    on.exit(close3d())
    scalp_plot(edata, amplitude = "signal")
  })
})

test_that("scalp_plot without display", {
  skip_on_cran()

  old_opts <- options(rgl.useNULL = TRUE)
  on.exit(options(old_opts), add = TRUE)

  edata <- epochdata |>
    dplyr::filter(subject == 2 & time == 10 & epoch == 1)

  expect_silent({
    open3d()
    scalp_plot(edata, amplitude = "signal")
    close3d()
  })
})
