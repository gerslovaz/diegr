
test_that("check boxplot_rt structure", {

  p_test <- boxplot_rt(rtdata)
  expect_s3_class(p_test, "plotly")

  p_test2 <- boxplot_rt(rtdata, interactivity = FALSE)
  expect_s3_class(p_test2, "ggplot")

})

test_that("check boxplot_epoch structure", {
  skip_on_cran()
  edata <- epochdata |>
    pick_data(subject_rg = 1, sensor_rg = "E34")
  p_test <- boxplot_epoch(edata, amplitude = "signal", time_lim = c(1:2))

  expect_s3_class(p_test, "plotly")

  p_test2 <- boxplot_epoch(edata, amplitude = "signal", time_lim = c(1:2),
                             interactivity = FALSE)
  expect_s3_class(p_test2, "ggplot")

})

test_that("check boxplot_subject structure", {
  skip_on_cran()
  edata <- epochdata |>
    pick_data(epoch_rg = 1, sensor_rg = "E34")
  p_test <- boxplot_subject(epochdata, amplitude = "signal", time_lim = c(1:2))

  expect_s3_class(p_test, "plotly")

  p_test2 <- boxplot_subject(epochdata, amplitude = "signal", time_lim = c(1:2),
                             interactivity = FALSE)
  expect_s3_class(p_test2, "ggplot")

})

test_that("check interactive_waveforms structure", {
  skip_on_cran()
  subdata <- pick_data(epochdata, subject_rg = 1, sensor_rg = "E65")
  p_test <- interactive_waveforms(subdata, amplitude = "signal", t0 = 10, level = "epoch")
  expect_s3_class(p_test, "plotly")

})

test_that("check interactive_surfaceplot structure", {
  skip_on_cran()
  subdata <- pick_data(epochdata, subject_rg = 1, epoch_rg = 1:13, time_rg = 10)
  data_mean <- compute_mean(subdata, amplitude = "signal", type = "point", domain = "space")
  p_test <- interactive_surfaceplot(data_mean, amplitude = "average", col_range = c(-10, 10))
  expect_s3_class(p_test, "plotly")

})

test_that("check interactive_surfaceplot_curves structure", {
  skip_on_cran()
  subdata <- pick_data(epochdata, subject_rg = 1, epoch_rg = 1:13)
  data_mean <- compute_mean(subdata, amplitude = "signal", type = "point", domain = "time")
  p_test <- interactive_surfaceplot_curves(data_mean, amplitude = "average")
  expect_s3_class(p_test, "plotly")

})
