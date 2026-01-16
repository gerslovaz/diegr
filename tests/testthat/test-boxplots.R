
test_that("check boxplot_rt structure", {

  p_test <- boxplot_rt(rtdata)

  expect_s3_class(p_test, "plotly")

})

test_that("check boxplot_epoch structure", {
  skip_on_cran()
  edata <- epochdata |>
    pick_data(subject_rg = 1, sensor_rg = "E34")
  p_test <- boxplot_epoch(edata, amplitude = "signal", time_lim = c(1:2))

  expect_s3_class(p_test, "plotly")

})

test_that("check boxplot_subject structure", {
  skip_on_cran()
  edata <- epochdata |>
    pick_data(epoch_rg = 1, sensor_rg = "E34")
  p_test <- boxplot_subject(epochdata, amplitude = "signal", time_lim = c(1:2))

  expect_s3_class(p_test, "plotly")

})

test_that("check interactive_waveforms structure", {
  skip_on_cran()
  subdata <- pick_data(epochdata, subject_rg = 1, sensor_rg = "E65")
  p_test <- interactive_waveforms(subdata, amplitude = "signal", t0 = 10, level = "epoch")
  expect_s3_class(p_test, "plotly")

})
