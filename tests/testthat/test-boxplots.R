
test_that("check boxplot_rt structure", {

  p_test <- boxplot_rt(rtdata)

  expect_s3_class(p_test, "plotly")

})

test_that("check boxplot_epoch structure", {
  skip_on_cran()
  p_test <- boxplot_epoch(epochdata, amplitude = "signal", subject = 1,
                          channel = "E34", time_lim = c(1:2))

  expect_s3_class(p_test, "plotly")

})

test_that("check boxplot_subject structure", {
  skip_on_cran()
  p_test <- boxplot_subject(epochdata, amplitude = "signal", channel = "E34",
                            time_lim = c(1:2))

  expect_s3_class(p_test, "plotly")

})

test_that("check interactive_waveforms structure", {
  skip_on_cran()
  p_test <- interactive_waveforms(epochdata, amplitude = "signal", subject = 1,
                                  channel = "E65", t0 = 10, level = "epoch")
  expect_s3_class(p_test, "plotly")

})
