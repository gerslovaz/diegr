test_that("output structure", {
  result <- outliers_epoch(epochdata, amplitude = "signal", subject = 2,
                           sensor = "E45", method = "iqr")

  expect_type(result, "list")
  expect_named(result, c("epoch_table", "outliers_data"), ignore.order = TRUE)
})

v1 <- c(rep(1,10), rep(2,10), 19)
testdata <- data.frame(signal = v1, subject = rep(1, 21), sensor = rep("E1", 21),
                       epoch = 1:21, time = rep(1,21))

test_that("computing outliers", {
  result <- outliers_epoch(testdata, amplitude = "signal", subject = 1,
                           sensor = "E1", method = "iqr")
  result2 <- outliers_epoch(testdata, amplitude = "signal", subject = 1,
                           sensor = "E1", method = "hampel")
  result3 <- outliers_epoch(testdata, amplitude = "signal", subject = 1,
                           sensor = "E1", method = "percentile")

  expect_equal(result$epoch_table$Count, 1)
  expect_equal(result2$epoch_table$Count, 1)
  expect_equal(result3$epoch_table$Count, 1)
})
