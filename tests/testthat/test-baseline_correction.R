edata <- epochdata |>
  dplyr::filter(subject == 1 & sensor == "E65")

test_that("baseline_correction output has correct structure and columns", {

  result <- baseline_correction(edata, baseline_range = 1:10)

  expect_s3_class(result, "data.frame")

  expect_true("signal_base" %in% colnames(result))
  expect_true("baseline" %in% colnames(result))

  expect_false(is.grouped_df(result))
})

test_that("behavior for baseline_range not present in time column", {

  expect_warning(baseline_correction(edata, baseline_range = 100:105))
  result <- suppressWarnings(baseline_correction(edata, baseline_range = 100:105))
  expect_true(all(is.na(result$baseline)))
  expect_true(all(is.na(result$signal_base)))
})


test_that("correct baseline calculation per groups", {
  edata2 <- epochdata |>
    dplyr::filter(sensor %in% c("E65", "E1"))
  result <- baseline_correction(edata2, baseline_range = 1:5)

  manual_base_S1E1 <- edata2 |>
    dplyr::filter(subject == 1, sensor == "E1", epoch == 1, time %in% 1:5) |>
    summarise(mean = mean(signal, na.rm = TRUE)) |>
    pull(mean)

  manual_base_S2E65 <- edata2 |>
    dplyr::filter(subject == 2, sensor == "E65", epoch == 9, time %in% 1:5) |>
    summarise(mean = mean(signal, na.rm = TRUE)) |>
    pull(mean)

  expect_equal(result |> dplyr::filter(subject == 1, sensor == "E1", epoch == 1) |> pull(baseline) |> head(1), manual_base_S1E1)
  expect_equal(result |> dplyr::filter(subject == 2, sensor == "E65", epoch == 9) |> pull(baseline) |> head(1), manual_base_S2E65)
})
