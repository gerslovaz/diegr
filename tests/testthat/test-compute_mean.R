test_that("compute_mean returns average, ci_low and ci_up", {
  result1 <- compute_mean(epochdata, amplitude = "signal", subject = 1,
                         channel = "E1", type = "jack")
  result2 <- compute_mean(epochdata, amplitude = "signal", subject = 1,
                          channel = "E1", type = "point")
  result3 <- compute_mean(epochdata, amplitude = "signal", subject = 1,
                          time = 10, type = "jack", group = "space")

  for (result in list(result1, result2, result3)) {
    expect_true(all(c("average", "ci_low", "ci_up") %in% names(result)))
  }
})


test_that("plot_time_mean returns ggplot with geom_ribbon", {
  skip_on_cran()
  data_mean <- compute_mean(epochdata, amplitude = "signal", subject = 2, channel = "E65",
    type = "jack")
  p1 <- plot_time_mean(data_mean, t0 = 10)

  expect_s3_class(p1, "ggplot")
  geoms <- sapply(p1$layers, function(layer) class(layer$geom)[1])

  expect_true("GeomRibbon" %in% geoms)
})

test_that("plot_topo_mean returns ggplot with geom_raster", {
  skip_on_cran()
  data_mean <- compute_mean(epochdata, amplitude = "signal", subject = 2, time = 10,
                            type = "jack", group = "space")
  p1 <- plot_topo_mean(data_mean)

  expect_s3_class(p1, "ggplot")
  geoms <- sapply(p1$layers, function(layer) class(layer$geom)[1])

  expect_true("GeomRaster" %in% geoms)
})

test_that("correct mean calculation per groups", {

  result <- compute_mean(epochdata, amplitude = "signal", subject = 1,
                         channel = "E1", time = 1:10, type = "point")
  result2 <- compute_mean(epochdata, amplitude = "signal",
                         channel = "E1", time = 1:10, type = "point", level = "subject")

  manual_mean_S1E1t1 <- epochdata |>
    dplyr::filter(subject == 1, sensor == "E1", time == 1) |>
    summarise(mean = mean(signal, na.rm = TRUE)) |>
    pull(mean)

  manual_mean_E1t1 <- epochdata |>
    dplyr::filter(sensor == "E1", time == 1) |>
    summarise(mean = mean(signal, na.rm = TRUE)) |>
    pull(mean)


  expect_equal(result |> dplyr::filter(subject == 1, sensor == "E1", time == 1) |> pull(average) |> head(1), manual_mean_S1E1t1)
  expect_equal(result2 |> pull(average) |> head(1), manual_mean_E1t1)
})
