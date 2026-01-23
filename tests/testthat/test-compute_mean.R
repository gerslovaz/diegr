test_that("compute_mean returns average, ci_low and ci_up", {
  edata <- epochdata |>
    pick_data(subject_rg = 1, sensor_rg = "E1")
  edata10 <- epochdata |>
    pick_data(subject_rg = 1, time_rg = 10)
  result1 <- compute_mean(edata, amplitude = "signal", type = "jack",
                          domain = "time")
  result2 <- compute_mean(edata, amplitude = "signal", type = "point",
                          domain = "time")
  result3 <- compute_mean(edata10, amplitude = "signal",
                          type = "jack", domain = "space")

  for (result in list(result1, result2, result3)) {
    expect_true(all(c("average", "ci_low", "ci_up") %in% names(result)))
  }
})


test_that("plot_time_mean returns ggplot with geom_ribbon", {
  skip_on_cran()
  data_mean <- epochdata |>
    pick_data(subject_rg = 2, sensor_rg = "E65") |>
    compute_mean(amplitude = "signal", type = "jack",
                 domain = "time")
  p1 <- plot_time_mean(data_mean, t0 = 10)

  expect_s3_class(p1, "ggplot")
  geoms <- sapply(p1$layers, function(layer) class(layer$geom)[1])

  expect_true("GeomRibbon" %in% geoms)
})

test_that("plot_topo_mean returns ggplot with geom_raster", {
  skip_on_cran()
  data_mean <- epochdata |>
    pick_data(subject_rg = 2, time_rg = 10) |>
    compute_mean(amplitude = "signal", type = "jack",
                 domain = "space")
  p1 <- plot_topo_mean(data_mean)

  expect_s3_class(p1, "ggplot")
  geoms <- sapply(p1$layers, function(layer) class(layer$geom)[1])

  expect_true("GeomRaster" %in% geoms)
})

test_that("correct mean calculation per groups", {

  result <- epochdata |>
    pick_data(subject_rg = 1, sensor_rg = "E1", time_rg = 1:10) |>
    compute_mean(amplitude = "signal", type = "point", domain = "time")
  result2 <- epochdata |>
    pick_data(sensor_rg = "E1", time_rg = 1:10) |>
    compute_mean(amplitude = "signal", type = "point",
                 level = "subject", domain = "time")

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
