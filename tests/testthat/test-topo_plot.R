test_that("topo_plot returns ggplot with geom_raster and geom_contour for contour = TRUE", {
  data_mean <- compute_mean(epochdata, amplitude = "signal", subject = 2, time = 10,
                            type = "jack", group = "space")
  p1 <- topo_plot(data_mean, amplitude = "average")
  p2 <- topo_plot(data_mean, amplitude = "average", contour = TRUE)

  expect_s3_class(p1, "ggplot")
  geoms <- sapply(p1$layers, function(layer) class(layer$geom)[1])

  expect_true("GeomRaster" %in% geoms)

  expect_s3_class(p2, "ggplot")
  geoms2 <- sapply(p2$layers, function(layer) class(layer$geom)[1])

  expect_true("GeomContour" %in% geoms2)
})
