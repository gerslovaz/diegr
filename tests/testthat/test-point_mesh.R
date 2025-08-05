test_that("point_mesh output containes D2 and D3 with named columns", {
  M <- point_mesh(n = 1000, template = "HCGSN256")

  expect_true(all(c("D2", "D3") %in% names(M)))

  expect_s3_class(M$D2, "data.frame")
  expect_named(M$D2, c("x", "y"), ignore.order = FALSE)

  expect_s3_class(M$D3, "data.frame")
  expect_named(M$D3, c("x", "y", "z"), ignore.order = FALSE)
})

test_that("point_mesh dimension = 2 output containes D2 with named columns", {
  M <- point_mesh(dimension = 2, n = 1000, template = "HCGSN256")

  expect_true("D2" %in% names(M))

  expect_s3_class(M$D2, "data.frame")
  expect_named(M$D2, c("x", "y"), ignore.order = FALSE)
})

test_that("point_mesh dimension = 3 output containes D3 with named columns", {
  M <- point_mesh(dimension = 3, n = 1000, template = "HCGSN256")

  expect_true("D3" %in% names(M))

  expect_s3_class(M$D3, "data.frame")
  expect_named(M$D3, c("x", "y", "z"), ignore.order = FALSE)
})


test_that("make_triangulation output is 3-column matrix", {
  M <- point_mesh(dimension = 2, n = 1000, template = "HCGSN256")
  TRI <- make_triangulation(M$D2)
  expect_true(is.matrix(TRI))
  expect_equal(ncol(TRI), 3)
})
