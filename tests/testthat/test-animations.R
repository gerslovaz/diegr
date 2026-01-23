sendata <- epochdata |> filter(subject == 1, time %in% 8:10, epoch %in% 11:12)
sensors <- unique(sendata$sensor)
M <- point_mesh(n = 1000, template = "HCGSN256", sensor_select = sensors)
meandata <- compute_mean(sendata, amplitude = "signal", domain = "space", type = "jack")
animdata <- sendata |> filter(epoch == 11)

coords_full <- HCGSN256$D2
sensor_index <- which(coords_full$sensor %in% sensors)
coords2d <- coords_full[sensor_index,]
coords3d <- HCGSN256$D3[sensor_index,]

test_that("prepare_anim_structure otuput", {

  Prep1 <- prepare_anim_structure(animdata, amp_name = "signal", coords = coords2d,
                         mesh_mat = M$D2)
  Prep2 <- prepare_anim_structure(animdata, amp_name = "signal", coords = coords3d,
                                  mesh_mat = M$D3)

  for (result in list(Prep1, Prep2)) {
    expect_true(all(c("amplitude_IM", "time", "mesh_coord") %in% names(result)))
  }

  expect_equal(ncol(Prep1$mesh_coord), 2)
  expect_equal(ncol(Prep2$mesh_coord), 3)
})

test_that("prepare_anim_structure_CI otuput", {

  Prep1 <- prepare_anim_structure_CI(meandata, coords = coords2d,
                                  mesh_mat = M$D2)
  Prep2 <- prepare_anim_structure_CI(meandata, coords = coords3d,
                                  mesh_mat = M$D3)

  for (result in list(Prep1, Prep2)) {
    expect_true(all(c("stats", "stats_value", "time", "mesh_coord") %in% names(result)))
  }

  expect_equal(ncol(Prep1$mesh_coord), 2)
  expect_equal(ncol(Prep2$mesh_coord), 3)
})
