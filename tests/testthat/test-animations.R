sendata <- epochdata |> filter(subject == 1, time %in% 8:10, epoch %in% 11:12)
M <- point_mesh(n = 1000, template = "HCGSN256")
meandata <- compute_mean(sendata, amplitude = "signal", group = "space", type = "jack")
animdata <- sendata |> filter(epoch == 11)

test_that("prepare_anim_structure otuput", {

  Prep1 <- prepare_anim_structure(animdata, amp_name = "signal", coords = HCGSN256$D2,
                         mesh_mat = M$D2)
  Prep2 <- prepare_anim_structure(animdata, amp_name = "signal", coords = HCGSN256$D3,
                                  mesh_mat = M$D3)

  for (result in list(Prep1, Prep2)) {
    expect_true(all(c("amplitude_IM", "time", "mesh_coord") %in% names(result)))
  }

  expect_equal(ncol(Prep1$mesh_coord), 2)
  expect_equal(ncol(Prep2$mesh_coord), 3)
})

test_that("prepare_anim_structure_CI otuput", {

  Prep1 <- prepare_anim_structure_CI(meandata, coords = HCGSN256$D2,
                                  mesh_mat = M$D2)
  Prep2 <- prepare_anim_structure_CI(meandata, coords = HCGSN256$D3,
                                  mesh_mat = M$D3)

  for (result in list(Prep1, Prep2)) {
    expect_true(all(c("stats", "stats_value", "time", "mesh_coord") %in% names(result)))
  }

  expect_equal(ncol(Prep1$mesh_coord), 2)
  expect_equal(ncol(Prep2$mesh_coord), 3)
})
