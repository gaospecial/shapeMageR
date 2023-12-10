test_that("VennShape methods work", {
  set_Edge = fancy_2d_circle()
  v = VennShape(
    shapeId = "1",
    type = "circle",
    setEdge = set_Edge,
    setLabel = fancy_2d_circle_label()
  )
  expect_s4_class(v, "VennShape")
  expect_equal(v@nsets, 2L)

  # test intersection
  overlap1 = sf::st_intersection(sf::st_polygon(list(set_Edge[[1]])), sf::st_polygon(list(set_Edge[[2]])))
  overlap2 = v@regionEdge$geometry[[3]]
  expect_equal(overlap1, overlap2)

  # test centrorid
  center1 = sf::st_centroid(overlap1)
  center2 = v@regionLabel$geometry[[3]]
  expect_equal(center1, center2)
})
