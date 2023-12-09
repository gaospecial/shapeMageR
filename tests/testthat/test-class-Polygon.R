test_that("Polygon methods works", {
  # generate sample data
  parameters <- list(c(0.35, 0.47, 0.35, 0.20, 135),
                     c(0.50, 0.57, 0.35, 0.15, 135),
                     c(0.50, 0.57, 0.33, 0.15,  45),
                     c(0.65, 0.47, 0.35, 0.20,  45))
  ellipses <- lapply(parameters,function(x){
    do.call(ellipse,as.list(c(x,100)))
  })
  polygons <- lapply(ellipses,function(x) sf::st_polygon(list(as.matrix(x))))
  polygon <- Polygon(sets = polygons)

  # test discern_overlap()
  discern_overlap1 <- discern_overlap(polygon) # the unique (overlapping) region of four elllipses
  discern_overlap2 <- Reduce(sf::st_intersection, polygon@sets)
  discern_overlap3 = overlap(polygon)
  expect_equal(discern_overlap1, discern_overlap2)
  expect_equal(discern_overlap1, discern_overlap3)

  # test discern
  discern1 = discern(polygon, 1:2, 3:4) # the region `slice1` has but `slice2` doest have.
  discern2 = sf::st_difference(sf::st_union(polygons[[1]], polygons[[2]]),sf::st_union(polygons[[3]], polygons[[4]]))
  expect_equal(discern1, discern2)

  # test overlap
  overlap1 = overlap(polygon)  # the overlaping region of four ellipses
  overlap2 = Reduce(sf::st_intersection, polygons)
  expect_equal(overlap1, overlap2)
})
