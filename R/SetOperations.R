################## Method for polygon intersection  ############

#' calculate the overlapping region of `Polygon` object
#'
#' @name overlap
#'
#' @param polygon a Polygon object
#' @param slice index of Venn members, default is "all"
#'
#' @return a Polygon object
#' @export
#'
#' @examples
#'   library("sf")
#'
#'   # a rectangle
#'  v1 = rbind(c(0,0),c(0,1),c(1,1),c(1,0), c(0,0))
#'  rect = st_polygon(list(v1))
#'
#'  # a triangle
#'  v2 = rbind(c(0.5,0.5),c(1.5,1.5),c(1,0),c(0.5,0.5))
#'  tri = st_polygon(list(v2))
#'
#'
#'  # a circle
#'
#'  # overlapping region
#'  # polygons = ....
#'  # overlap = overlap(rect, tri, circle)
#'  # plot(overlap)
setGeneric("overlap", function(polygon, slice = "all") standardGeneric("overlap"))


#' @rdname overlap
#' @export
setMethod("overlap", c(polygon = "Polygon", slice = "ANY"),
          function(polygon, slice = "all"){
            if (slice[1] != "all"){
              polygon2 = polygon@sets[slice]
              inter = purrr::reduce(polygon2, function(x,y) sf::st_intersection(x,y))
            } else {
              inter = purrr::reduce(polygon@sets, function(x,y) sf::st_intersection(x,y))
            }
            return(inter)
          })


################ Method for polygon difference ############

#' Discern the 'true' unique region of `Polygon` object
#'
#' @name discern
#'
#' @param polygon Polygon object
#' @param slice1 first slice of Polygon object
#' @param slice2 second slice of Polygon object, default is all except the first slice
#'
#' @return a Polygon object
#' @export
#'
#' @examples
#' # don't run
#' # discern(polygon, slice1 = 1)
setGeneric("discern", function(polygon, slice1, slice2) standardGeneric("discern"))

#' @rdname discern
#' @export
setMethod("discern", c(polygon = "Polygon", slice1 = "ANY", slice2 = "ANY"),
          function(polygon,
                   slice1,
                   slice2 = "all") {
            if (is.numeric(slice1)) {
              slice1 = names(polygon@sets)[slice1]
            }

            if (is.numeric(slice2)) {
              slice2 = names(polygon@sets)[slice2]
            }

            if (slice2[1] == "all") {
              slice2 = setdiff(names(polygon@sets), slice1)
              set1 = polygon@sets[slice1] %>% purrr::reduce(function(x, y) sf::st_union(x, y))
              set2 = polygon@sets[slice2] %>% purrr::reduce(function(x, y) sf::st_union(x, y))
              differ = sf::st_difference(set1, set2)
            } else {
              set1 = polygon@sets[slice1] %>% purrr::reduce(function(x, y) sf::st_union(x, y))
              set2 = polygon@sets[slice2] %>% purrr::reduce(function(x, y) sf::st_union(x, y))
              differ = sf::st_difference(set1, set2)
            }

            differ
          }
)


######## Method for polygon specific overlap ===========


#' Calculate region of polygons
#'
#' @param polygon a Polygon object
#' @param slice index of Venn members, default is "all"
#'
#' @return region items
#' @export
#' @name discern_overlap
#'
#' @examples
#' # discern_overlap(polygon)
setGeneric("discern_overlap", function(polygon, slice = "all") standardGeneric("discern_overlap"))

#' calculate the unique region defined by `Venn` object and the parameter `slice`
#'
#' @export
#' @rdname discern_overlap
setMethod("discern_overlap", c(polygon="Polygon", slice="ANY"),
          function(polygon, slice = "all"){
            overlap = overlap(polygon, slice = slice)
            if (slice[1] == "all" | identical(polygon@sets[slice], polygon@sets)){
              discern = NULL
              return(overlap)
            } else {
              discern = discern(polygon, slice1 = slice, slice2 = "all")
              return(sf::st_intersection(overlap, discern))
            }
          })


