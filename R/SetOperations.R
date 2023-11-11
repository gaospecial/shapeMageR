################## method for polygon intersection  ############

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
#' library(ggVennDiagram)
#' venn <- Venn(list(A=1:3,B=2:5,C=c(1L,3L,5L)))
#'
#' discern_overlap(venn, slice = "all")
#' # is equal to
#' overlap(venn, slice = "all")
#'
#' # however, `discern_overlap()` only contains specific region
#' discern_overlap(venn, slice = 1:2)
#'
setGeneric("discern_overlap", function(venn, slice = "all") standardGeneric("discern_overlap"))

#' calculate the unique region defined by `Venn` object and the parameter `slice`
#'
#' @note move to ggVennDiagram
#'
#' @param venn Venn object
#' @param slice a numeric vector indicating the index of slice, default is "all"
#' @export
#' @rdname discern_overlap
setMethod("discern_overlap", c(venn="Venn", slice="ANY"),
          function(venn, slice = "all"){
            overlap = RVenn::overlap(venn, slice = slice)
            if (slice[1] == "all" | identical(venn@sets[slice], venn@sets)){
              discern = NULL
              return(overlap)
            } else {
              discern = RVenn::discern(venn, slice1 = slice)
              return(intersect(overlap, discern))
            }
          })

#' @export
#' @rdname discern_overlap
setMethod("discern_overlap", c(venn="Polygon", slice="ANY"),
          function(venn, slice = "all"){
            overlap = overlap(venn, slice = slice)
            if (slice[1] == "all" | identical(venn@sets[slice], venn@sets)){
              discern = NULL
              return(overlap)
            } else {
              discern = discern(venn, slice1 = slice)
              return(sf::st_intersection(overlap, discern))
            }
          })


