################## Method for polygon intersection  ############

#' Calculate the overlapping region of `Polygon` object
#'
#' It returns the overlapping region of all slices in `Polygon` object.
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
#'  # a rectangle
#'  v1 = rbind(c(0,0),c(0,1),c(1,1),c(1,0),c(0,0))
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
            slice = slice_idx(polygon, slice)
            if (slice[1] != "all"){
              polygon2 = polygon@sets[slice]
              inter = Reduce(sf::st_intersection, polygon2)
            } else {
              inter = Reduce(sf::st_intersection, polygon@sets)
            }
            return(inter)
          })


################ Method for polygon difference ############

#' Calculate the region `slice1` has but `slice2` doesn't have of `Polygon` object
#'
#' @name discern
#'
#' @param polygon Polygon object
#' @param slice1 first slice of Polygon object
#' @param slice2 second slice of Polygon object, default is "all" except the first slice
#'
#' @return a Polygon object
#' @export
#'
#' @examples
#' # don't run
#' # discern(polygon, slice1 = 1, slice2 = "all")
setGeneric("discern", function(polygon, slice1, slice2) standardGeneric("discern"))

#' @rdname discern
#' @export
setMethod("discern", c(polygon = "Polygon", slice1 = "ANY", slice2 = "ANY"),
          function(polygon,
                   slice1,
                   slice2 = "all") {
            slice1 = slice_idx(polygon, slice1)
            slice2 = slice_idx(polygon, slice2)
            if (slice2[[1]] == "all") {
              slice2 = polygon@setName[-slice1]
              set1 = Reduce(sf::st_union, polygon@sets[slice1])
              set2 = Reduce(sf::st_union, polygon@sets[slice2])
              differ = sf::st_difference(set1, set2)
            } else {
              set1 = Reduce(sf::st_union, polygon@sets[slice1])
              set2 = Reduce(sf::st_union, polygon@sets[slice2])
              differ = sf::st_difference(set1, set2)
            }

            differ
          }
)




######## Method for polygon specific overlap ===========


#' Calculate the 'unique' region defined by `Venn` object and the parameter `slice`
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

#' @export
#' @rdname discern_overlap
setMethod("discern_overlap", c(polygon="Polygon", slice="ANY"),
          function(polygon, slice = "all"){
            slice = slice_idx(polygon, slice)
            overlap = overlap(polygon, slice = slice)
            if (any(slice == "all") | identical(polygon@sets[slice], polygon@sets)){
              discern = NULL
              return(overlap)
            } else {
              discern = discern(polygon, slice1 = slice, slice2 = "all")
              return(sf::st_intersection(overlap, discern))
            }
          })


######## Check and format slice name ======

#' check and format slice name
#'
#' @param polygon a Polygon object
#' @param slice a numeric or character vector
#'
#' @return the index of polygon (numeric vector) or "all"
slice_idx = function(polygon, slice){
  set_name = polygon@setName
  if (is.numeric(slice)){
    found = slice %in% seq_along(set_name)
    if (all(found)){
      return(slice)
    } else {
      stop(paste("slice is not valid:", slice[!found]))
    }
  }
  if (is.character(slice)){
    if (any(slice == "all")){
      return("all")
    } else {
      matches = match(slice, set_name)
      if (all(!is.na(matches))){
        slice = matches
        return(slice)
      } else {
        non_exist_item = slice[is.na(matches)]
        stop(paste(non_exist_item, "is not found in this object."))
      }
    }
  }
  stop("slice should either be a character or numeric vector.")
}
