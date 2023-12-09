################# Class Polygon ###############
# Assign a name to Polygon objects
# Are Polygon objects equal to MULTIPOLYGON or GEOMETRYCOLLECTION?
# No. MULTIPOLYGON is a special form of Polygon.

#' An S4 class to represent multiple shapes/polygons.
#'
#' @slot sets A list contains sets
#' @slot names The names of the `sets` if has names. If the `list`
#'   doesn't have names, the sets will be named as "Set_1", "Set_2"
#'   and so on.
#' @name Polygon-class
setClass("Polygon",
         slots = list(sets = "ANY", setName = "ANY"))


#' Polygon class object constructor
#'
#' @param sets a list containing multiple simple features
#' @export
#' @docType methods
#' @rdname polygon-methods
setGeneric("Polygon", function(sets){
  standardGeneric("Polygon")
})


#' @rdname polygon-methods
#' @export
#' @importFrom methods new
setMethod("Polygon", c(sets = "ANY"),
          function(sets){
            set_name = names(sets)
            if (is.null(set_name)){
              set_name = paste("Set", seq_along(sets), sep = "_")
            }
            names(sets) = set_name
            polygon = new(Class = "Polygon", sets = sets, setName = set_name)
            return(polygon)
          })

############# Class VennPlotData  ###################
# Some of the code should be moved to ggVennDiagram.
# We need to directly retrieved shapes from sysdata,
# and contruct VennPlotData with region items.

#' An S4 class to represent Venn plot components.
#'
#' @slot shapeId shape id
#' @slot setEdge a list of coordinates matrix defining Venn set edges
#' @slot region the feature region will be calculated automatically with `setEdge`
#' @slot type type of shape
#' @slot nsets number of sets
#' @slot setLabel label of sets
setClass("VennPlotData",
         slots = c(shapeId = "ANY",
                   type = "ANY",
                   nsets = "ANY",
                   setEdge = "ANY",
                   setLabel = "ANY",
                   regionEdge = "ANY",
                   regionLabel = "ANY"))

#' VennPlotData constructor
#'
#' Region Edge and Label will be caculated automatically with functions from `sf` package.
#'
#' @param shapeId shape id
#' @param type type of shape, can be one of ellipse, circle, triangle, or polygon
#' @param setEdge a list of coordinates matrix defining Venn set edges
#' @param setLabel a list of coordinates matrix defining Venn set labels
#'
#' @return a S4 class VennPlotData object
#'
#' @name VennPlotData
#' @docType methods
#' @examples
#' # construct a VennPlotData
#' venn = VennPlotData(shapeId = "1",
#'                     type = "e",
#'                     setEdge = fancy_4d_ellipse(),
#'                     setLabel = fancy_4d_ellipse_label())
setGeneric("VennPlotData", function(shapeId, type, setEdge, setLabel){
  standardGeneric("VennPlotData")
})



#' @rdname VennPlotData
#' @export
#' @importFrom methods new
setMethod("VennPlotData", c(shapeId = "ANY",
                            type = "ANY",
                            setEdge = "ANY",
                            setLabel = "ANY"),
          function(shapeId, type, setEdge, setLabel){
            valid_type = c("ellipse","triangle","polygon","circle")
            type = match.arg(type, choices = valid_type)
            if (!is.list(setEdge) | !is.list(setLabel))
              stop("SetEdge/setLabel must be a list.")
            if (length(setEdge) != length(setLabel))
              stop("SetEdge/setlabel must be the same length.")
            if (length(setEdge) < 1)
              stop("SetEdge/SetLabel should have at least one item.")
            if (!all(sapply(setEdge, is.matrix), sapply(setLabel, is.matrix)))
              stop("The element in setEdge/setLabel must be a matrix with two columns (x, y)")

            nsets = length(setEdge)
            set_Edge = .setEdge(setEdge)
            set_Label = .setLabel(setLabel)
            region_Edge = .regionEdge(setEdge)
            region_Label = .regionLabel(region_Edge)
            data = new(Class = "VennPlotData",
                       shapeId = shapeId,
                       type = type,
                       nsets = nsets,
                       setEdge = set_Edge,
                       setLabel = set_Label,
                       regionEdge = region_Edge,
                       regionLabel = region_Label)
            data
          })

.setEdge <- function(setEdge){
  linestrings <- lapply(setEdge, sf::st_linestring)
  d <- data.frame(
    id = as.character(seq_len(length(setEdge))),
    geometry = sf::st_as_sfc(linestrings)
  )
  return(d)
}

.setLabel <- function(setLabel){
  points <- lapply(setLabel, sf::st_point)
  d <- data.frame(
    id = as.character(seq_len(length(setLabel))),
    geometry = sf::st_as_sfc(points)
  )
  return(d)
}

.regionEdge <- function(setEdge){
  polygons <- lapply(setEdge, function(x) sf::st_polygon(list(x)))
  polygon <- Polygon(polygons)
  regions <- get_region_items(polygon)
  region_id <- get_region_ids(polygon)
  d <- data.frame(
    id = region_id,
    geometry = sf::st_as_sfc(regions)
  )
  return(d)
}

.regionLabel = function(regionEdge){
  regionLabel = regionEdge
  regionLabel$geometry = sf::st_centroid(regionLabel$geometry)
  return(regionLabel)
}

get_region_items <- function(polygon){
  n = length(polygon@sets)
  c = combinations(n)
  lapply(c, function(i) discern_overlap(polygon,i))
}


get_region_ids <- function(polygon){
  n = length(polygon@sets)
  c = combinations(n)
  sapply(c, function(i) paste0(i, collapse = "/"))
}


get_region_names <- function(polygon){
  n = length(polygon@sets)
  set_name = polygon@names
  c = combinations(n)
  sapply(c, function(i) paste0(set_name[i], collapse = "/"))
}

