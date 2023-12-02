################# Class Polygon ###############
# Assign a name to Polygon objects
# Are Polygon objects equal to MULTIPOLYGON or GEOMETRYCOLLECTION?
# No. MULTIPOLYGON is a special form of Polygon.

#' An S4 class to represent multiple polygons.
#'
#' @slot sets A list contains sets
#' @slot names The names of the `sets` if has names. If the `list`
#'   doesn't have names, the sets will be named as "Set_1", "Set_2"
#'   and so on.
#' @name Polygon-class
setClass("Polygon",
         slots = list(sets = "ANY", names = "ANY"))


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
            if (!is.list(sets)){
              stop("Data sets should be a list.")
            }

            if (sum(sapply(sets, is.null) == TRUE) >= 1){
              sets = sets[!(sapply(sets, is.null))]
            }

            if (length(sets) < 1){
              stop("The list should contain at least 1 vector.")
            }

            if (length(unique(lapply(sets, class))) != 1) {
              stop("Vectors should be in the same class.")
            }

            if (!(sapply(sets, class)[1] %in% c("XY", "POLYGON", "sfg"))) {
              stop("The list must contain only XY, POLYGON or sfg object.")
            }

            polygon = new(Class = "Polygon", sets = sets)

            if (is.null(names(polygon@sets))) {
              names(polygon@sets) = paste("Set", seq_len(length(polygon@sets)), sep = "_")
            }

            polygon@names = names(polygon@sets)

            polygon
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
                      region = "ANY"))

#' VennPlotData constructor
#'
#' @param setEdge a list of coordinates matrix defining Venn set edges
#' @param shapeId shape id
#' @param type type of shape, can be one of ellipse, circle, triangle, or polygon
#' @param setLabel a list of coordinates matrix defining Venn set labels
#'
#' @return a S4 class VennPlotData object
#'
#' @name VennPlotData
#' @docType methods
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
            if (!all(sapply(setEdge, is.data.frame), sapply(setLabel, is.data.frame)))
              stop("The element in setEdge/setLabel must be a data.frame with two columns (x, y)")

            nsets = length(setEdge)
            edge <- .setEdge(setEdge)
            label <- .setLabel(setLabel)
            region <- .region(setEdge)
            data = new(Class = "VennPlotData",
                       shapeId = shapeId,
                       type = type,
                       nsets = nsets,
                       setEdge = edge,
                       setLabel = label,
                       region = region)
            data
          })

.setEdge <- function(setEdge){
  setEdge = lapply(setEdge, as.matrix)
  linestrings <- lapply(setEdge, sf::st_linestring)
  d <- tibble::tibble(
    id = as.character(seq_len(length(setEdge))),
    geometry = sf::st_as_sfc(linestrings)
  )
  return(d)
}

.setLabel <- function(setLabel){
  setLabel = lapply(setLabel, as.matrix)
  points <- lapply(setLabel, sf::st_point)
  d <- tibble::tibble(
    id = as.character(seq_len(length(setLabel))),
    geometry = sf::st_as_sfc(points)
  )
  return(d)
}

.region <- function(setEdge){
  setEdge = lapply(setEdge, as.matrix)
  polygons <- lapply(setEdge, function(x) sf::st_polygon(list(x)))
  polygon <- Polygon(polygons)
  regions <- get_region_items(polygon)
  region_id <- get_region_ids(polygon)
  d <- tibble::tibble(
    id = region_id,
    geometry = sf::st_as_sfc(regions)
  )
  return(d)
}

get_region_items <- function(polygon){
  n = length(polygon@sets)
  c = combinations(n)
  lapply(c, function(i) discern_overlap(polygon,i))
}


get_region_ids <- function(polygon){
  n = length(polygon@sets)
  c = combinations(n)
  sapply(c, function(i) paste0(i, collapse = ""))
}


get_region_names <- function(polygon){
  n = length(polygon@sets)
  set_name = polygon@names
  c = combinations(n)
  sapply(c, function(i) paste0(set_name[i], collapse = ".."))
}

