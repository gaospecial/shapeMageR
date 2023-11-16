#' Helper function to add shape
#'
#' @param edge a list of xy matrix
#' @param label a list of xy matrix
#' @param nsets 2:7
#' @param type c("ellipse","triangle","polygon","circle")
#' @param shape_id a unique id
#'
#' @return a tibble with columns: nsets, type, shape_id, component, id, xy.
#'
#' @export
build_shape <- function(edge, label,
                      nsets = length(edge),
                      shape_id,
                      type = c("ellipse","triangle","polygon","circle")){
  type <- match.arg(type)
  geometry = VennPlotData(setEdge = edge, setLabel = label)
  shape = list(shape_id = shape_id,
       type = type,
       nsets = nsets,
       geometry = geometry)

  return(shape)
}
