#' Transform a VennPlotData object to a generic list
#'
#' So that users can use this object without shapeMageR package.
#'
#' @param x a VennPlotData class object.
#'
#' @return
#' @export
#'
#' @examples
#'  f4e = VennPlotData(
#'   shapeId = "401f",
#'   type = "ellipse",
#'   setEdge = fancy_4d_ellipse(),
#'   setLabel =  fancy_4d_ellipse_label()
#'   )
#'  list = as.list(f4e)
#'  names(f4e)
setMethod("as.list", c(x = "VennPlotData"),
          function(x){
            return(list(
              shapeId = x@shapeId,
              type = x@type,
              nsets = x@nsets,
              setEdge = x@setEdge,
              setLabel = x@setLabel,
              region = x@region
            ))
          })
