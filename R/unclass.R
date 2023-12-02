#' Remove shape class
#'
#' Unlink shapes from the 'shapeMageR' package.
#'
#' @param obj a VennPlotData object
#'
#' @return a list
#' @export
#'
#' @examples
#' # 4d ellipse
#' f4e = VennPlotData(
#'   shapeId = "401f",
#'   type = "ellipse",
#'   setEdge = fancy_4d_ellipse(),  # how to store object in data.frame
#'   setLabel =  fancy_4d_ellipse_label())
#' l = unclass(f4e)
#' l
unclass = function(obj){
  if (inherits(obj, "VennPlotData")){
    l = list(
      shapeId = obj@shapeId,
      type = obj@type,
      setEdge = obj@setEdge,
      setLabel = obj@setLabel,
      region = obj@region
    )
    return(l)
  }

  warning("The class of this object is not supported: ",
          paste(class(obj), collapse = ", "), ".")
  invisible(NULL)
}
