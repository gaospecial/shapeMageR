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
    slot_name = methods::slotNames(obj)
    l = vector("list", length = length(slot_name))
    l = lapply(slot_name, slot, object = obj)
    names(l) = slot_name
    return(l)
  }

  warning("The class of this object is not supported: ",
          paste(class(obj), collapse = ", "), ".")
  invisible(NULL)
}
