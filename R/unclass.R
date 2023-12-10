#' Remove shape class
#'
#' Unlink shapes from the 'shapeMageR' package.
#'
#' @param obj a VennShape object
#'
#' @return a list, the items are data.frame for plot
#'
#' @importFrom methods slot
#' @export
#'
#' @examples
#' # 4d ellipse
#' f4e = VennShape("401f", "ellipse", fancy_4d_ellipse(), fancy_4d_ellipse_label())
#' l = unclass(f4e)
#' l
unclass = function(obj){
            slot_name = methods::slotNames(obj)
            l = vector("list", length = length(slot_name))
            l = lapply(slot_name, slot, object = obj)
            names(l) = slot_name
            l = lapply(l, sfc2df)
            return(l)
}

# from sfc to a combined/tidy data.frame
sfc2df = function(x){
  if (!is.data.frame(x)) return(x)
  if ("geometry" %in% colnames(x)){
    geometry = x$geometry
    list = lapply(seq_along(geometry), function(i){
        sfg2df(geometry[[i]], x$id[[i]])
      })
    data = do.call("rbind", list)
    return(data)
  } else {
    stop("Must have a geometry column (sfc).")
  }
}

# from a sfg to a data.frame
sfg2df = function(sfg, id){
  coords = sf::st_coordinates(sfg)
  coords = as.data.frame(coords)
  coords$id = id
  coords = coords[,c("id","X","Y")]
  return(coords)
}
