########## Complex Shapes ###########


##### Four dimension ellipses ######

#' fancy 4d ellipse from `VennDiagram`
#'
#' @param parameters will pass to shape generators
#' @param n count of points to shape this polygon
#'
#' @export
#'
#' @return a list of coordinates matrix
fancy_4d_ellipse <- function(parameters = NULL, n = 100){
  # 4d ellipses
  if (is.null(parameters))
    parameters <- list(c(0.35, 0.47, 0.35, 0.20, 135),
                     c(0.50, 0.57, 0.35, 0.15, 135),
                     c(0.50, 0.57, 0.33, 0.15,  45),
                     c(0.65, 0.47, 0.35, 0.20,  45))
  ellipses <- lapply(parameters,function(x){
    do.call(ellipse, as.list(c(x,n)))
  })

  ellipses
}


#' @export
#' @rdname label_position
fancy_4d_ellipse_label <- function(position = NULL) {
  if (is.null(position)) {
    position = tibble::tribble(
      ~x,    ~y,
      0.08, 0.78,
      0.26, 0.86,
      0.71, 0.85,
      0.93, 0.78
    )
  }
  label_position(position)
}

############## Three dimension circle #########

#' fancy 3d circle
#' @inheritParams fancy_4d_ellipse
#' @export
fancy_3d_circle <- function(parameters = NULL, n = 100){
  # three circles
  if(is.null(parameters))
    parameters <- list(c(0,0,4),c(4,0,4), c(2,-4,4))

  circles <- lapply(parameters, function(x){
    do.call(circle, as.list(c(x,n)))
  })

  circles
}

#' @export
#' @rdname label_position
fancy_3d_circle_label <- function(position = NULL){
  if (is.null(position))
    position <- tibble::tribble(
      ~x,       ~y,
      -3.5,     4.6,
      7.5,     4.6,
      2,      -8.5
    )
  label_position(position)
}

#' two dimension circle
#' @inheritParams fancy_4d_ellipse
#' @export
fancy_2d_circle <- function(parameters = NULL, n = 100){
  if(is.null(parameters))
    parameters <- list(c(0,0,4),c(0,4,4))

  circles <- lapply(parameters, function(x){
    do.call(circle,as.list(c(x,n)))
  })

  circles
}

#' @export
#' @rdname label_position
fancy_2d_circle_label <- function(position = NULL){
  if (is.null(position))
    position <- tibble::tribble(
      ~x,       ~y,
      -5,      0,
      -5,      4
    )
  label_position(position)
}

#' Six dimension triangle
#'
# triangles source: https://upload.wikimedia.org/wikipedia/commons/5/56/6-set_Venn_diagram_SMIL.svg
#' @inheritParams fancy_4d_ellipse
#' @export
fancy_6d_triangle <- function(parameters = NULL){
  if(is.null(parameters))
    parameters <- list(c(-69277,-32868,135580,121186, 70900,199427),
                   c( 81988,-44426, 38444,206222,121044,165111),
                   c(203271,  9619, 39604, 82683, 84652,206669),
                   c(333561,225349, 61764, 76805, 38980,182461),
                   c(131886,385785, 38136,111491, 94208, 24690),
                   c(-60184,274046,142476, 39903,103276,183962))

  shapes <- lapply(parameters, function(x){
    triangle(x)
  })

  shapes
}

#' @export
#' @rdname label_position
fancy_6d_triangle_label <- function(position = NULL){
  if (is.null(position))
    position <- tibble::tribble(
      ~x,       ~y,
      -50000,     50000,
      60000,          0,
      160000,     20000,
      280000,    170000,
      140000,    300000,
      -20000,   270000
    )
  label_position(position)
}

#' helper function to set label position
#'
#' @param position a two column (x, y) data.frame containing label coordinates
#' @export
#'
#' @return a list of matrix
#' @name label_position
#'
#' @details
#' - `label_position`: basal wrapper for label positions
#' - `fancy_6d_triangle_label`: 6 sets triangle label position work with `fancy_6d_triangle`
#' - `fancy_4d_ellipse_label`: 4 sets ellipse label position work with `fancy_4d_ellipse`
#' - `fancy_3d_circle_label`: 3 sets circle label position work with `fancy_3d_circle`
#' - `fancy_2d_circle_label`: 2 sets circle label position work with `fancy_2d_circle`
#' @md
#'
#' @examples
#' fancy_4d_ellipse_label()
#' fancy_2d_circle_label()
label_position <- function(position){
  points <- lapply(seq_len(nrow(position)),function(i){
    position[i,]
  })
  points
}
