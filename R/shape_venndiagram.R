########## Complex Shapes ###########


##### Five dimension ellipses ######
fancy_5d_ellipse = function(){
  cx = 120
  cy = -275
  rx = 1000 * 2
  ry = 1587 * 2
  ellipses = lapply(1:5, function(i){
    ellipse(cx, cy, rx, ry, 72*(i-1), n = 100)
  })
  return(ellipses)
}

fancy_5d_ellipse_label = function(){
  x = 120
  y = 1500
  position = lapply(1:5, function(i){
    data = rotate(x, y, cx = 1000, cy = 1587, 72*(i-1)) |> unlist()
    matrix(data, ncol = 2, dimnames = list(NULL, c("x","y")))
  })
  return(position)
}

#' Rotate a point by the other point
#'
#' @param x,y point coordinates
#' @param cx,cy center coordinates
#' @param angle angle
#'
#' @return
#' @export
#'
#' @examples
#'   rotate(1,1,0,0,90)
rotate = function(x, y, cx, cy, angle){
  # 定义旋转角度（以弧度为单位）
  theta = angle * (pi / 180)

  # 计算旋转后的点坐标
  x_rotated <- cos(theta) * (x - cx) - sin(theta) * (y - cy) + cx
  y_rotated <- sin(theta) * (x - cx) + cos(theta) * (y - cy) + cy

  return(list(x = x_rotated, y = y_rotated))

}

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
fancy_4d_ellipse_label <- function(position = NULL) {
  if (is.null(position)) {
    position = data.frame(
      x = c(0.08, 0.26, 0.71, 0.93),  # x coordinate
      y = c(0.78, 0.86, 0.85, 0.78)   # y coordinate
    )
  }
  to_matrix_list(position)
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
    position = data.frame(
      x = c(-3.5, 7.5, 2.0),
      y = c(4.6, 4.6, -8.5)
    )
  to_matrix_list(position)
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
    position = data.frame(
      x = c(-5, -5),
      y = c( 0,  4)
    )
  to_matrix_list(position)
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
    position <- data.frame(
      x = c(-50000, 60000, 160000, 280000, 140000, -20000),
      y = c( 50000,     0,  20000, 170000, 300000, 270000)
    )
  to_matrix_list(position)
}


to_matrix_list <- function(position){
  points <- lapply(seq_len(nrow(position)),function(i){
    as.matrix(position[i,])
  })
  points
}
