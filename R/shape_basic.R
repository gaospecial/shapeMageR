#' functions to generate ellipse, circle, triangle and other shapes,
#' @name shape_generator
NULL


#' generating a closed ellipse
#'
#' This function is derived from `VennDiagram::ell2poly`, we modified it and then
#' it can generating a closed ellipse, which is a requirement for further transformation
#' to a POLYGON sf object.
#'
#' @param x,y the coordinates of ellipse center
#' @param a radius of short arm
#' @param b radius of long arm
#' @param rotation rotation in degree
#' @param n number of points
#'
#' @return a matrix representing ellipse coordinates
#' @export
#'
#' @examples
#' # plot the default ellipse
#' library(sf)
#' ellipse() %>% as.matrix() %>% st_linestring() %>% plot()
ellipse <- function(x = 0, y = 0, a = 2, b = 1, rotation = 0, n = 100){
  rotation <- rotation * pi/180
  theta <- 2 * pi/n
  angles <- seq(0, 2 * pi, theta)
  x.coord <- vector(length = n+1, mode = "numeric")
  y.coord <- vector(length = n+1, mode = "numeric")
  for (i in 1:n) {
    x.coord[i] <- x + a * cos(angles[i]) * cos(rotation) -
      b * sin(angles[i]) * sin(rotation)
    y.coord[i] <- y + a * cos(angles[i]) * sin(rotation) +
      b * sin(angles[i]) * cos(rotation)
  }
  # close ellipse
  x.coord[n+1] <- x.coord[1]
  y.coord[n+1] <- y.coord[1]

  data.frame(x = x.coord, y = y.coord)
}


#' generating a circle
#'
#' @param x,y center of circle
#' @param r radius of circle
#' @param n number of points for polygon object (resolution)
#'
#' @return a matrix representing circle coordinates
#' @export
#'
#' @examples
#' # plot the default circle
#' library(sf)
#' circle() %>% as.matrix() |>  st_linestring() %>% plot()
circle <- function(x = 0, y = 0, r = 1, n=100){
  angles <- seq(0,2*pi,length.out = n)
  x.coord <- x + cos(angles) * r
  y.coord <- y + sin(angles) * r
  x.coord[n] <- x.coord[1]
  y.coord[n] <- y.coord[1]
  data.frame(x=x.coord, y=y.coord)
}

#' generating a triangle by three points
#'
#' @param xy coordinates of the three points defining a triangle
#'
#' @export
#' @return a matrix with xy coordinates
#'
#' @examples
#' # triangle coordinates
#' library(sf)
#' triangle()
#'
#' # plot a new triangle
#' triangle() %>% as.matrix() %>% st_linestring() %>% plot()
triangle <- function(xy = c(0,0,1,0,0,1)){
  xy <- matrix(rep(xy, length.out =8), ncol=2, byrow = TRUE)
  xy = as.data.frame(xy)
  colnames(xy) <- c("x","y")
  return(xy)
}


#' generating a rectangle by two or four points
#'
rectangle = function(){

}
