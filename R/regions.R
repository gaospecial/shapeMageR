# region items and polygons

#' all possible combinations of n sets
#'
#' @param n dim
#'
#' @importFrom utils combn
combinations <- function(n){
  l <- lapply(seq_len(n), function(x){
    m <- combn(n,x)
    matrix2list(m)
  })
  unlist(l, recursive = F)
}

matrix2list <- function(matrix){
  lapply(seq_len(ncol(matrix)), function(i) matrix[,i])
}




