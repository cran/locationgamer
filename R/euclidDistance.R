#' @title Euclidean distance between two points
#'
#' @param x1 x-coordinate of point 1
#' @param y1 y-coordinate of point 1
#' @param x2 x-coordinate of point 2
#' @param y2 y-coordinate of point 2
#'
#' @return The Euclidean distance between points 1 and 2 as a number
#' @export
euclidDistance <- function(x1, y1, x2, y2){
  sqrt((x1-x2)^2 + (y1-y2)^2)
}
