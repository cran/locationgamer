#' @title Create random coordinates for network vertexes
#'
#' @param nNodes The number of vertexes/ nodes in the network
#' @param xMax The maximum x-coordinate of the nodes in the network
#' @param xMin The minimum x-coordinate of the nodes in the network
#' @param yMax The maximum y-coordinate of the nodes in the network
#' @param yMin The minimum y-coordinate of the nodes in the network
#'
#' @return A data frame with dimensions nNodes x 2 containing the x and y coordinates of the network's vertexes
#'
#' @examples
#' nNodes <- 10
#' xMax <- 2000
#' xMin <- 0
#' yMax <- 3000
#' yMin <- 200
#' randomCoordinates(nNodes, xMax, xMin, yMax, yMin)
#'
#' @export

randomCoordinates <- function(nNodes, xMax, xMin, yMax, yMin){
  nodeCoordinates <- as.data.frame(matrix(0, ncol = 2, nrow = nNodes))
  colnames(nodeCoordinates) <- c("xCoordinate", "yCoordinate")
  for (i in 1:nNodes){
    nodeCoordinates[i,1] <- sample(x = seq(from = xMin, to = xMax, by = 1), size = 1, replace = TRUE)
    nodeCoordinates[i,2] <- sample(x = seq(from = yMin, to = yMax, by = 1), size = 1, replace = TRUE)
  }
  nodeCoordinates
}
