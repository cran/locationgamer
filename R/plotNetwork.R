#' @title Plotting a network consisting of edges and vertexes
#' @param edgeMatrix A matrix containing zeros and ones if an edge between two vertexes is absent or not
#' @param coordMatrix A data frame containing the x and y coordinates of each vertex of the network
#' @return A plot of the connected network
#' edgeMatrix <- matrix(0, nrow = 4, ncol = 4)
#' edgeMatrix[,1] <- c(0,1,0,0)
#' edgeMatrix[,2] <- c(1,0,1,1)
#' edgeMatrix[,3] <- c(0,1,0,0)
#' edgeMatrix[,4] <- c(0,1,0,0)
#' coordMatrix <- matrix(c(0,10,15,20,30,30,15,15),ncol = 2)
#' plotNetwork(edgeMatrix, coordMatrix)
#'
#' @import graphics
#'
#' @export

plotNetwork <- function(edgeMatrix, coordMatrix){
  minAx <- min(union(coordMatrix[,1], coordMatrix[,2]))
  maxAx <- max(union(coordMatrix[,1], coordMatrix[,2]))
  plot(coordMatrix[,1], coordMatrix[,2], xlim = c(minAx,maxAx), ylim = c(minAx,maxAx), xlab = "X", ylab = "Y")
  for (i in 1:dim(edgeMatrix)[1]){
    for (j in 1:dim(edgeMatrix)[2]){
      if(edgeMatrix[i,j] == 1){
        segments(coordMatrix[i,1], coordMatrix[i,2], coordMatrix[j,1], coordMatrix[j,2])
      }
    }
  }
}
