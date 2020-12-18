#' @title Minimum spanning tree using Prim's algorithm
#'
#' @param distMatrix A square matrix containing the distances between all vertexes of a network
#' @return A matrix with rows describing which vertex is connected to which other vertex.
#' @examples
#' distMatrix <- matrix(c(0,10,20,30,10,0,40,60,20,40,0,30,30,60,30,0),
#' nrow = 4, ncol = 4, byrow = TRUE)
#' primDistance(distMatrix)
#'
#' @import graphics
#'
#' @export
#'
primDistance <- function(distMatrix){
  diag(distMatrix) <- NA
  startNode <- 1
  nNodes <- dim(distMatrix)[1]
  auxMatrix <- matrix(NA, nrow = dim(distMatrix)[1], ncol = dim(distMatrix)[2])
  diag(auxMatrix) <- NA
  auxMatrix[startNode,] <- distMatrix[startNode,]
  shortestPath <- NULL

  while (length(startNode) < nNodes){
    auxMatrix[,union(shortestPath[,1],shortestPath[,2] )] <- NA
    shortestDistance <- which(auxMatrix == min(auxMatrix, na.rm = TRUE), arr.ind = TRUE)

    if(dim(shortestDistance)[1] > 1){
      shortestDistance <- matrix(c(shortestDistance[1,1],shortestDistance[1,2]), nrow = 1, ncol = 2)
    }else{
      shortestDistance <- shortestDistance
    }

    startNode <- append(startNode, shortestDistance[,2], after = length(startNode))
    auxMatrix[startNode[length(startNode)],] <- distMatrix[startNode[length(startNode)],]
    shortestPath <- rbind(shortestPath, shortestDistance)
  }
  shortestPath
}
