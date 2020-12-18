#' @title Plotting minimum spanning tree connecting all vertexes
#'
#' @param minimumSp A data frame in which each row corresponds to an edge between two numbered vertexes Use function primDistance
#' to obtain minimum spanning tree using Prim's algorithm.
#' @param coordMat A matrix containing all the x and y coordinates of the network vertexes.
#'
#' @examples
#' minimumSp <- matrix(c(1,4,4,3,2,3),ncol = 2)
#' coordMatrix <- matrix(c(0,10,15,20,30,30,15,15),ncol = 2)
#' plotPrim(minimumSp, coordMatrix)
#'
#' @import graphics
#'
#' @export

plotPrim <- function(minimumSp, coordMat){
  minAx <- min(union(coordMat[,1], coordMat[,2]))
  maxAx <- max(union(coordMat[,1], coordMat[,2]))
  plot(coordMat[,1], coordMat[,2], xlim = c(minAx,maxAx), ylim = c(minAx,maxAx), xlab = "X", ylab = "Y", main = "Prim - Minimum spanning tree")
  for (i in 1:dim(minimumSp)[1]){
    segments(coordMat[minimumSp[i,1],1],coordMat[minimumSp[i,1],2],coordMat[minimumSp[i,2],1],coordMat[minimumSp[i,2],2])
  }
}
