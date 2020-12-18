#' @title Create distance matrix for a completely connected network
#'
#' @param coordMatrix A matrix containing all the x and y coordinates of the network vertexes
#' @return A square matrix containing the Euclidean distances between all vertexes, assuming that the network is completely connected.
#' @examples
#' coordMatrix <- matrix(c(0,10,15,20,30,30,15,15),ncol = 2)
#' createDistance(coordMatrix)
#'
#' @export
#'
createDistance <- function(coordMatrix){
  distMat <- matrix(0, nrow = dim(coordMatrix)[1], ncol = dim(coordMatrix)[1])
  for (i in 1:dim(coordMatrix)[1]){
    for (j in 1:dim(coordMatrix)[1]){
      distMat[i,j] = sqrt((coordMatrix[i,1]-coordMatrix[j,1])^2 + (coordMatrix[i,2]-coordMatrix[j,2])^2)
    }
  }
  distMat
}
