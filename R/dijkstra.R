#' @title Shortest path through network using dijkstra's algorithm
#'
#' @description This function finds the shortest path from a starting node to an end node in a network
#' specified by an edge matrix and vertex coordinates. Position i,j of the edge matrix is one if there
#' is an edge between the ith and jth vertex, zero otherwise. The function returns the path NA with length
#' infinity if the network is disconnected, i.e. if no shortest path can be found.
#'
#' @param edgeMatrix A square matrix consisting of zeros and ones. Has to be zero on the diagonals
#' @param coordMatrix A data frame containing the x and y coordinates of each network vertex
#' @param initialNode A number corresponding to the start node/ vertex
#' @param endNode A number corresponding to the end node/ vertex
#' @param nNodes The number of vertices/ nodes in the network
#' @return A list consisting of a vector with the vertices/ nodes visited by the shortest path and the length of the shortest path.
#' @examples
#' initialNode <- 1
#' endNode <- 4
#' nNodes <- 4
#' edgeMatrix <- matrix(0, nrow = 4, ncol = 4)
#' edgeMatrix[,1] <- c(0,1,0,0)
#' edgeMatrix[,2] <- c(1,0,1,1)
#' edgeMatrix[,3] <- c(0,1,0,0)
#' edgeMatrix[,4] <- c(0,1,0,0)
#' coordMatrix <- matrix(c(0,10,15,20,30,30,15,15),ncol = 2)
#' dijkstra(edgeMatrix, coordMatrix, initialNode, endNode, nNodes)
#'
#' @export

dijkstra <- function(edgeMatrix, coordMatrix, initialNode, endNode, nNodes){
  unvisitedNodes <- as.data.frame(matrix(NA, nrow = nNodes, ncol = 4))
  colnames(unvisitedNodes) <- c('Node','Status', 'CurrentValue','TentDistance')
  unvisitedNodes$Node <- seq(from = 1, to = nNodes, by = 1)
  unvisitedNodes$Status[initialNode] <- "Current"
  unvisitedNodes$CurrentValue[initialNode] <- 0
  visitedNodes <- NULL
  dijkstraPath <- 0

  while ((endNode %in% visitedNodes$Node) == FALSE){
    neighborNodes <- which(edgeMatrix[unvisitedNodes$Node[which(unvisitedNodes$Status == "Current")],] == 1 )

    for (i in 1:length(neighborNodes)){
      idx = which(unvisitedNodes$Node == neighborNodes[i])
      curridx = which(unvisitedNodes$Status == "Current")
      unvisitedNodes$TentDistance[idx] <- unvisitedNodes$CurrentValue[curridx] + euclidDistance(coordMatrix[unvisitedNodes$Node[curridx],1],coordMatrix[unvisitedNodes$Node[curridx],2], coordMatrix[neighborNodes[i],1],coordMatrix[neighborNodes[i],2])

      if (length(idx) != 0){
        unvisitedNodes$CurrentValue[idx] <- min(unvisitedNodes$CurrentValue[idx], unvisitedNodes$TentDistance[idx], na.rm = TRUE)
      }
    }
    # remove current from unvisited set

    removalNode <- which(unvisitedNodes$Status == "Current")
    visitedNodes <- rbind(visitedNodes, unvisitedNodes[removalNode, 1:3])
    unvisitedNodes <- unvisitedNodes[-removalNode,]

    if(any(visitedNodes$Node == endNode) == TRUE){
      break
    } else{
      if(length(visitedNodes[,1]) > 0){
        noNeighborsLeft <- which(edgeMatrix[visitedNodes$Node, -visitedNodes$Node] == 1)
        if(length(noNeighborsLeft) == 0){
          dijkstraPath <- NA
          lengthdijkstra <- Inf
          warning("Network is not connected")
          break
        } else{
          newCurrent <- unvisitedNodes$Node[which(unvisitedNodes$CurrentValue == min(unvisitedNodes$CurrentValue, na.rm = TRUE))]

          if(length(newCurrent) > 1){
            randomIdx <- sample(1:length(newCurrent),1)
            unvisitedNodes$Status[which(unvisitedNodes$Node == newCurrent[randomIdx])] <- "Current"
          } else{
            unvisitedNodes$Status[which(unvisitedNodes$Node == newCurrent)] <- "Current"
          }
        }
      }
    }
  }

  #Recursively solve shortest path

  if(unique(is.na(dijkstraPath)) == FALSE){
    shortestPath <- matrix(NA, nrow = (dim(visitedNodes)[1])+1, ncol = 1)
    shortestPath[1] <- endNode
    nVisited <- dim(visitedNodes)[1]
    visitedNodes <- visitedNodes[seq(from = nVisited, to = 1),]

    # Check whether previous nodes are parent to current node
    # Recursive solving of shortest Path vertices
    child <- visitedNodes$Node[1]
    visitedNodes$directParent <- 0
    visitedNodes$directParent[1] <- 1

    while (visitedNodes$directParent[which(visitedNodes$Node == initialNode)] == 0){
      possibleParents <- which(edgeMatrix[,child] == 1)
      containedParents <- possibleParents[(possibleParents %in% visitedNodes$Node)]

      if (length(containedParents) == 1){
        child <- containedParents
        visitedNodes$directParent[which(visitedNodes$Node == child)] <- 1
      } else{

        childToParent <- matrix(0, nrow = length(containedParents), ncol = 5)
        for(i in 1:length(containedParents)){
          childToParent[i,1] <- containedParents[i]
          childToParent[i,2] <- euclidDistance(coordMatrix[child,1],coordMatrix[child,2],coordMatrix[containedParents[i],1],coordMatrix[containedParents[i],2])
          childToParent[i,4] <- visitedNodes$CurrentValue[which(visitedNodes$Node == containedParents[i])]
        }

        childToParent[,3] <- visitedNodes$CurrentValue[which(visitedNodes$Node == child)] - childToParent[,2]
        childToParent[,5] <- childToParent[,4] - childToParent[,3]
        childToParent[,5] <- abs(childToParent[,5])
        parentNewIdx <- which(childToParent[,5] == min(childToParent[,5]))
        visitedNodes$directParent[which(visitedNodes$Node == childToParent[parentNewIdx,1])] <- 1
        child <- childToParent[parentNewIdx,1]
      }
    }
    dijkstraPath <- visitedNodes$Node[which(visitedNodes$directParent == 1)]
    lengthdijkstra <- visitedNodes[1,3]
  }
  result <- list(dijkstraPath, lengthdijkstra)
  result
}

