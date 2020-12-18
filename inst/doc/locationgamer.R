## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----randomNetwork------------------------------------------------------------
nNodes <- 6
xMin <- 0
xMax <- 100
yMin <- 0
yMax <- 100
coordMatrix <- matrix(c(0,10,3,20,10,90,80,30,50,100,40,75), nrow = nNodes, ncol =2, byrow = TRUE)
edgeMatrix <- matrix(0, ncol = nNodes, nrow = nNodes)
edgeMatrix[,1] <- c(0,1,0,1,0,0)
edgeMatrix[,2] <- c(1,0,1,0,1,0)
edgeMatrix[,3] <- c(0,1,0,0,0,0)
edgeMatrix[,4] <- c(1,0,0,0,1,0)
edgeMatrix[,5] <- c(0,1,0,1,0,1)
edgeMatrix[,6] <- c(0,0,0,0,1,0)

locationgamer::plotNetwork(edgeMatrix, coordMatrix)

startNode <- 1
endNode <- 6

dijkstraResult <- locationgamer::dijkstra(edgeMatrix, coordMatrix, startNode, endNode, nNodes)
locationgamer::plotDijkstra(edgeMatrix, coordMatrix, dijkstraResult[[1]])

## ----demandVector-------------------------------------------------------------
demandLoc <- c(100,300,50,400,200,350)

## ----NashLocations------------------------------------------------------------
locationgamer::lgsolve(edgeMatrix, coordMatrix, 2, demandLoc)

