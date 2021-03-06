% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dijkstra.R
\name{dijkstra}
\alias{dijkstra}
\title{Shortest path through network using dijkstra's algorithm}
\usage{
dijkstra(edgeMatrix, coordMatrix, initialNode, endNode, nNodes)
}
\arguments{
\item{edgeMatrix}{A square matrix consisting of zeros and ones. Has to be zero on the diagonals}

\item{coordMatrix}{A data frame containing the x and y coordinates of each network vertex}

\item{initialNode}{A number corresponding to the start node/ vertex}

\item{endNode}{A number corresponding to the end node/ vertex}

\item{nNodes}{The number of vertices/ nodes in the network}
}
\value{
A list consisting of a vector with the vertices/ nodes visited by the shortest path and the length of the shortest path.
}
\description{
This function finds the shortest path from a starting node to an end node in a network
specified by an edge matrix and vertex coordinates. Position i,j of the edge matrix is one if there
is an edge between the ith and jth vertex, zero otherwise. The function returns the path NA with length
infinity if the network is disconnected, i.e. if no shortest path can be found.
}
\examples{
initialNode <- 1
endNode <- 4
nNodes <- 4
edgeMatrix <- matrix(0, nrow = 4, ncol = 4)
edgeMatrix[,1] <- c(0,1,0,0)
edgeMatrix[,2] <- c(1,0,1,1)
edgeMatrix[,3] <- c(0,1,0,0)
edgeMatrix[,4] <- c(0,1,0,0)
coordMatrix <- matrix(c(0,10,15,20,30,30,15,15),ncol = 2)
dijkstra(edgeMatrix, coordMatrix, initialNode, endNode, nNodes)

}
