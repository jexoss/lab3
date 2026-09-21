#' Dijkstra's algorithm
#'
#' Computes the shortest distance from a starting node to every node in a
#' weighted graph.
#'
#' @details
#' All distances start at infinity, except the starting node which has distance
#' zero. In each step the unvisited node with the smallest distance is picked
#' and marked as visited, and the distances to its neighbours are updated
#' if a shorter path through that node is found. This is repeated until all
#' nodes are visited. Edges are treated as directed (from \code{v1} to
#'\code{v2}) and all weights must be non-negative.
#'
#'@param graph A data.frame with the columns \code{v1}, \code{v2} and \code{w},
#'where each row is an edge from \code{v1} to \code{v2} with weight \code{w}.
#'@param init_node A numeric scalar: the starting node, which must be one of the
#'nodes in \code{graph}.
#'
#'@return A numeric vector with the shortest distance from \code{init_node} to
#'each node, named by node.
#'
#'@references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'
#'@examples
#'g <- data.frame(
#'  v1 = c(1,1,2,3),
#'  v2 = c(2,3,3,4),
#'  w  = c(7,9,10,2)
#')
#'dijkstra(g,1)
#'
#'@export
#'

dijkstra <- function(graph, init_node){
  stopifnot(
    is.data.frame(graph),
    all(c("v1", "v2", "w") %in% names(graph)),
    is.numeric(init_node) && length(init_node) == 1,
    init_node %in% c(graph$v1, graph$v2)
  )

  nodes <- sort(unique(c(graph$v1, graph$v2)))
  dist <- rep(Inf, length(nodes))
  names(dist) <- nodes
  dist[as.character(init_node)] <- 0
  unvisited <- nodes

  while(length(unvisited) > 0) {
    u <- unvisited[which.min(dist[as.character(unvisited)])]
    unvisited <- unvisited[unvisited != u]

    edges <- graph[graph$v1 == u, ]
    for (i in seq_len(nrow(edges))){
      v<- edges$v2[i]
      alt <- dist[as.character(u)] + edges$w[i]
      if (alt < dist[as.character(v)]){
        dist[as.character(v)] <- alt
      }
    }
  }
  unname(dist)
}
