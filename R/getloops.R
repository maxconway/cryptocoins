library(igraph)
# functions for working out costs of conversion between nodes

#' getcost
#' 
#' takes a node and returns a list showing which nodes can be directly accessed from it, and the profit of them.
#' 
#' @param graph a directed igraph, with edges having a \code{rate} property.
#' @param pred the predecessor. A list containing:\describe{
#'   \item{node}{The node id}
#'   \item{profit}{The profit of getting to the node}
#'   \item{path}{The path to the node (optional)}
#' }
#' 
#' @return A list of nodes, each in the same format as pred.
#'   
getcost <- function(graph, pred){
  lapply(incident(graph=graph,v=pred$node,mode='out'),function(edge){
    structure(
      list(
        node = get.edge(graph,edge)[2],
        path = c(pred$path,edge),
        profit = pred$profit*E(graph)[edge]$rate
      ),
      class='nodewithpath'
    )
  })
}


#' getcost
#' 
#' takes a node and returns a list showing which nodes can be accessed from it to depth \code{maxdepth} with \code{profit>1}.
#' 
#' @param graph a directed igraph, with edges having a \code{rate} property.
#' @param pred the predecessor. A list containing:\describe{
#'   \item{node}{The node id}
#'   \item{profit}{The profit of getting to the node}
#'   \item{path}{The path to the node (optional)}
#' }
#' @param maxdepth the maximum depth to search to
#' 
#' @return A list of nodes, each in the same format as pred.
#' 
getloops <- function(graph, origin, pred=list(node=origin, path=NULL, profit=1), maxdepth = Inf){
  if(V(graph)[pred$node]==V(graph)[origin] && !is.null(pred$path)){
    if(pred$profit>1 && is.finite(pred$profit)){
      return(list(pred))
    }else{
      return(NULL)
    }
  }
  
  if(length(pred$path)>maxdepth){
    return(NULL)
  }
  
  do.call(c,
          lapply(getcost(graph, pred),function(pred){
            getloops(graph, origin, pred, maxdepth)
          })
  )
}