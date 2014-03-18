#' find the best trades to make and execute them
library(Rglpk)

augmentgraph <- function(graph, vertex, capital = Inf){
  graph + vertices('source','sink') + 
    edge('source',vertex,rate=1,volume=capital)+
    edge(vertex,'sink',rate=1,volume=Inf)
}

myincmat <- function(graph){
  # rows are vertices, columns edges, entries rates
  incmat <- matrix(0,nrow=vcount(graph),ncol=ecount(graph))
  rownames(incmat) <- V(graph)$name
  l_ply(1:ecount(graph), function(edgeid){
    ends <- get.edge(graph, edgeid)
    incmat[ends,edgeid] <<- c(-1,get.edge.attribute(graph,'rate',edgeid))
  })
  return(incmat)
}

optimize <- function(graph){
  # obj coefficients: maximize sink, minimize source
  obj <- rep.int(x=0,ecount(graph))
  obj[incident(graph,V(graph)['sink'],'in')] <- 1
  obj[incident(graph,V(graph)['source'],'out')] <- -1
  
  # bounds: based on volume (arbitraily refers to start volume)
  bounds <- list(lower = list(ind = 1:ecount(graph), val = rep.int(0,ecount(graph))),
                 upper = list(ind = 1:ecount(graph), val = E(graph)$volume)
  )
  
  # constraints: make sure we don't lose any kind of money
  dir = rep_len('>=',vcount(graph)) # don't wanna end up with less money than we started with
  rhs = rep.int(0,vcount(graph)) # we start with 0 of everything
  rhs[V(graph)$name=='source'] <- -Inf # can use as much source as we like 
  
  # coefficient matrix: columns are vertices, rows edges, entries rates
  mat <- myincmat(graph)
  
  # run it
  result <- Rglpk_solve_LP(obj,mat,dir,rhs,bounds,max=TRUE,control=list(canonicalize_status=FALSE))
}