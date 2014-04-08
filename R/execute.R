#'select_opportunities
#'
#' prunes a graph so that average profit per edge is greater than some value
select_opportunities <- function(graph, start, edgecost){
  penalizedgraph <- graph
  E(penalizedgraph)$rate <- ((100-edgecost)/100)*E(penalizedgraph)$rate
  res1 <- optimize(augmentgraph(penalizedgraph,start))
  solgraph <- augmentgraph(graph, start)
  solgraph <- delete.edges(graph=solgraph,edges=which(res1$solution==0))
  solgraph <- delete.vertices(solgraph, setdiff(V(solgraph),subcomponent(solgraph,start,mode='out')))
  solgraph <- delete.vertices(solgraph,which(V(solgraph)$name %in% c('sink','source')))
  solgraph <- augmentgraph(solgraph,start)
  E(solgraph)$solution <- optimize(solgraph)$solution
  solgraph <- delete.vertices(solgraph,which(V(solgraph)$name %in% c('sink','source')))
  return(solgraph)
}

#'describe_trades
#'
#' prints a table in a convenient form to read and execute, and plots the overall layout
describe_trades <- function(graph){
  tkplot(graph, edge.arrow.size=2)
  df <- get.data.frame(graph) %.%
    mutate(
      myaction = ifelse(type=='sell',
                        'buy',
                        'sell'),
      myaction = ifelse(is.na(type),
                        'transfer',
                        myaction),
      price_unit = ifelse(type=='sell',
                     1/rate, 
                     rate),
      amount_unit = ifelse(type=='buy',
                           solution*rate,
                           solution),
      amount_asset = ifelse(type=='sell',
                           solution*rate,
                           solution),      
      usage = solution/volume,
      amount_both = ifelse(myaction=='transfer',
                           solution, NA)
      ) %.%
    select(from, to, myaction, usage, price_unit, amount_unit, amount_asset, amount_both)
    format(df, scientific=FALSE)
}