while(TRUE){
  try({
    orders_bter <- getorders_bter()
    graph_bter <- orders2igraph(orders_bter,exchangename='bter')
    
    orders_comkort <- getorders_comkort()
    graph_comkort <- orders2igraph(orders_comkort,exchangename='comkort')
    
    orders_cryptsy <- getorders_cryptsy()
    graph_cryptsy <- orders2igraph(orders_cryptsy,0.002,0.003,exchangename='cryptsy')
    
    gr2 <- graph_bter %du% graph_cryptsy %du% graph_comkort
    
    gr2 <- gr2 + edge('DOGE_cryptsy','DOGE_bter',rate=1,volume=100)
    gr2 <- gr2 + edge('LTC_cryptsy','LTC_bter',rate=1,volume=10)
    gr2 <- gr2 + edge('BTC_cryptsy','BTC_bter',rate=1,volume=10)
    gr2 <- gr2 + edge('DOGE_bter','DOGE_cryptsy',rate=1,volume=100)
    gr2 <- gr2 + edge('LTC_bter','LTC_cryptsy',rate=1,volume=10)
    gr2 <- gr2 + edge('BTC_bter','BTC_cryptsy',rate=1,volume=10)
    gr2 <- gr2 + edge('DOGE_bter','DOGE_comkort',rate=1,volume=100)
    gr2 <- gr2 + edge('LTC_bter','LTC_comkort',rate=1,volume=10)
    gr2 <- gr2 + edge('BTC_bter','BTC_comkort',rate=1,volume=10)
    gr2 <- gr2 + edge('DOGE_comkort','DOGE_bter',rate=1,volume=100)
    gr2 <- gr2 + edge('LTC_comkort','LTC_bter',rate=1,volume=10)
    gr2 <- gr2 + edge('BTC_comkort','BTC_bter',rate=1,volume=10)
    gr2 <- gr2 + edge('DOGE_cryptsy','DOGE_comkort',rate=1,volume=100)
    gr2 <- gr2 + edge('LTC_cryptsy','LTC_comkort',rate=1,volume=10)
    gr2 <- gr2 + edge('BTC_cryptsy','BTC_comkort',rate=1,volume=10)
    gr2 <- gr2 + edge('DOGE_comkort','DOGE_cryptsy',rate=1,volume=100)
    gr2 <- gr2 + edge('LTC_comkort','LTC_cryptsy',rate=1,volume=10)
    gr2 <- gr2 + edge('BTC_comkort','BTC_cryptsy',rate=1,volume=10)
    
    res <- optimize(augmentgraph(gr2,vertex='BTC_cryptsy'))
    
    write.table(data.frame(now(),res$optimum),
                file='./dev/logs/opportunities.txt',
                append=TRUE,
                col.names=FALSE,
                row.names=FALSE,
                sep='\t'
    )
  })
}

