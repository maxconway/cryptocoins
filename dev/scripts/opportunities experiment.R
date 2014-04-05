while(TRUE){
  try({
    orders_bter <- getorders_bter()
    graph_bter <- orders2igraph(orders_bter,0.2,0.2,exchangename='bter')
    
    orders_comkort <- getorders_comkort()
    graph_comkort <- orders2igraph(orders_comkort,0.2,0.2,exchangename='comkort')
    
    orders_cryptsy <- getorders_cryptsy()
    graph_cryptsy <- orders2igraph(orders_cryptsy,0.2,0.3,exchangename='cryptsy')
    
    orders_bitrex <- getorders_bitrex()
    graph_bitrex <- orders2igraph(orders_bitrex,exchangename='bitrex')
    
    gr2 <- graph_bter %du% graph_cryptsy %du% graph_comkort %du% graph_bitrex
    
    gr2 <- gr2 + vertices(c('BTC_wallet','LTC_wallet','DOGE_wallet'))
    
    markets <- c('bter','comkort','cryptsy','bitrex')
    
    for(currency in c('BTC','LTC','DOGE')){
      gr2[from=rep_len(paste(sep='_',currency,'wallet'), length(markets)), to=paste(sep='_', currency, markets), attr='rate'] <- 1
      gr2[from=rep_len(paste(sep='_',currency,'wallet'), length(markets)), to=paste(sep='_', currency, markets), attr='volume'] <- Inf
      gr2[to=rep_len(paste(sep='_',currency,'wallet'), length(markets)), from=paste(sep='_', currency, markets), attr='rate'] <- 1
      gr2[to=rep_len(paste(sep='_',currency,'wallet'), length(markets)), from=paste(sep='_', currency, markets), attr='volume'] <- Inf
    }
    
    res <- optimize(augmentgraph(gr2,vertex='BTC_wallet'))
    
    write.table(data.frame(now(),res$optimum),
                file='./dev/logs/opportunities.txt',
                append=TRUE,
                col.names=FALSE,
                row.names=FALSE,
                sep='\t'
    )
  })
}

