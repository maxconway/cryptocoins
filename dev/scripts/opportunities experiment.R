if(file.exists('./dev/logs/ordershistory.RData')){
  load('./dev/logs/ordershistory.RData')
}

while(TRUE){
  try({
    pulltime <- now()
#     orders_bter <- getorders_bter()
#     graph_bter <- orders2igraph(orders_bter,0.2,0.2,exchangename='bter')
    
    orders_comkort <- getorders_comkort()
    graph_comkort <- orders2igraph(orders_comkort,0.2,0.2,exchangename='comkort')
    
    orders_cryptsy <- getorders_cryptsy()
    graph_cryptsy <- orders2igraph(orders_cryptsy,0.2,0.3,exchangename='cryptsy')
    
    orders_bitrex <- getorders_bitrex()
    graph_bitrex <- orders2igraph(orders_bitrex,exchangename='bitrex')
    
    orders_coinse <- getorders_coinse()
    graph_coinse <- orders2igraph(orders_coinse,0.2,0.2,exchangename='coinse')
    
    gr2 <- graph_cryptsy %du% graph_comkort %du% graph_bitrex %du% graph_coinse #%du% graph_bter
    
    gr2 <- gr2 + vertices(c('BTC_wallet','LTC_wallet','DOGE_wallet'))
    
    markets <- c('comkort','cryptsy','bitrex','coinse')#,'bter')
    
    for(currency in c('BTC','LTC','DOGE')){
      gr2[from=rep_len(paste(sep='_',currency,'wallet'), length(markets)), to=paste(sep='_', currency, markets), attr='rate'] <- 1
      gr2[from=rep_len(paste(sep='_',currency,'wallet'), length(markets)), to=paste(sep='_', currency, markets), attr='volume'] <- Inf
      gr2[to=rep_len(paste(sep='_',currency,'wallet'), length(markets)), from=paste(sep='_', currency, markets), attr='rate'] <- 1
      gr2[to=rep_len(paste(sep='_',currency,'wallet'), length(markets)), from=paste(sep='_', currency, markets), attr='volume'] <- Inf
    }
    
    res <- optimize(augmentgraph(gr2,vertex='BTC_wallet'))
    if(res$optimum>1){
      sendmail('conway.max1@gmail.com',
               subject=paste('Arbitrage opportunity for',format(res$optimum,digits=3),'BTC'),
               message=paste("Hi Max, you have found an arbitrage opportunity for",format(res$optimum,digits=3),'BTC.')
               )
    }
    
    write.table(data.frame(pulltime,res$optimum),
                file='./dev/logs/opportunities.txt',
                append=TRUE,
                col.names=FALSE,
                row.names=FALSE,
                sep='\t'
    )
    
    currentorders <- list(#mutate(orders_bter, exchange = 'bter'),
                      mutate(orders_comkort, exchange = 'comkort'),
                      mutate(orders_cryptsy, exchange = 'cryptsy'),
                      mutate(orders_bitrex, exchange = 'bittrex'),
                      mutate(orders_coinse, exchange = 'coinse')
    ) %.% ldply(select, exchange, asset, unit, type, price, volume) %.% 
      mutate(pulled = pulltime)
    
    if(exists('historicalorders')){
      historicalorders <- rbind.fill(historicalorders, currentorders)
    }else{
      historicalorders <- currentorders
    }
    
    save(historicalorders, file='./dev/logs/ordershistory_temp.RData')
    file.rename('./dev/logs/ordershistory_temp.RData',
                './dev/logs/ordershistory.RData')
  })
}

