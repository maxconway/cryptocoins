source('./R/apiclient.R')
source('./R/identify-opportunities.R')
source('./R/execute.R')

# if(file.exists('./dev/logs/ordershistory.RData')){
#   load('./dev/logs/ordershistory.RData')
# }

#while(TRUE){
  try({
    pulltime <- now()
    markets <- NULL
    orderslist <- list()
    graphs <- list()
    
    try({
      orders_bter <- getorders_bter() %.% mutate(exchange = 'bter')
      graph_bter <- orders2igraph(orders_bter,0.2,0.2,exchangename='bter')
      markets <- c(markets, 'bter')
      orderslist$bter <- orders_bter
      graphs$bter <- graph_bter
    })
    
    try({
      orders_comkort <- getorders_comkort() %.% mutate(exchange = 'comkort')
      graph_comkort <- orders2igraph(orders_comkort,0.2,0.2,exchangename='comkort')
      markets <- c(markets, 'comkort')
      orderslist$comkort <- orders_comkort
      graphs$comkort <- graph_comkort
    })
    
    try({
      orders_cryptsy <- getorders_cryptsy() %.% mutate(exchange = 'cryptsy')
      graph_cryptsy <- orders2igraph(orders_cryptsy,0.2,0.3,exchangename='cryptsy')
      markets <- c(markets, 'cryptsy')
      orderslist$cryptsy <- orders_cryptsy
      graphs$cryptsy <- graph_cryptsy
    })
    
    try({
      orders_bitrex <- getorders_bitrex() %.% mutate(exchange = 'bitrex')
      graph_bitrex <- orders2igraph(orders_bitrex,exchangename='bitrex')
      markets <- c(markets, 'bitrex')
      orderslist$bitrex <- orders_bitrex
      graphs$bitrex <- graph_bitrex
    })
    
     try({
       orders_coinse <- getorders_coinse() %.% mutate(exchange = 'coinse')
       graph_coinse <- orders2igraph(orders_coinse,0.2,0.2,exchangename='coinse')
       markets <- c(markets, 'coinse')
       orderslist$coinse <- orders_coinse
       graphs$coinse <- graph_coinse
     })
    
     try({
       orders_ccex <- getorders_ccex() %.% mutate(exchange = 'ccex')
       graph_ccex <- orders2igraph(orders_ccex,0.2,0.2,exchangename='ccex')
       markets <- c(markets, 'ccex')
       orderslist$ccex <- orders_ccex
       graphs$ccex <- graph_ccex
     })
    
#     try({
#       orders_vircurex <- getorders_vircurex() %.% mutate(exchange = 'vircurex')
#       graph_vircurex <- orders2igraph(orders_vircurex,0.2,0.2,exchangename='vircurex')
#       markets <- c(markets, 'vircurex')
#       orderslist$vircurex <- orders_vircurex
#       graphs$vircurex <- graph_vircurex
#     })
    
    gr2 <- graph.disjoint.union(graphs)
    
    gr2 <- gr2 + vertices(c('BTC_wallet','LTC_wallet','DOGE_wallet'))
    
    for(currency in c('BTC','LTC','DOGE')){
      gr2[from=rep_len(paste(sep='_',currency,'wallet'), length(markets)), to=paste(sep='_', currency, markets), attr='rate'] <- 1
      gr2[from=rep_len(paste(sep='_',currency,'wallet'), length(markets)), to=paste(sep='_', currency, markets), attr='volume'] <- Inf
      gr2[to=rep_len(paste(sep='_',currency,'wallet'), length(markets)), from=paste(sep='_', currency, markets), attr='rate'] <- 1
      gr2[to=rep_len(paste(sep='_',currency,'wallet'), length(markets)), from=paste(sep='_', currency, markets), attr='volume'] <- Inf
    }
    
    res0 <- select_opportunities(gr2,'BTC_wallet',0)
    res1 <- select_opportunities(gr2,'BTC_wallet',1)

    print(res1$optimum)
    if(res1$optimum>0.05){
      sendmail('conway.max1@gmail.com',
               subject=paste('Arbitrage opportunity for',format(res$optimum,digits=3),'BTC'),
               message=paste("Hi Max, you have found an arbitrage opportunity for",format(res$optimum,digits=3),'BTC.')
      )
    }
    
    tolog <- data.frame(pulltime = pulltime, 
                        res0 = res0$optimum,
                        res1 = res1$optimum)

    write.table(tolog,
                file='./dev/logs/opportunities.txt',
                append=TRUE,
                col.names=FALSE,
                row.names=FALSE,
                sep='\t'
    )
    
#     currentorders <- orderslist %.% ldply(select, exchange, asset, unit, type, price, volume) %.% 
#       mutate(pulled = pulltime)
    
    #     if(exists('historicalorders')){
    #       historicalorders <- rbind.fill(historicalorders, currentorders)
    #     }else{
    #       historicalorders <- currentorders
    #     }
    #     
    #     try({
    #     save(historicalorders, file='./dev/logs/ordershistory_temp.RData')
    #     file.rename('./dev/logs/ordershistory_temp.RData',
    #                 './dev/logs/ordershistory.RData')
    #     })
#    max(as.double(pulltime + dminutes(0.5) - now(),units='secs'), 0)
  })
#}

