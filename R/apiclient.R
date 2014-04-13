library(plyr)
library(dplyr)
library(RCurl)
library(jsonlite)
library(httr)
library(lubridate)
library(stringr)
library(igraph)

#' converts from a a data frame of orders to an igraph
#' 
#' @param orders dataframe with columns:\describe{
#'  \item{\code{asset}} first part of trading pair
#'  \item{\code{units}} second part of trading pair
#'  \item{\code{price}} the price of the \code{asset}, in \code{units}
#'  \item{\code{volume}} the amount available to buy, in \code{asset}
#'  \item{\code{type}} buy or sell
#' }
orders2igraph <- function(orders, buyfee=0, sellfee=0, exchangename = NULL){
  # note: a buy order represents and opportunity to sell, and vice versa
  buys <- dplyr::mutate(orders[orders$type=='buy',],
                        from = asset,
                        to = unit,
                        rate = price*(1-0.01*sellfee),
                        volume = volume,
                        type = 'buy'
  ) %.% select(from,to,rate,volume,type)
  sells <- dplyr::mutate(orders[orders$type=='sell',],
                         from = unit,
                         to = asset,
                         rate = (1/price)*(1-0.01*buyfee),
                         volume = volume*price,
                         type = 'sell'
  ) %.% select(from,to,rate,volume,type)
  resgraph <- graph.data.frame(rbind.fill(buys,sells), directed=TRUE)
  if(!is.null(exchangename)){
    V(resgraph)$name <- paste(V(resgraph)$name, sep='_', exchangename)
  }
  resgraph
}

igraph2orders <- function(graph){
  orders <- get.data.frame(graph)
  buys <- orders %.%
    filter(type=='buy') %.%
    mutate(asset = from,
           unit = to,
           price = rate,
           volume = volume
           ) %.%
    select(asset, unit, type, price, volume)
  sells <- orders %.%
    filter(type=='sell') %.%
    mutate(unit=from,
           asset = to,
           price = 1/rate,
           volume = volume * rate) %.% 
    select(asset, unit, type, price, volume)
  rbind.fill(buys,sells) %.% 
    arrange(unit,asset,price)
}

validateorders <- function(orders){
  by_market <- orders %.% group_by(market = interaction(asset, unit))
  minsells <- by_market %.% filter(type=='sell') %.% summarise(minsell = min(price))
  maxbuys <- by_market %.% filter(type=='buy') %.% summarise(maxbuy = max(price))
  mktsummary <- inner_join(minsells, maxbuys, by='market') %.% mutate(spread = minsell-maxbuy)
  if(sum(mktsummary$spread < 0)>2){
    stop('minsell < maxbuy')
  }else{
    orders <- orders %.% 
      mutate(market = interaction(asset, unit)) %.%
      filter(market %in% mktsummary[mktsummary$spread>=0,'market']) %.%
      select(-market)
  }
  
  mktsummary <- mktsummary %.% mutate(midprice = rowMeans(cbind(minsell,maxbuy)))
  if(any(na.rm=TRUE,
         mktsummary[match(c('DOGE.BTC','LTC.BTC'),mktsummary$`interaction(asset, unit)`),'midprice'] > 1,
         mktsummary[match(c('BTC.DOGE','BTC.LTC'),mktsummary$`interaction(asset, unit)`),'midprice'] < 1
  )){
    stop('DOGE or LTC appears to be worth more than BTC')
  }
  return(orders)
}

cleandf <- function(dataframe){
  data.frame(stringsAsFactors=FALSE, lapply(dataframe, function(column){
    if(is.factor(column)){
      column <- as.character(column)
    }
    suppressWarnings(numtry <- as.numeric(column))
    if(all(is.na(numtry)==is.na(column))){
      column <- numtry
    }
    return(column)
  }))
}

getorders_cryptsy <- function(){
  result <- fromJSON(getURL('http://pubapi.cryptsy.com/api.php?method=orderdatav2'), simplifyVector=FALSE)
  if(result$success!=1){stop('retrieval failed')}
  
  ordersdf <- ldply(result[['return']], with,
                    data.frame(stringsAsFactors=FALSE,
                               unit=rep(secondarycode, length(buyorders)+length(sellorders)),
                               asset=rep(primarycode, length(buyorders)+length(sellorders)),
                               rbind(
                                 data.frame(stringsAsFactors=FALSE,
                                            type=rep('buy',length(buyorders)),
                                            ldply(buyorders,data.frame,stringsAsFactors=FALSE)
                                 ),
                                 data.frame(stringsAsFactors=FALSE,
                                            type=rep('sell',length(sellorders)),
                                            ldply(sellorders,data.frame,stringsAsFactors=FALSE)
                                 )
                               )
                    )
  )
  
  ordersdf <- cleandf(ordersdf)
  ordersdf <- mutate(ordersdf, volume=quantity)
  validateorders(ordersdf)
}

getorders_bter <- function(){
  pairs <- fromJSON(getURL('http://data.bter.com/api/1/pairs'))
  resps_markets <- getURL(paste0('http://data.bter.com/api/1/depth/',pairs),async=FALSE)
  names(resps_markets) <- pairs
  orders <- ldply(pairs, function(pair){
    depth <- fromJSON(resps_markets[[pair]])
    mode(depth$asks) <- 'numeric'
    mode(depth$bids) <- 'numeric'
    df <- rbind(
      data.frame(stringsAsFactors=FALSE,
                 type=rep('sell',length(depth$asks)),
                 as.data.frame(depth$asks,stringsAsFactors=FALSE)
      ),
      data.frame(stringsAsFactors=FALSE,
                 type=rep('buy',length(depth$bids)),
                 as.data.frame(depth$bids,stringsAsFactors=FALSE)
      )
    )
    df$price = df$V1
    df$volume = df$V2
    df$asset = toupper(str_split_fixed(pair,pattern='_',2)[1])
    df$unit = toupper(str_split_fixed(pair,pattern='_',2)[2])
    df
  })
  orders <- cleandf(orders)
  validateorders(orders)
}

getorders_comkort <- function(){
  allorders <- fromJSON(getURL('https://api.comkort.com/v1/public/market/summary'),simplifyVector=FALSE)$markets
  orders <- ldply(allorders, function(x){
    df <- rbind_list(
      data.frame(ldply(x$sell_orders,unlist),stringsAsFactors=FALSE),
      data.frame(ldply(x$buy_orders,unlist),stringsAsFactors=FALSE)
    )
  })
  orders <- cleandf(orders)
  orders <- mutate(orders,
                   asset = item,
                   unit = price_currency,
                   volume = amount
  )
  validateorders(orders)
}

getorders_bitrex <- function(){
  mkts_raw <- content(GET('https://bittrex.com/api/v1/public/getmarkets'))
  mkts <- ldply(mkts_raw$result,data.frame,stringsAsFactors=FALSE)
  urls <- laply(mkts[mkts$IsActive,'MarketName'], function(mktname){
    modify_url(url='https://bittrex.com/api/v1/public/getorderbook',
               query=paste(sep='=',collapse='&',
                           c('type','depth','market'),
                           c('both','100',mktname)
               )
    )
  })
  names(urls) <- mkts[mkts$IsActive,'MarketName']
  orders <- ldply(mkts[mkts$IsActive,'MarketName'], function(mkt){
    depth <- fromJSON(getURL(urls[mkt]))
    stopifnot(depth$success)
    buyorders <- as.data.frame(depth$result$buy)
    sellorders <- as.data.frame(depth$result$sell)
    buyorders$type <- rep_len('buy',nrow(buyorders))
    sellorders$type <- rep_len('sell',nrow(sellorders))
    mktorders <- rbind_list(buyorders,sellorders)
    mktorders$unit <- rep_len(mkts[mkts$MarketName==mkt,'BaseCurrency'],nrow(mktorders))
    mktorders$asset <- rep_len(mkts[mkts$MarketName==mkt,'MarketCurrency'],nrow(mktorders))
    return(mktorders)
  })
  orders <- mutate(orders,
                   price = Rate,
                   volume = Quantity
  )
  orders <- cleandf(orders)
  validateorders(orders)
}

getorders_coinse <- function(){
  response <- content(GET("https://www.coins-e.com/api/v2/markets/data/"))
  if(!response$status){
    stop('retrieval failed')
  }
  markets <- response$markets
  orders <- ldply(markets, function(mkt){
    if(mkt$status!='healthy'){
      return()
    }
    data.frame(stringsAsFactors=FALSE,
               unit = mkt$c2,
               asset = mkt$c1,
               depth = rbind_list(
                 data.frame( stringsAsFactors=FALSE,
                             type=rep_len('buy',length(mkt$marketdepth$bids)),
                             ldply(mkt$marketdepth$bids, unlist)
                 ),
                 data.frame(stringsAsFactors=FALSE,
                            type=rep_len('sell',length(mkt$marketdepth$asks)),
                            ldply(mkt$marketdepth$asks, unlist))
               )
    )
  })
  orders <- cleandf(orders)
  orders <- orders[rep(1:nrow(orders), times=orders$depth.n),] %.% 
    mutate(unit = unit,
           asset = asset,
           type = depth.type,
           price = depth.r,
           volume = depth.q
    )
  validateorders(orders)
}

getorders_ccex <- function(){
  pairs <- fromJSON(getURL('https://c-cex.com/t/pairs.json'))$pairs
  ordersresps <- getURL(paste0('https://c-cex.com/t/r.html?key=1822B7C281E6742D0B903E6893CA860F&a=orderlist&pair=', pairs), async=FALSE)
  names(ordersresps) <-  pairs
  orders <- ldply(ordersresps, .id = 'market', function(mkt){
    ldply(fromJSON(mkt, simplifyDataFrame=FALSE)$return, as.data.frame,stringsAsFactors=FALSE)
  })
  orders <- cleandf(orders)
  
  orders$price <- orders$rate
  orders$market <- toupper(orders$market)
  orders$unit <- str_split_fixed(orders$market,pattern='-',2)[,2]
  orders$asset <- str_split_fixed(orders$market,pattern='-',2)[,1]
  orders$volume <- orders$amount
  
  validateorders(orders)
}