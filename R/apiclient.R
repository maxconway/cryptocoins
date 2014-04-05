library(plyr)
library(dplyr)
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
#'  \item{\code{volume}} the amount available to buy, in \code{units}
#'  \item{\code{type}} buy or sell
#' }
orders2igraph <- function(orders, buyfee=0, sellfee=0, exchangename = ''){
  # note: a buy order represents and opportunity to sell, and vice versa
  buys <- plyr::summarise(orders[orders$type=='buy',],
                          from = asset,
                          to = unit,
                          rate = price*(1-sellfee),
                          volume = volume
  )
  sells <- plyr::summarise(orders[orders$type=='sell',],
                           from = unit,
                           to = asset,
                           rate = 1/price*(1-buyfee),
                           volume = volume/price
  )
  resgraph <- graph.data.frame(rbind.fill(buys,sells), directed=TRUE)
  V(resgraph)$name <- paste0(V(resgraph)$name, '_', exchangename)
  resgraph
}

validateorders <- function(orders){
  by_market <- orders %.% group_by(interaction(asset, unit))
  minsells <- by_market %.% filter(type=='sell') %.% summarise(minsell = min(price))
  maxbuys <- by_market %.% filter(type=='buy') %.% summarise(maxbuy = max(price))
  mktsummary <- inner_join(minsells, maxbuys, by='interaction(asset, unit)') %.% mutate(spread = minsell-maxbuy)
  if(any(mktsummary$spread < 0)){
    warning('minsell < maxbuy')
  }
  
  mktsummary <- mktsummary %.% mutate(midprice = rowMeans(cbind(minsell,maxbuy)))
  if(any(na.rm=TRUE,
  mktsummary[match(c('DOGE.BTC','LTC.BTC'),mktsummary$`interaction(asset, unit)`),'midprice'] > 1,
  mktsummary[match(c('BTC.DOGE','BTC.LTC'),mktsummary$`interaction(asset, unit)`),'midprice'] < 1
  )){
    warning('DOGE or LTC appears to be worth more than BTC')
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
  result <- content(GET('http://pubapi.cryptsy.com/api.php?method=orderdatav2'), as='parsed')
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
  ordersdf <- mutate(ordersdf, volume=total)
  validateorders(ordersdf)
}

getorders_bter <- function(){
  pairs <- content(GET('http://data.bter.com/api/1/pairs'))
  orders <- ldply(pairs, function(pair){
    resp <- GET(url=paste0('http://data.bter.com/api/1/depth/',pair))
    if(resp$headers[['content-type']]!='application/json'){
      Sys.sleep(10)
      resp <- GET(url=paste0('http://data.bter.com/api/1/depth/',pair))
    }
    depth <- content(resp)
    df <- rbind(
      data.frame(stringsAsFactors=FALSE,
                 type=rep('sell',length(depth$asks)),
                 ldply(depth$asks,unlist)
      ),
      data.frame(stringsAsFactors=FALSE,
                 type=rep('buy',length(depth$bids)),
                 ldply(depth$bids,unlist)
      )
    )
    df <- mutate(df,
                 price = V1,
                 volume = V2,
                 asset = toupper(str_split_fixed(pair,pattern='_',2)[1]),
                 unit = toupper(str_split_fixed(pair,pattern='_',2)[2])
    )
  })
  orders <- cleandf(orders)
  validateorders(orders)
}

