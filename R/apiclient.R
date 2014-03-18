library(plyr)
library(dplyr)
library(httr)

getcryptsyorders_df <- function(){
  result <- content(GET('http://pubapi.cryptsy.com/api.php?method=orderdatav2'), as='parsed')
  if(result$success!=1){stop('retrieval failed')}
  
  cryptsydf <- ldply(result[['return']], function(market){
    rbind(
      ldply(market$buyorders, function(x){data.frame(stringsAsFactors=FALSE,
        from = market$secondarycode,
        to = market$primarycode,
        rate = as.numeric(x$price),
        volume = as.numeric(x$total)
      )}),
      ldply(market$sellorders, function(x){data.frame(stringsAsFactors=FALSE,
        from = market$primarycode,
        to = market$secondarycode,
        rate = 1/as.numeric(x$price),
        volume = as.numeric(x$quantity)
      )})
    )
  })[,-1]
}