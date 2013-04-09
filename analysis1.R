#const
coinsperblock=50
costperhash=0
difficulty = system('bitcoind getdifficulty',intern=TRUE,)
tryCatch(difficulty=as.numeric(difficult),stop('difficult should be a number'))
assert(difficult!=0)
costpercoin=difficulty*2^32*costperhash/coinsperhash

trades <- read.csv("~/git/bitcoin/trades.csv?symbol=mtgoxUSD", header=F)#maybe this should wget directly
colnames(trades)=c('time','price','amount')


## TODO:
#compare to hashrate
#look for arbitrage