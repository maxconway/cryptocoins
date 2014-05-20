#const
coinsperblock=50
costperhash=0
difficulty = system('bitcoind getdifficulty',intern=TRUE,)
difficulty=as.numeric(difficulty)
if(difficulty<=0){stop('wrong difficulty')}
costpercoin=difficulty*2^32*costperhash/coinsperblock

mtgoxUSD <- read.csv("mtgoxUSD.csv", header=F)#maybe this should wget directly
colnames(mtgoxUSD)=c('time','price','amount')

save(mtgoxUSD,file='mtgoxUSD.RData')


## TODO:
#compare to hashrate
#look for arbitrage