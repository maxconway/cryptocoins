trades <- read.csv("~/git/bitcoin/trades.csv?symbol=mtgoxUSD", header=F)#maybe this should wget directly
colnames(trades)=c('time','price','amount')
## TODO:
#compare to hashrate
#look for arbitrage