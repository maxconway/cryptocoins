# See how fast market changes
samples <- list()
start <- now()
i <- 0
while(as.duration(now()-start) < dminutes(15)){
  i <- i+1
  samples[[i]] <- getcryptsyorders_df()
  Sys.sleep(30)
}

sample <- do.call(rbind,samples)

samplesubset <- ddply(sample,.(retrivaltime, from, to), summarise,
                      rate = max(rate)
                      )


gr1 <- graph.data.frame(getcryptsyorders_df())
res <- optimize(augmentgraph(gr1, 'BTC'))
gr2 <- delete.edges(graph=augmentgraph(gr1, 'BTC'),edges=which(res$solution==0))
gr2 <- delete.vertices(gr2, setdiff(V(gr2),subcomponent(gr2,'BTC',mode='out')))
plot(gr2)
res$optimum

