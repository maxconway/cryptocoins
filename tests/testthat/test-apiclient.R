context('orders2igraph')
########################

orders_simple <- data.frame(
  asset  = c('A',  'A',   'B',  'B',   'C',  'C'   ),
  unit   = c('B',  'B',   'C',  'C',   'A',  'A'   ),
  type   = c('buy','sell','buy','sell','buy','sell'),
  price  = c(0.9,  1.1,   0.9,  1.1,   0.9,  1.1   ),
  volume = 100
)

test_that('orders2igraph: handles rates correctly', {
  graph <- orders2igraph(orders_simple)
  
  expect_that(graph['B','C',attr='rate'], equals(0.9))
  expect_that(graph['C','B',attr='rate'], equals(1/1.1))
})

test_that('orders2igraph: fees work correctly', {
  graph_free <- orders2igraph(orders_simple)
  graph_fee <- orders2igraph(orders_simple,buyfee=1,sellfee=1)
  
  expect_that(min(E(graph_free)$rate - E(graph_fee)$rate), is_more_than(0))
})

test_that('igraph2orders reverses orders2igraph', {
  load(file='tests/testthat/exampleorders.RData')
  expected <- testorders
  expected <- expected[order(expected$unit,expected$asset,expected$type,expected$price,expected$volume),c('asset','unit','type','price','volume')]
  sample <- igraph2orders(orders2igraph(testorders))
  sample <- sample[order(sample$unit,sample$asset,sample$type,sample$price,sample$volume),c('asset','unit','type','price','volume')]
  
  expect_that(nrow(sample), equals(nrow(expected)))
  
  expect_that(sample$asset, equals(expected$asset))
  expect_that(sample$unit, equals(expected$unit))
  expect_that(sample$type, equals(expected$type))
  expect_that(sample$price, equals(expected$price))
  expect_that(sample$volume, equals(expected$volume))
})


context('validation')
#####################

orders_realistic <- data.frame(
  asset  = c('LTC', 'LTC', 'DOGE', 'DOGE', 'DOGE', 'DOGE'),
  unit   = c('BTC', 'BTC', 'LTC', 'LTC', 'BTC', 'BTC'),
  type   = c('buy','sell','buy','sell','buy','sell'),
  price  = c(0.02531928, 0.02534930, 0.00000101, 0.00000102, 0.00004050, 0.00004051),
  volume = c(18.65970323, 13.11048123, 0.02265174, 0.96622129, 35.01377276, 0.20844736)
)

test_that('validation: true positive', {  
  expect_that(validateorders(orders_realistic), is_identical_to(orders_realistic))
})

test_that('validation: inverted prices', {
  orders_wrongprice1 <- mutate(orders_realistic, price = 1/price)
  expect_that(validateorders(orders_wrongprice1), throws_error())
})

test_that('validation: inverted types', {
  orders_wrongprice2 <- mutate(orders_realistic,
                               type = c('sell','buy','sell','buy','sell','buy')
  )
  expect_that(validateorders(orders_wrongprice2), throws_error())
})

test_that('validation: main values in correct ratio', {
  orders_wrong <- orders_realistic
  orders_wrong[3,'price'] <- 2
  
  expect_that(validateorders(orders_wrong), throws_error())
})