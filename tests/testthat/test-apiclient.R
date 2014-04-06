context('test api')

orders <- data.frame(
  asset  = c('A',  'A',   'B',  'B',   'C',  'C'   ),
  unit   = c('B',  'B',   'C',  'C',   'A',  'A'   ),
  type   = c('buy','sell','buy','sell','buy','sell'),
  price  = c(0.9,  1.1,   0.9,  1.1,   0.9,  1.1   ),
  volume = 1
)

test_that('orders2igraph produces expected results', {
  graph <- orders2igraph(orders)
  
  expect_that(graph['B','C',attr='rate'], equals(0.9))
  expect_that(graph['C','B',attr='rate'], equals(1/1.1))
})

test_that('orders2igraph fees work correctly', {
  graph_free <- orders2igraph(orders)
  graph_fee <- orders2igraph(orders,buyfee=0.01,sellfee=0.01)
  
  expect_that(min(E(graph_free)$rate - E(graph_fee)$rate), is_more_than(0))
})