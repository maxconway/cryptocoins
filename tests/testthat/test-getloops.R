context('test getloops')

test_that('if no edges, NULL', {
  graph <- graph.formula(A,B)
  
  res <- getloops(graph,'A',list(node='A',path=NULL,profit=1),5)
  expect_that(res, is_null())
})

test_that('if only edge out, NULL', {
  graph <- graph.formula(A-+B)
  graph['A','B',attr='rate'] <- 0.9
  expect_that(getloops(graph,'A',list(node='A',path=NULL,profit=1),5), is_null())
})

test_that('if only edge in, NULL', {
  graph <- graph.formula(A+-B)
  graph['B','A',attr='rate'] <- 0.9
  expect_that(getloops(graph,'A',list(node='A',path=NULL,profit=1),5), is_null())
})

test_that('if one edge in, one out, rate less than 1, NULL', {
  graph <- graph.formula(A+-+B)
  graph['A','B',attr='rate'] <- 0.9
  graph['B','A',attr='rate'] <- 0.9
  expect_that(getloops(graph,'A',list(node='A',path=NULL,profit=1),5), is_null())
})

test_that('if one edge in, one out, rate greater than 1, correct', {
  graph <- graph.formula(A+-+B)
  graph['A','B',attr='rate'] <- 1.1
  graph['B','A',attr='rate'] <- 1.1
  
  res <- getloops(graph,'A',list(node='A',path=NULL,profit=1),5)
  
  expect_that(length(res), equals(1))
  expect_that(res[[1]]$profit, equals(1.1^2))
  expect_that(res[[1]]$path, equals(c(1,2)))
})

test_that('depth 3 loop, correct', {
  graph <- graph.formula(A-+B-+C-+A)
  graph['A','B',attr='rate'] <- 1.1
  graph['B','C',attr='rate'] <- 0.9
  graph['C','A',attr='rate'] <- 1.1
  
  res <- getloops(graph,'A',list(node='A',path=NULL,profit=1),5)
  
  expect_that(length(res), equals(1))
  expect_that(res[[1]]$profit, equals(1.1*0.9*1.1))
})

test_that('two depth 2 loop, correct', {
  graph <- graph.formula(A+-+B:C)
  graph['A','B',attr='rate'] <- 1.1
  graph['B','A',attr='rate'] <- 1.1
  graph['A','C',attr='rate'] <- 1.1
  graph['C','A',attr='rate'] <- 1.1
    
  res <- getloops(graph,'A',list(node='A',path=NULL,profit=1),5)
  
  expect_that(length(res), equals(2))
  expect_that(res[[1]]$profit, equals(1.1*1.1))
  expect_that(res[[2]]$profit, equals(1.1*1.1))
})

test_that('depth 3 loop, correct', {
  graph <- graph.formula(A+-+B+-+C+-+A)
  graph['A','B',attr='rate'] <- 1.1
  graph['B','A',attr='rate'] <- 0.81
  graph['B','C',attr='rate'] <- 0.9
  graph['C','B',attr='rate'] <- 1
  graph['C','A',attr='rate'] <- 1.1
  graph['A','C',attr='rate'] <- 0.81
  
  res <- getloops(graph,'A',list(node='A',path=NULL,profit=1),5)
  
  expect_that(length(res), equals(1))
  expect_that(res[[1]]$profit, equals(1.1*0.9*1.1))
})

