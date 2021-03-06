context('test opportunity identification')

test_that('augmentgraph works',{
  before <- graph.formula(A)
  after <- augmentgraph(before,'A')
  expect_that(vcount(after), equals(3))
})

test_that('myincmat works', {
  graph <- graph.empty()
  graph <- graph + vertices('A','B','C')
  graph <- graph + edge('A','B',rate=1.1,volume=1)
  graph <- graph + edge('B','A',rate=1.1,volume=1.1)
  graph <- graph + edge('A','C',rate=1.1,volume=1)
  graph <- graph + edge('C','A',rate=1.1,volume=1.1)
  
  graph <- augmentgraph(graph,'A')
  mat <- myincmat(graph)
  
  targetmatrix <- matrix(0,nrow=5,ncol=6)
  rownames(targetmatrix) <- V(graph)$name
  targetmatrix[c(1,2),1] <- c(-1,1.1)
  targetmatrix[c(2,1),2] <- c(-1,1.1)
  targetmatrix[c(1,3),3] <- c(-1,1.1)
  targetmatrix[c(3,1),4] <- c(-1,1.1)
  targetmatrix[c(4,1),5] <- c(-1,1)
  targetmatrix[c(1,5),6] <- c(-1,1)
    
  expect_that(mat, equals(targetmatrix))
})

test_that('if no edges, 0', {
  graph <- graph.formula(A,B)
  graph <- augmentgraph(graph,'A')
  
  res <- optimize(graph)
  
  expect_that(res$solution, equals(rep.int(0,ecount(graph))))
  expect_that(res$optimum, equals(0))
})

test_that('if only edge out, 0', {
  graph <- graph.empty()
  graph <- graph + vertices('A','B')
  graph <- graph + edge('A','B',rate=0.9,volume=1)
  
  graph <- augmentgraph(graph,'A')
  res <- optimize(graph)
  
  expect_that(res$solution, equals(rep.int(0,ecount(graph))))
  expect_that(res$optimum, equals(0))
})

test_that('if only edge in, 0', {
  graph <- graph.empty()
  graph <- graph + vertices('A','B')
  graph <- graph + edge('B','A',rate=0.9,volume=1)
  
  graph <- augmentgraph(graph,'A')
  res <- optimize(graph)
  
  expect_that(res$solution, equals(rep.int(0,ecount(graph))))
  expect_that(res$optimum, equals(0))
})

test_that('if one edge in, one out, rate less than 1, 0', {
  graph <- graph.empty()
  graph <- graph + vertices('A','B')
  graph <- graph + edge('A','B',rate=0.9,volume=1)
  graph <- graph + edge('B','A',rate=0.9,volume=0.9)
  
  graph <- augmentgraph(graph,'A')
  res <- optimize(graph)
  
  expect_that(res$solution, equals(rep.int(0,ecount(graph))))
  expect_that(res$optimum, equals(0))
})

test_that('if one edge in, one out, rate greater than 1, correct', {
  graph <- graph.empty()
  graph <- graph + vertices('A','B')
  graph <- graph + edge('A','B',rate=1.1,volume=1)
  graph <- graph + edge('B','A',rate=1.1,volume=1.1)
  
  graph <- augmentgraph(graph,'A')
  res <- optimize(graph)
  
  expect_that(res$optimum, equals(1.1^2-1))
})

test_that('depth 3 loop, correct', {
  graph <- graph.empty()
  graph <- graph + vertices('A','B','C')
  graph <- graph + edge('A','B',rate=1.1,volume=1)
  graph <- graph + edge('B','C',rate=0.9,volume=1.1)
  graph <- graph + edge('C','A',rate=1.1,volume=1.1*0.9)
  
  graph <- augmentgraph(graph,'A')
  res <- optimize(graph)
  
  expect_that(res$optimum, equals(1.1*0.9*1.1-1))
})

test_that('two depth 2 loop, correct', {
  graph <- graph.empty()
  graph <- graph + vertices('A','B','C')
  graph <- graph + edge('A','B',rate=1.1,volume=1)
  graph <- graph + edge('B','A',rate=1.1,volume=1.1)
  graph <- graph + edge('A','C',rate=1.1,volume=1)
  graph <- graph + edge('C','A',rate=1.1,volume=1.1)
  
  graph <- augmentgraph(graph,'A')
  res <- optimize(graph)
  
  expect_that(res$optimum, equals(1.1^2*2-2))
})

test_that('depth 3 loop, correct', {
  graph <- graph.empty()
  graph <- graph + vertices('A','B','C')
  graph <- graph + edge('A','B',rate=1.1,volume=1)
  graph <- graph + edge('B','A',rate=0.81,volume=2)
  graph <- graph + edge('B','C',rate=0.9,volume=1.1)
  graph <- graph + edge('C','B',rate=1,volume=2)
  graph <- graph + edge('C','A',rate=1.1,volume=1.1*0.9)
  graph <- graph + edge('A','C',rate=0.81,volume=2)
  
  graph <- augmentgraph(graph,'A')
  res <- optimize(graph)
  
  expect_that(res$optimum, equals(1.1*0.9*1.1-1))
})
