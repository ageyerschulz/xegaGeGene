
library(testthat)
library(xegaGeGene)

test_that("mLCMG OK",
{ LHSsim<-sample(20, 100, replace=TRUE)
  a<-prod(unique(colSums(outer(LHSsim, unique(LHSsim), FUN = "=="))))
  b<-mLCMG(LHSsim)
  expect_identical(b<=a, TRUE)
}
)

