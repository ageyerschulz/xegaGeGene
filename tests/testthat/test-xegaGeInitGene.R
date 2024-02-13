
library(testthat)
library(xegaBNF)
library(xegaDerivationTrees)
library(xegaGeGene)

test_that("InitGene OK",
          {
           g<-xegaGeInitGene(lFxegaGeGene)
           expect_identical(g$evaluated, FALSE)
           expect_identical(g$evalFail, FALSE)
           expect_equal(g$fit, 0)
           expect_equal(length(g$gene1), lFxegaGeGene$BitsOnGene())
           expect_identical(1 %in% g$gene1, TRUE)
           expect_identical(0 %in% g$gene1, TRUE)
          }
)

