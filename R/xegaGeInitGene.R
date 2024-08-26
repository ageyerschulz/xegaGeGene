#
# (c) 2024 Andreas Geyer-Schulz
#     Grammatical Evolution in R. V0.1
#     Layer: Gene-Level Functions 
#            For a binary gene representation.
#     Package: xegaGeGene
#

#' Initialize a binary gene
#'
#' @description \code{xegaGeInitGene()} generates a random binary gene 
#'              with a given length.
#'
#' @details In the binary representation of 
#'          package \code{xegaGeGene}, a \emph{gene} is a list with 
#'          \enumerate{
#'          \item \code{$evaluated} Boolean: TRUE if the fitness is known.
#'          \item \code{$fit}       The fitness of the genotype of 
#'                                  \code{$gene1}         
#'          \item \code{$gene1}     a bit string (the genetopye).
#'          }
#'
#' @param lF   the local configuration of the genetic algorithm
#'
#' @return A binary gene (a named list):
#'         \itemize{
#'         \item \code{$evaluated}: FALSE. See package \code{xegaSelectGene}
#'         \item \code{$evalFail}:  FALSE. Set by the error handler(s)
#'                                  in package \code{xegaSelectGene} 
#'                                  in the case of failure.
#'         \item \code{$fit}:       Fitness vector.
#'         \item \code{$gene1}:     Binary gene.
#'         }
#'
#' @family Gene Generation
#'
#' @examples
#' xegaGeInitGene(lFxegaGeGene)
#'
#' @export
xegaGeInitGene<-function(lF)
{gene1<-sample(0:1, lF$BitsOnGene(), replace=TRUE)
return(list(evaluated=FALSE, evalFail=FALSE, fit=0, gene1=gene1))
}

