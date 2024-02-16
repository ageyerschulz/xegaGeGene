
#' The \code{xegaGeGene} package 
#' provides functions implementing
#' grammatical evolution with binary coded genes:
#' 
#' \itemize{
#'     \item Gene initialization.
#'     \item Gene maps for the mod and (approximately) for the bucket rule.
#'     \item Grammar-based decoders for binary coded genes.
#'     \item Analysis of the interaction of  codon precision 
#'           with the rule choice bias for a given grammar.
#'     \item Automatic determination of codon precision 
#'           with a limited rule choice bias.
#'         }
#' 
#' @references Ryan, Conor and Collins, J. J. AND Neill, Michael O. (1998)
#'        Grammatical evolution: Evolving programs for an arbitrary language.
#'        In: 	Banzhaf, Wolfgang and Poli, Riccardo, Schoenauer, Marc and 
#'              Fogarty, Terence C. (1998):
#'       Genetic Programming. First European Workshop, EuroGP' 98 
#'       Paris, France, April 14-15, 1998 Proceedings,
#'       Lecture Notes in Computer Science, 1391, Springer, Heidelberg.
#'       <doi:10.1007/BFb0055930>
#'
#'       O'Neil, Michael AND Ryan, Conor (2003)
#'       Grammatical Evolution: 
#'       Evolutionary Automatic Programming in an Arbitrary Language.
#'       Kluwer, Dordrecht.
#'       <ISBN:1-4020-7444-1>
#'
#'       Ryan, Conor and O'Neill, Michael and Collins, J. J. (2018)
#'       Handbook of Grammatical Evolution.
#'       Springer International Publishing, Cham.
#'       <doi:10.1007/978-3-319-78717-6>      
#' 
#' @section Gene Initialization:
#'
#' The number of bits of a gene are specified by \code{lF$BitsOnGene()}.
#' 
#' The number of bits of a codon are specified 
#'                by \code{lF$CodonPrecision()}.  
#' 
#' @section Binary Gene Representation:
#'            
#' A binary gene is a named list:
#'   \itemize{
#'    \item $gene1      the gene must be a binary vector.
#'    \item $fit        the fitness value of the gene
#'                      (for EvalGeneDet and EvalGeneU) or
#'                      the mean fitness (for stochastic functions
#'                      evaluated with EvalGeneStoch).
#'    \item $evaluated  has the gene been evaluated?
#'    \item $evalFail   has the evaluation of the gene failed?
#'    \item $var        the cumulative variance of the fitness 
#'                      of all evaluations of a gene.
#'                      (For stochastic functions)
#'    \item $sigma      the standard deviation of the fitness of 
#'                      all evaluations of a gene.
#'                      (For stochastic functions)
#'    \item $obs        the number evaluations of a gene.
#'                      (For stochastic functions)
#'   }
#'
#' @section Abstract Interface of Problem Environment:
#'
#' A problem environment \code{penv} must provide:
#'   \itemize{
#'     \item \code{$f(parameters, gene, lF)}: 
#'   Function with a real parameter vector as first argument 
#'   which returns a gene 
#'   with evaluated fitness.
#'   
#'   \item $genelength(): The number of bits of the binary coded
#'                        real parameter vector. Used in \code{InitGene}.
#'     \item $bitlength(): A vector specifying the number of bits 
#'                        used for coding each real parameter.
#'                        If \code{penv$bitlength()[1]} is \code{20}, 
#'                        then \code{parameters[1]} is coded by 20 bits.
#'           Used in \code{GeneMap}.
#'     \item $lb(): The lower bound vector of each parameter.
#'           Used in \code{GeneMap}.
#'     \item $ub(): The upper bound vector of each parameter.
#'           Used in \code{GeneMap}.
#'   } 
#'
#' @section The Architecture of the xegaX-Packages:
#' 
#' The xegaX-packages are a family of R-packages which implement 
#' eXtended Evolutionary and Genetic Algorithms (xega).  
#' The architecture has 3 layers, 
#' namely the user interface layer,
#' the population layer, and the gene layer: 
#' 
#' \itemize{
#' \item
#' The user interface layer (package \code{xega}) 
#' provides a function call interface and configuration support
#' for several algorithms: genetic algorithms (sga), 
#' permutation-based genetic algorithms (sgPerm), 
#' derivation free algorithms as e.g. differential evolution (sgde), 
#' grammar-based genetic programming (sgp) and grammatical evolution
#' (sge). 
#'
#' \item
#' The population layer (package \code{xegaPopulation}) contains
#' population related functionality as well as support for 
#' population statistics dependent adaptive mechanisms and parallelization.
#'
#' \item 
#' The gene layer is split in a representation independent and 
#' a representation dependent part:
#' \enumerate{
#' \item 
#'  The representation indendent part (package \code{xegaSelectGene})
#'  is responsible for variants of selection operators, evaluation 
#'  strategies for genes, as well as profiling and timing capabilities.        
#' \item 
#'  The representation dependent part consists of the following packages: 
#' \itemize{
#' \item \code{xegaGaGene} for binary coded genetic algorithms.
#' \item \code{xegaPermGene} for permutation-based genetic algorithms.
#' \item \code{xegaDfGene} for derivation free algorithms as e.g. 
#'                         differential evolution.
#' \item \code{xegaGpGene} for grammar-based genetic algorithms.
#' \item \code{xegaGeGene} for grammatical evolution algorithms.
#' }
#' The packages \code{xegaDerivationTrees} and \code{xegaBNF} support
#' the last two packages:
#' \code{xegaBNF} essentially provides a grammar compiler and 
#' \code{xegaDerivationTrees} an abstract data type for derivation trees.
#' }} 
#'
#' @family Package Description
#'
#' @name xegaGeGene
#' @aliases xegaGeGene
#' @docType package
#' @title Package xegaGeGene.
#' @author Andreas Geyer-Schulz
#' @section Copyright: (c) 2024 Andreas Geyer-Schulz
#' @section License: MIT
#' @section URL: https://github.com/ageyerschulz/xegaGeGene
#' @section Installation: From CRAN by \code{install.packages('xegaGeGene')}
NULL

