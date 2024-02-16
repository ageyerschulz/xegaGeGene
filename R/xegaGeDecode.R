#
# (c) 2024 Andreas Geyer-Schulz
#     Grammatical Evolution V0.1
#     Layer: Gene-Level Functions 
#            For a binary gene representation.
#     Package: xegaGeGene
#

#' Map the bit strings of a binary gene to parameters in the interval
#' \code{1:2^k}.
#'
#' @description \code{xegaGeGeneMapMod()} maps the bit strings of a binary string 
#'              to integers in the interval \code{1} to 
#'              \code{lF$CodonPrecision()}. 
#'              Bit vectors are mapped into equispaced numbers in the interval.
#'
#' @details The modulo rule of grammatical evolution produces (slightly)
#'          biased choices of rules with this mapping. The bias goes to 
#'          zero as \code{lF$CodonPrecision()} \code{>>} number of rules 
#'          to choose from.
#'
#' @references Keijzer, M., O'Neill, M., Ryan, C. and Cattolico, M. (2002)
#'          Grammatical Evolution Rules: The Mod and the Bucket Rule,
#'          pp. 123-130. 
#'          In: Foster, J. A., Lutton, E., Miller, J., Ryan, C. and 
#'              Tettamanzi, A. (Eds.): Genetic Programming.
#'          Lecture Notes in Computer Science, Vol.2278, 
#'              Springer, Heidelberg. 
#'          <doi:10.1007/3-540-45984-7_12> 
#'
#' @param gene    Binary gene (the genotype).
#' @param lF      Local configuration.
#'
#' @return Integer vector.
#'
#' @family Decoder
#'
#' @examples
#' gene<-xegaGeInitGene(lFxegaGeGene)
#' xegaGeGeneMapMod(gene$gene1, lFxegaGeGene)
#'
#' @export
xegaGeGeneMapMod<-function(gene, lF)
{ nparm<-lF$Codons()
  precision<-lF$CodonPrecision()
  ub<-(-1)+2^precision
  parm<-rep(0, nparm)
  s<-1
  for (i in 1:nparm)
  { bv<-gene[s:(s-1+precision)]
	s<-s+precision
	parm[i]<-1+ub* 
        (sum(bv*2^((precision-1):0)))/ub}
return(parm) 
}

#' Map the bit strings of a binary gene to integer parameters in the interval
#' \code{1} to \code{numbers::mLCM(x) < 2^k}.
#'
#' @description \code{xegaGeGeneMapmLCM()} 
#'              maps the bit strings of a binary string 
#'              to integers in the interval \code{1} to 
#'              \code{lF$CodonPrecision()}. 
#'              Bit vectors are mapped into equispaced numbers in the interval.
#'
#' @details Using the interval of \code{1} to \code{numbers::mLCM(1:m)}
#'          provides a the least common multiple of all prime factors 
#'          of the numbers in the interval \code{1:m}.
#'          This corresponds to the bucket rule of Keijzer et al. (2002).
#'          For 16-bit precision, the highest number of rules
#'          for the same non-terminal symbols is 12.
#'          For 8-bit precision,this reduces to 6.
#'          With 64-bit integer arithmetic, the bucket rule works up to 
#'          42 rules starting with the same non-terminal.
#'
#' @references Keijzer, M., O'Neill, M., Ryan, C. and Cattolico, M. (2002)
#'          Grammatical Evolution Rules: The Mod and the Bucket Rule,
#'          pp. 123-130. 
#'          In: Foster, J. A., Lutton, E., Miller, J., Ryan, C. and 
#'              Tettamanzi, A. (Eds.): Genetic Programming.
#'          Lecture Notes in Computer Science, Vol.2278, 
#'              Springer, Heidelberg. 
#'          <doi:10.1007/3-540-45984-7_12> 
#'
#' @param gene    Binary gene (the genotype).
#' @param lF      Local configuration.
#'
#' @return Integer vector.
#'
#' @family Decoder
#'
#' @examples
#' gene<-xegaGeInitGene(lFxegaGeGene)
#' xegaGeGeneMapmLCM(gene$gene1, lFxegaGeGene)
#'
#' @export
xegaGeGeneMapmLCM<-function(gene, lF)
{ nparm<-lF$Codons()
  precision<-lF$CodonPrecision()
  diff<-lF$CodonLCM()-1
  ub<-(-1)+2^precision
  parm<-rep(0, nparm)
  s<-1
  for (i in 1:nparm)
  { bv<-gene[s:(s-1+precision)]
	s<-s+precision
	parm[i]<-1+diff* 
        (sum(bv*2^((precision-1):0)))/ub}
return(floor(parm)) 
}

#' Configure the gene map function of a genetic algorithm for grammar evolution.
#'
#' @description \code{xegaGeGeneMapFactory()} implements the selection
#'              of one of the GeneMap functions in this
#'              package by specifying a text string.
#'              The selection fails ungracefully (produces
#'              a runtime error), if the label does not match.
#'              The functions are specified locally.
#'
#'              Current support:
#'
#'              \enumerate{
#'              \item "Mod" returns \code{GeneMapMod()}. (Default).
#'              \item "Bucket" returns \code{GeneMapmLCM()}.
#'              }
#'
#' @param method    String specifying the GeneMap function.
#'
#' @return Gene map function for genes.
#'
#' @family Configuration
#'
#' @examples
#' XGene<-xegaGeGeneMapFactory("Mod")
#' gene<-xegaGeInitGene(lFxegaGeGene)
#' XGene(gene$gene1, lFxegaGeGene)
#' @export
xegaGeGeneMapFactory<-function(method="Mod") {
if (method=="Mod") {f<-xegaGeGeneMapMod}
if (method=="Bucket") {f<-xegaGeGeneMapmLCM}
if (!exists("f", inherits=FALSE))
        {stop("sge GeneMap label ", method, " does not exist")}
return(f)
}

#' Decode a gene for a context free grammar.
#'
#' @description \code{xegaGeDecodeGene()} decodes a binary gene with 
#'              a context-free grammar.
#'
#' @details The codons (k-bit sequences) of the binary gene are determining
#'          the choices of non-terminal symbols of a depth-first left-to-right
#'          tree traversal. Decoding works in 2 steps:
#'          \enumerate{
#'          \item From the binary gene and a grammar a potentially 
#'                incomplete derivation tree is built.
#'          \item The leaves of the derivation tree are extracted.
#'          }
#'
#'          It is not guaranteed that a complete derivation trees is returned.
#'
#' @param gene   Binary gene.
#' @param lF     Local configuration of the genetic algorithm.
#'
#' @return Decoded gene.
#'
#' @family Decoder
#'
#' @examples
#' lFxegaGeGene$GeneMap<-xegaGeGeneMapFactory("Mod")
#' gene<-xegaGeInitGene(lFxegaGeGene)
#' xegaGeDecodeGene(gene, lFxegaGeGene)
#'
#' @importFrom xegaDerivationTrees generateDerivationTree
#' @importFrom xegaDerivationTrees decodeDT
#' @export
xegaGeDecodeGene<-function(gene, lF)
{
  kvec<-lF$GeneMap(gene$gene1, lF)
  t<-xegaDerivationTrees::generateDerivationTree(
  sym=lF$Grammar$Start, kvec=kvec, G=lF$Grammar, maxdepth=lF$maxdepth)
  return(xegaDerivationTrees::decodeDT(t$tree, lF$Grammar$ST)) 
}


