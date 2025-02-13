
#
# (c) 2024 Andreas Geyer-Schulz
#          
#          Layer: Gene Layer
#          Package: xegaGeGene
#


#
# Methods for setting the codon precision in 
# grammatical evolution.
#

#' Computes the largest least common multiple of all prime factors 
#' of the integers in the interval \code{1:m} for k-bit integers.
#'  
#' @description For 64 bit numbers, numerically stable up to \code{m==42}.
#'          The modulo rule in grammatical evolution assigns to the choices 
#'          of substitutions for a non-terminal slightly (biased) probabilities.
#'          For an integer coding, the least common multiple of all rule choices 
#'          from no choice (1) to the maximal number of substitutions of a non-terminal
#'          removes this bias completely. However, whenever the prime factors of the 
#'          least common multiple contain a prime different from \code{2}, 
#'          the bias cannot be removed completely for a binary gene coding. 
#'          However, each additional bit used for coding approximately halves the bias.  
#' 
#' @details This could be done with the help of 
#'          the function mLCM of the R-package \code{numbers}. 
#'          We implement this by enumerating the vector of prime factors  
#'          in \code{1:42}.        
#'
#' @param k    Number of bits.
#' 
#' @return     A list of three elements:
#'             \itemize{
#'             \item \code{$k}: The number of bits.
#'             \item \code{$m}: Maximal number of substitutions for a non-terminal symbol
#'                              in a grammar. 
#'             \item \code{$mLCM}: Least common multiple of the prime factors of
#'                                 all rule choices from 1 to \code{$m}.
#'             }
#'
#' @family Diagnostics
#'
#' @examples
#'   tLCM(8)
#'   tLCM(16)
#'   tLCM(32)
#' @export
tLCM<-function(k)
{ pfv<-rep(1, 42)
  pfv[2]<-2; pfv[3]<-3; pfv[4]<-2; pfv[5]<-5; pfv[7]<-7; pfv[8]<-2; pfv[9]<-3
  pfv[11]<-11; pfv[13]<-13; pfv[16]<-2; pfv[17]<-17; pfv[19]<-19
  pfv[23]<-23; pfv[25]<-5; pfv[27]<-3; pfv[29]<-29
  pfv[31]<-31; pfv[32]<-2; pfv[37]<-37; pfv[41]<-41
  cpfv<-cumprod(pfv)
  if (!k %in% (1:64))
    {stop("xegaGeGene::tLCM argument k=", k, " must be an integer in 1:64")}
  b<-cpfv<2^k
  return(list(k=k, m=sum(b), mLCM=cpfv[sum(b)]))
}

#' Choice vector of a grammar.
#'
#' @param  LHS   Vector of Integers. 
#'               The left-hand side \code{G$LHS} of a grammar object \code{G}. 
#'
#' @return Vector of the number of choices for non-terminal symbols.
#'
#' @family Utility
#'
#' @examples
#' NT<-sample(5, 50, replace=TRUE)
#' ChoiceVector(NT)
#' @export
ChoiceVector<-function(LHS)
{unique(colSums(outer(LHS, unique(LHS), FUN="==")))}

#' Minimal precision of codon.
#'
#' @description The minimal precision of the codon needed for generating a working 
#'              decoder for a context-free grammar \code{G}. However, the decoder 
#'              has some choice bias which reduces the efficiency of grammar evolution. 
#'
#' @param  LHS    Vector of Integers. The left-hand side of a grammar object \code{G}. 
#' @param  ...    Unused. Needed for the common abstract interface of 
#'                precision functions.
#'
#' @return Integer.  The precision of a codon whose upper bound is the least power of 2 
#'         above the maximum number of rules for a non-terminal of a grammar.
#'
#' @family Precision
#'
#' @examples
#' NT<-sample(5, 50, replace=TRUE)
#' MinCodonPrecision(NT)
#' @export
MinCodonPrecision<-function(LHS, ...)  
{ceiling(log(max(ChoiceVector(LHS)),2))}

#' Compute the mLCM of the vector of the number of production rules
#' in a production table.
#'
#' @description Compute the least common multiple of the prime factors 
#' of the vector of the number of rules applicable for each 
#' non-terminal symbol. 
#'
#' @details For removing the bias of the modulo rule in 
#'          grammatical evolution, see Keijzer, M., O'Neill, M., 
#'          Ryan, C. and Cattolico, M. (2002). 
#'          This version works for integer genes coded in the 
#'          domain \code{1:mlCM} without bias in choosing a rule.
#'          See Keijzer et al. (2002).
#'          However, if the mLCM and \code{2^k} are relative prime, it is impossible 
#'          to find an unbiased binary coding.
#'          The choice bias is considerably lower than for \code{MinCodonPrecision()}.
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
#' @param  LHS   Vector of integers. The left-hand side of a grammar object \code{G}. 
#'
#' @return Integer. The least common multiple of the vector of the available 
#'         rules for each non-terminal.
#'
#' @family Utility
#'
#' @examples
#' library(xegaBNF)
#' g<-compileBNF(booleanGrammar())
#' mLCMG(g$PT$LHS)
#' @importFrom numbers mLCM
#' @export
mLCMG<-function(LHS)
{v<-ChoiceVector(LHS)
 if (length(v)==1) {return(v)} else {return(numbers::mLCM(v))}
}

#' mLCMG precision of codon.
#'
#' @param  LHS    Vector of Integers. The left-hand side of a grammar object \code{G}. 
#' @param  ...    Unused. Needed for the common abstract interface of 
#'                precision functions.
#'
#' @return Integer. The precision of a codon whose upper bound is larger than the
#'         least common multiple of the prime factors of the 
#'         vector of the available rules 
#'         for each non-terminal of a grammar.
#'
#' @family Precision
#'
#' @examples
#' NT<-sample(5, 50, replace=TRUE)
#' mLCMGCodonPrecision(NT)
#' @export
mLCMGCodonPrecision<-function(LHS, ...)  
{ceiling(log(mLCMG(LHS),2))}

#' Biases in Rule Choice. 
#'
#' @description Measures the biases in rule choice for each non-terminal.
#'              Statistics computed:
#'              \itemize{
#'              \item dP: Difference in probability  
#'                        between random choice with equal 
#'                        probability and modulo rule with a codon 
#'                        \code{precision}.
#'              \item dH: Difference in entropy  
#'                        between random choice with equal 
#'                        probability and modulo rule with a codon 
#'                        \code{precision}.
#'                        }
#'
#' @param cv         Choice vector of grammar.
#' @param precision  Number of bits of codon.  
#'
#' @return Data frame with the following columns
#'         \itemize{
#'         \item \code{$precision}: Number of bits.
#'         \item \code{$cv}:   i-th element of the choice vector.
#'         \item \code{$dp}:   Deviation from choice with equal probability for \code{$precision}.
#'         \item \code{$dH}:   Entropy difference between choice with equal probability 
#'                             and biased choice for \code{$precision}.
#'         }

#'        
#' @family Diagnostics
#'
#' @examples
#'  CodonChoiceBiases(c(1, 2, 3, 5), 3)
#'  CodonChoiceBiases(c(1, 2, 3, 5), 5)
#' @export
CodonChoiceBiases<-function(cv,precision)
{
 df<-data.frame()
 entropy<-function(p) {sum(-p*log(p, 2))}
  for (i in (1:length(cv)))
  {
  r<-(2^precision)/cv[i]  
  ub<-(2^precision) %% cv[i]
  lb<-cv[i]-ub
  l<-floor(r)
  h<-ceiling(r)
  d<-c(rep(l, lb), rep(h, ub))
  diff<-length(d)-max(ub, lb)
  u<-rep(1, cv[i])/cv[i]
  p<-d/sum(d)
  dp<-sum(abs(u-p))
 dH<-entropy(u)-entropy(p)
df<-rbind(df, c(precision, cv[i], dp, dH))
}
colNames<-c("Bits", "Choices", "dp", "dH")
colnames(df)<-colNames
return(df)
}

#' Biases in Rule Choice (Deprecated)
#'
#' @description See \code{CodonChoiceBiases}. 
#'              The use of the outer product leads to memory problems for 
#'              \code{precision>31} and becomes slow for \code{precision>24}. 
#'
#' @param cv         Choice vector of grammar.
#' @param precision  Number of bits of codon.  
#'
#' @return Data frame with the following columns
#'         \itemize{
#'         \item \code{$precision}: Number of bits.
#'         \item \code{$cv}:   i-th element of choice the vector.
#'         \item \code{$dp}:   Deviation from choice with equal probability for \code{$precision}.
#'         \item \code{$dH}:   Entropy difference between choice with equal probability 
#'                             and biased choice for \code{$precision}.
#'         }
#'
#' @family Diagnostics
#'
#' @examples
#'  CodonChoiceBiasesDeprecated(c(1, 2, 3, 5), 3)
#'  CodonChoiceBiasesDeprecated(c(1, 2, 3, 5), 5)
#' @export
CodonChoiceBiasesDeprecated<-function(cv,precision)
{
 df<-data.frame()
 entropy<-function(p) {sum(-p*log(p, 2))}
  for (i in (1:length(cv)))
  {
# The following code uses outer products and vector operations
  a<-(1:(2^precision)) %% cv[i]
  d<-colSums(outer(a, (0:(cv[i]-1)), FUN="=="))
  l<-min(d)
  h<-max(d)
  cl<-sum(min(d)==d)
  ch<-sum(max(d)==d)
# end of comment
  diff<-length(d)-max(cl, ch)
  u<-rep(1, cv[i])/cv[i]
  p<-d/sum(d)
  dp<-sum(abs(u-p))
 dH<-entropy(u)-entropy(p)
df<-rbind(df, c(precision, cv[i], dp, dH))
}
colNames<-c("Bits", "Choices", "dp", "dH")
colnames(df)<-colNames
return(df)
}

#' Compute codon precision with the choice bias of rules below a threshold.   
#'
#' @description For automatic determination of the least codon precision for grammar evolution 
#'              with an upper threshold on the choice bias for the substitution of all
#'              non-terminal symbols.    
#'
#' @param cv         Choice vector of a context-free grammar.
#' @param pCrit      Threshold for choice bias.
#'
#' @return Precision of codon.
#'
#' @family Diagnostics
#'
#' @examples
#'  CodonPrecision(c(1, 2, 3, 5), 0.1)
#'  CodonPrecision(c(1, 2, 3, 5), 0.01)
#' 
#' @export
CodonPrecision<-function(cv, pCrit)
{ 
  precs<-rep(0, length(cv))
  start<-ceiling(log(max(cv),2))
  for (i in (1:length(cv)))
  {
  for (precision in (start:64))
  {
  # a<-(1:2^precision) %% cv[i]
  # d<-colSums(outer(a, (0:(cv[i]-1)), FUN="=="))
  r<-(2^precision)/cv[i]  
  ub<-(2^precision) %% cv[i]
  lb<-cv[i]-ub
  l<-floor(r)
  h<-ceiling(r)
  d<-c(rep(l, lb), rep(h, ub))
  diff<-length(d)-max(ub, lb)

  u<-rep(1, cv[i])/cv[i]
  p<-d/sum(d)
  dp<-sum(abs(u-p))
  if (dp<pCrit) {precs[i]<-precision; break}
}}
return(max(precs))
}

#' Precision of a codon which has a choice bias below a probability threshold.
#'
#' @description  The choice bias is the sum of the absolute values of the 
#'               difference between a k equally probable choices and the 
#'               probability distribution of the modulo choice rule. 
#'      
#' @details For the computation of the precision, the function \code{CodonPrecision()} is used.
#'
#' @param  LHS    The left-hand side of a grammar object \code{G}. 
#' @param  pCrit  Threshold for the choice bias for a single non-terminal. 
#'
#' @return The precision of a codon which guarantees that the choice bias 
#'         for all non-terminals is below a probability threshold of
#'         \code{pCrit}.
#'
#' @family Precision
#'
#' @examples
#' NT<-sample(5, 50, replace=TRUE)
#' CodonPrecisionWithThreshold(NT, 0.1)
#' CodonPrecisionWithThreshold(NT, 0.01)
#' @export
CodonPrecisionWithThreshold<-function(LHS, pCrit)
{
CodonPrecision(ChoiceVector(LHS), pCrit)
}

#' Configure the function for computing the codon precision for grammar evolution.
#'
#' @description \code{xegaGePrecisionFactory()} implements the selection
#'              of one of the functions for computing the codon precision in this
#'              package by specifying a text string.
#'              The selection fails ungracefully (produces
#'              a runtime error), if the label does not match.
#'              The functions are specified locally.
#'
#'              Current support:
#'
#'              \enumerate{
#'              \item "Min" returns \code{MinCodonPrecision}.
#'                    Shortest coding, but some choice bias.
#'              \item "LCM" returns \code{mLCMGCodonPrecision}. (Default)
#'              \item "MaxPBias" returns \code{CodonPrecisionWithThreshold}.
#'              }
#'
#' @param method    String specifying the GeneMap function.
#'
#' @return Precision of codon function.
#'
#' @family Configuration
#'
#' @examples
#' CodonPrecision<-xegaGePrecisionFactory("Min")
#' NT<-sample(5, 50, replace=TRUE)
#' CodonPrecision(NT)
#' CodonPrecision<-xegaGePrecisionFactory("MaxPBias")
#' CodonPrecision(NT, 0.1)
#' @export
xegaGePrecisionFactory<-function(method="LCM") {
if (method=="Min") {f<-MinCodonPrecision}
if (method=="LCM") {f<-mLCMGCodonPrecision}
if (method=="MaxPBias") {f<-CodonPrecisionWithThreshold}
if (!exists("f", inherits=FALSE))
        {stop("sge Precision label ", method, " does not exist")}
return(f)
}

